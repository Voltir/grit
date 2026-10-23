package grit.mcp.drive

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Duration

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.core.identity.Email
import grit.mcp.client.{Authorizer, Credential, KeyFiles, LiveProbe}

/** [[DriveRestContract]] against Drive itself, read-only, as [[LiveProbe]] runs it: over every
  * shared drive shared with the service account whose key file [[Key]] names, its files found
  * by listing them (a Doc, a Sheet, a text file, a folder, a drive's top file and a folder's
  * file), never named or printed.
  */
object DriveLive {

  /** The variable naming the service account's key file, as `grit serve` reads it. */
  val Key: Variable = Variable("GRIT_DRIVE_SA_KEY")

  /** The contract over the drives `env`'s [[Key]] reaches; `None` when [[Key]] is unset. `Left`,
    * saying why it cannot run: the key unread or no service account's, a listing refused, or no
    * file of a kind the contract reads shared with the account.
    */
  def contract(env: Map[String, String]): Option[Either[String, DriveRestContract]] =
    Option.when(env.contains(Variable.value(Key))) {
      val credential = Credential.serviceAccount(Key, DriveRest.ReadOnly)
      for {
        self <- Credential
          .account(credential, env, KeyFiles.Local)
          .left
          .map(_.message)
          .flatMap(_.toRight(s"${Variable.value(Key)} names no service account"))
        authorizer <- Credential.open(credential, env, KeyFiles.Local).left.map(_.message)
        folders <- listed(authorizer, s"mimeType = '${DriveFile.Folder}' and trashed = false")
        files <- listed(authorizer, s"mimeType != '${DriveFile.Folder}' and trashed = false")
        folderIds = folders.map(_.id).toSet
        docs <- listed(authorizer, s"mimeType = '${DriveFile.Document}' and trashed = false")
        sheets <- listed(authorizer, s"mimeType = '${DriveFile.Spreadsheet}' and trashed = false")
        texts <- listed(authorizer, "mimeType contains 'text/' and trashed = false")
        doc <- found(docs.headOption, "Doc")
        sheet <- found(sheets.headOption, "Sheet")
        text <- found(texts.find(_.mimeType.startsWith("text/")), "text file")
        folder <- found(folders.headOption, "folder")
        top <- found(
          files.find(f => f.within.isDefined && f.within == f.drive),
          "drive's top file"
        )
        inFolder <- found(files.find(_.within.exists(folderIds)), "folder's file")
      } yield new Live(authorizer, self, doc, sheet, text, folder, top, inFolder)
    }

  /** The files Drive lists matching `q` over every drive, at most one page of 100. */
  private def listed(authorizer: Authorizer, q: String): Either[String, Vector[DriveFile]] = {
    val query = Vector(
      "q" -> q,
      "corpora" -> "allDrives",
      "includeItemsFromAllDrives" -> "true",
      "supportsAllDrives" -> "true",
      "pageSize" -> "100",
      "fields" -> DriveJson.SearchFields
    ).map((k, v) => s"$k=${URLEncoder.encode(v, UTF_8).replace("+", "%20")}").mkString("&")
    authorizer.bearer(Clock.system()).left.map(_.message).flatMap { bearer =>
      val response = HttpClient
        .newHttpClient()
        .send(
          LiveProbe
            .bearing(HttpRequest.newBuilder(URI.create(s"${DriveRest.Api}files?$query")), bearer)
            .timeout(Duration.ofSeconds(60))
            .GET()
            .build(),
          HttpResponse.BodyHandlers.ofString()
        )
      if (response.statusCode != 200) Left(s"Drive answered a listing ${response.statusCode}")
      else
        scala.util
          .Try(ujson.read(response.body))
          .toOption
          .toRight("a listing that is not JSON")
          .flatMap(DriveJson.hits)
          .map(_.files)
    }
  }

  private def found(file: Option[DriveFile], what: String): Either[String, DriveFile] =
    file.toRight(s"no $what is shared with the account, so the contract cannot run")

  /** The contract over the files found. */
  private final class Live(
      authorizer: Authorizer,
      account: Email,
      docFile: DriveFile,
      sheetFile: DriveFile,
      text: DriveFile,
      folderOne: DriveFile,
      top: DriveFile,
      inFolder: DriveFile
  ) extends DriveRestContract {
    protected def rest: DriveRest^ =
      new DriveRest(
        URI.create(DriveRest.Api),
        URI.create(DriveRest.Slides),
        authorizer,
        Clock.system()
      )
    // Words nearly every file holds, as DriveCapture's search asks.
    protected def words: String = "a"
    protected def topFile: (FileId, FileId) = (top.id, within(top))
    protected def folderFile: (FileId, FileId) = (inFolder.id, within(inFolder))
    protected def doc: FileId = docFile.id
    protected def sheet: FileId = sheetFile.id
    protected def textFile: FileId = text.id
    protected def folder: FileId = folderOne.id
    protected def missing: FileId = Recorded.id("0000000000000000000000000")
    protected def self: Email = account

    // Found only with a parent.
    private def within(f: DriveFile): FileId = f.within.getOrElse(f.id)
  }
}
