package grit.mcp.drive

import java.io.IOException
import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.nio.charset.StandardCharsets.UTF_8

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*

import grit.core.clock.Clock
import grit.core.files.Unread
import grit.mcp.client.Authorizer

/** Why a request to Drive answered nothing grit reads. */
enum DriveError derives CanEqual {

  /** No token was got, or Drive refused the one sent; `why` is for the operator's log. */
  case NoToken(why: String)

  /** Drive answered 404, or 403 for any reason but a rate limit or an export: the two are not
    * told apart, so nothing says whether a file exists that is not shared with grit.
    */
  case Hidden

  /** Drive answered 429, a 5xx, or 403 for a rate or quota limit: try again later. */
  case Busy(status: Int)

  /** Drive could not be reached in [[DriveRest.Timeout]]: `why`, never the request. */
  case Unreachable(why: String)

  /** Drive's answer was not in the form it documents: `why`, for the operator's log alone. */
  case Malformed(why: String)

  /** The file is of a type grit does not read, `kind` in words ("Slides"). */
  case Unreadable(kind: String)

  /** Drive has flagged the file as possibly harmful, and will not give its bytes. */
  case Flagged

  /** The file was downloaded, or would have been, and could not be read: `unread` says why. */
  case Unread(unread: grit.core.files.Unread)

  /** The file's bytes could not be kept here to be read: `why`, for the operator's log alone. */
  case Unkept(why: String)

  /** What the operator's log is told of it beside [[message]]: its cause; `None` for an answer
    * a person's own ask explains (a file not shared, or of a type not read).
    */
  def logged: Option[String] = this match {
    case NoToken(why) => Some(s"Drive gave no access: $why")
    case Hidden | Unreadable(_) | Flagged | Unread(_) => None
    case Busy(status) => Some(s"Drive answered $status")
    case Unreachable(why) => Some(s"Drive could not be reached: $why")
    case Malformed(why) => Some(s"Drive's answer could not be read: $why")
    case Unkept(why) => Some(s"a Drive file's bytes could not be kept to read: $why")
  }

  /** What the model reads, and may say: what happened and what a person can do, in plain
    * words naming no setting and no name of the deployment's own.
    */
  def message: String = this match {
    case NoToken(_) => "Drive access isn't working just now; tell an administrator."
    case Hidden => "That file isn't shared here, or doesn't exist."
    case Busy(_) => "Drive is busy just now; try again in a minute."
    case Unreachable(why) => s"Drive could not be reached ($why); try again in a minute."
    case Malformed(_) => "Drive's answer couldn't be read; try again later."
    case Unreadable(kind) =>
      s"Docs, Sheets, PDFs, Word and Excel files, and text files in Drive can be read here, not $kind."
    case Flagged => "Drive has flagged this file as possibly harmful, so it isn't read here."
    case Unread(unread) => unread.message
    case Unkept(_) => "That file couldn't be read here just now; try again later."
  }
}

/** Drive's v3 REST API at `api`, read-only, over every drive shared with the account
  * `authorizer` authorises, each request carrying a token got at `clock`'s time and timing out
  * after [[DriveRest.Timeout]].
  */
final class DriveRest(api: URI, authorizer: Authorizer, clock: Clock) {

  private val http = HttpClient.newBuilder().connectTimeout(DriveRest.Timeout.toJava).build()

  /** The first page, at most [[DriveRest.PageSize]], of files in every drive shared with the
    * account whose text holds `text`, within the folder `folder` when given, none in the trash.
    */
  def search(text: String, folder: Option[FileId]): Either[DriveError, Hits] = {
    val quoted = text.replace("\\", "\\\\").replace("'", "\\'")
    val q = (Vector(s"fullText contains '$quoted'", "trashed = false") ++
      folder.map(f => s"'${FileId.value(f)}' in parents")).mkString(" and ")
    json(
      "files",
      Vector(
        "q" -> q,
        "corpora" -> "allDrives",
        "includeItemsFromAllDrives" -> "true",
        "pageSize" -> DriveRest.PageSize.toString,
        "fields" -> DriveJson.SearchFields
      )
    ).flatMap(a => DriveJson.hits(a).left.map(DriveError.Malformed(_)))
  }

  /** The first [[DriveRest.ListMost]] files and folders directly in `parent`, a folder or a
    * shared drive, folders first and then by name, none in the trash; `incomplete` when Drive
    * had more.
    */
  def children(parent: FileId): Either[DriveError, Hits] =
    json(
      "files",
      Vector(
        "q" -> s"'${FileId.value(parent)}' in parents and trashed = false",
        "corpora" -> "allDrives",
        "includeItemsFromAllDrives" -> "true",
        "pageSize" -> DriveRest.ListMost.toString,
        "orderBy" -> "folder,name",
        "fields" -> DriveJson.SearchFields
      )
    ).flatMap(a => DriveJson.hits(a).left.map(DriveError.Malformed(_)))

  /** What Drive says of `id`. */
  def file(id: FileId): Either[DriveError, DriveFile] =
    json(s"files/${FileId.value(id)}", Vector("fields" -> DriveJson.FileFields))
      .flatMap(a => DriveJson.file(a).left.map(DriveError.Malformed(_)))

  /** The first [[DriveRest.DrivesMost]] shared drives the account is a member of; `incomplete`
    * when Drive had more.
    */
  def drives(): Either[DriveError, SharedDrives] =
    json(
      "drives",
      Vector("pageSize" -> DriveRest.DrivesMost.toString, "fields" -> DriveJson.DriveFields)
    ).flatMap(a => DriveJson.drives(a).left.map(DriveError.Malformed(_)))

  /** `id`'s grants, `None` past one page of [[DriveRest.GrantsMost]]. */
  def grants(id: FileId): Either[DriveError, Option[Vector[Grant]]] =
    json(
      s"files/${FileId.value(id)}/permissions",
      Vector("pageSize" -> DriveRest.GrantsMost.toString, "fields" -> DriveJson.GrantFields)
    ).flatMap(a => DriveJson.grants(a).left.map(DriveError.Malformed(_)))

  /** `file`'s text, a leading byte-order mark dropped: a Doc as markdown, a Sheet's first sheet
    * as CSV, a file whose type is `text/` anything as it is; its first [[DriveRest.MaxRead]] bytes when longer.
    * [[DriveError.Unreadable]] for any other type, a folder's included, and for a file Drive
    * will not export ("this file").
    */
  def text(file: DriveFile): Either[DriveError, String] = {
    val id = FileId.value(file.id)
    val read = file.kindOf match {
      case DriveKind.Document => get(s"files/$id/export", Vector("mimeType" -> "text/markdown"))
      case DriveKind.Spreadsheet => get(s"files/$id/export", Vector("mimeType" -> "text/csv"))
      case DriveKind.Text => get(s"files/$id", Vector("alt" -> "media"))
      case other => Left(DriveError.Unreadable(other.words))
    }
    read.map(_.stripPrefix("\uFEFF"))
  }

  /** `file`'s bytes written to `to`, their count; [[DriveError.Unread]] with
    * [[grit.core.files.Unread.TooLarge]] when Drive gives its size over `max`, before its bytes
    * are asked for, or when more than `max` arrive (`to` then holds a part of it);
    * [[DriveError.Flagged]] when Drive will not give the bytes of a file it flagged as possibly
    * harmful; [[DriveError.Unkept]] when `to` cannot be written; any other failure as
    * [[text]]'s.
    */
  def bytes(file: DriveFile, max: Long, to: java.nio.file.Path): Either[DriveError, Long] =
    file.size.filter(_ > max) match {
      case Some(size) => Left(DriveError.Unread(Unread.TooLarge(size, max)))
      case None =>
        send(s"files/${FileId.value(file.id)}", Vector("alt" -> "media")) { (status, in) =>
          if (status != 200) DriveRest.answered(status, DriveRest.text(in)).flatMap(_ => Right(0L))
          else {
            // `to`'s own failures are told apart from Drive's, which `send` catches.
            def kept[B](io: => B): Either[DriveError, B] =
              try Right(io)
              catch { case e: IOException => Left(DriveError.Unkept(e.getClass.getSimpleName)) }
            kept(java.nio.file.Files.newOutputStream(to)).flatMap { out =>
              // At most one byte past `max`, so a file too large is known without reading it all.
              var total = 0L
              var failed: Option[DriveError] = None
              var more = true
              var closed: Either[DriveError, Unit] = Right(())
              try {
                while (more && failed.isEmpty && total <= max) {
                  val chunk = in.readNBytes((max + 1 - total).min(64L * 1024).toInt)
                  failed = kept(out.write(chunk)).left.toOption
                  total += chunk.length
                  more = chunk.nonEmpty
                }
              } finally closed = kept(out.close())
              for {
                _ <- failed.toLeft(())
                _ <- closed
                n <-
                  if (total > max) Left(DriveError.Unread(Unread.TooLarge(total, max)))
                  else Right(total)
              } yield n
            }
          }
        }
    }

  /** Whether the account's token is taken: one search of one file. */
  private[drive] def reachable(): Either[DriveError, Unit] =
    json(
      "files",
      Vector(
        "corpora" -> "allDrives",
        "includeItemsFromAllDrives" -> "true",
        "pageSize" -> "1",
        "fields" -> "files(id)"
      )
    ).map(_ => ())

  private def json(path: String, query: Vector[(String, String)]): Either[DriveError, ujson.Value] =
    get(path, query).flatMap(body =>
      scala.util
        .Try(ujson.read(body))
        .toOption
        .toRight(DriveError.Malformed("an answer that is not JSON"))
    )

  /** The body of a GET of `path` with `query`, and `supportsAllDrives` for a file's, as
    * [[DriveError]] says.
    */
  private def get(path: String, query: Vector[(String, String)]): Either[DriveError, String] =
    send(path, query)((status, in) => DriveRest.answered(status, DriveRest.text(in)))

  /** A GET of `path` with `query`, and `supportsAllDrives` for a file's, its answer's status and
    * body handed to `read`; the body closed after. A failure to reach Drive, or of `read`'s own
    * I/O, as [[DriveError.Unreachable]].
    */
  private def send[A](path: String, query: Vector[(String, String)])(
      read: (Int, java.io.InputStream) => Either[DriveError, A]
  ): Either[DriveError, A] =
    authorizer
      .bearer(clock)
      .left
      .map(miss => DriveError.NoToken(miss.why))
      .flatMap { bearer =>
        // Only files' requests take supportsAllDrives; drives.list documents no such parameter.
        val all = if (path.startsWith("files")) query :+ ("supportsAllDrives" -> "true") else query
        val encoded = all
          .map((k, v) => s"$k=${URLEncoder.encode(v, UTF_8).replace("+", "%20")}")
          .mkString("&")
        val request = HttpRequest
          .newBuilder(api.resolve(s"$path?$encoded"))
          .timeout(DriveRest.Timeout.toJava)
          .header("Authorization", s"Bearer ${bearer.value}")
          .GET()
          .build()
        try {
          val response = http.send(request, HttpResponse.BodyHandlers.ofInputStream())
          val in = response.body()
          try read(response.statusCode, in)
          finally in.close()
        } catch {
          case _: HttpTimeoutException =>
            Left(DriveError.Unreachable(s"no answer within ${DriveRest.Timeout}"))
          // Its class alone: an I/O error's message may name the request.
          case e: IOException => Left(DriveError.Unreachable(e.getClass.getSimpleName))
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            Left(DriveError.Unreachable("interrupted"))
        }
      }
}

object DriveRest {

  /** Drive's v3 REST API. */
  val Api: String = "https://www.googleapis.com/drive/v3/"

  /** The scope grit's token asks for: reading every file shared with the account. */
  val ReadOnly: Set[String] = Set("https://www.googleapis.com/auth/drive.readonly")

  /** How long one request may take: 60 seconds. */
  val Timeout: FiniteDuration = 60.seconds

  /** The most hits a search reads: 20. */
  val PageSize: Int = 20

  /** The most files and folders a listing of one folder reads: 100, one page. */
  val ListMost: Int = 100

  /** The most shared drives named: 100, one page. */
  val DrivesMost: Int = 100

  /** The most grants read of a file: 100, one page; a file shared more widely is read as not. */
  val GrantsMost: Int = 100

  /** The most bytes of an answer read: 1 MiB, far past what the model is shown. */
  val MaxRead: Int = 1024 * 1024

  /** The 403 reasons Drive gives for a rate or quota limit. */
  private val Limits: Set[String] =
    Set("userRateLimitExceeded", "rateLimitExceeded", "dailyLimitExceeded", "quotaExceeded")

  /** `in`'s first [[MaxRead]] bytes as text; when cut there, its whole lines. */
  private def text(in: java.io.InputStream): String = {
    val bytes = in.readNBytes(MaxRead)
    val text = new String(bytes, UTF_8)
    // A body cut at MaxRead ends partway through a line: keep its whole lines.
    if (bytes.length < MaxRead) text else text.take(text.lastIndexOf('\n') + 1)
  }

  /** `status` and `body` as a request's answer. */
  private def answered(status: Int, body: String): Either[DriveError, String] = {
    def reasons = scala.util.Try(ujson.read(body)).toOption.toVector.flatMap(DriveJson.reasons)
    status match {
      case 200 => Right(body)
      case 401 => Left(DriveError.NoToken("Drive refused the token it was given"))
      case 403 if reasons.exists(Limits) => Left(DriveError.Busy(status))
      case 403 if reasons.contains("fileNotExportable") => Left(DriveError.Unreadable("this file"))
      case 403 if reasons.contains("cannotDownloadAbusiveFile") => Left(DriveError.Flagged)
      case 403 | 404 => Left(DriveError.Hidden)
      case 429 => Left(DriveError.Busy(status))
      case s if s >= 500 => Left(DriveError.Busy(status))
      case s => Left(DriveError.Malformed(s"Drive answered $s"))
    }
  }
}
