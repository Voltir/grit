package grit.mcp.drive

import java.io.IOException
import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.nio.charset.StandardCharsets.UTF_8

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*

import grit.core.clock.Clock
import grit.mcp.client.Authorizer
import grit.mcp.wire.McpError

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

  /** The file is of a type grit does not read, `kind` in words ("a PDF"). */
  case Unreadable(kind: String)

  /** What the model reads, and may say: what happened and what a person can do, in plain
    * words naming no setting and no name of the deployment's own.
    */
  /** What the operator's log is told of it beside [[message]]: its cause; `None` for an answer
    * a person's own ask explains (a file not shared, or of a type not read).
    */
  def logged: Option[String] = this match {
    case NoToken(why) => Some(s"Drive gave no access: $why")
    case Hidden | Unreadable(_) => None
    case Busy(status) => Some(s"Drive answered $status")
    case Unreachable(why) => Some(s"Drive could not be reached: $why")
    case Malformed(why) => Some(s"Drive's answer could not be read: $why")
  }

  def message: String = this match {
    case NoToken(_) => "Drive access isn't working just now; tell an administrator."
    case Hidden => "That file isn't shared here, or doesn't exist."
    case Busy(_) => "Drive is busy just now; try again in a minute."
    case Unreachable(why) => s"Drive could not be reached ($why); try again in a minute."
    case Malformed(_) => "Drive's answer couldn't be read; try again later."
    case Unreadable(kind) =>
      s"Only Docs, Sheets and text files in Drive can be read here, not $kind."
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

  /** What Drive says of `id`. */
  def file(id: FileId): Either[DriveError, DriveFile] =
    json(s"files/${FileId.value(id)}", Vector("fields" -> DriveJson.FileFields))
      .flatMap(a => DriveJson.file(a).left.map(DriveError.Malformed(_)))

  /** The name of each shared drive the account is a member of, by its id, of the first
    * [[DriveRest.DrivesMost]] Drive lists.
    */
  def drives(): Either[DriveError, Map[FileId, String]] =
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
    val read =
      if (file.mimeType == DriveFile.Document)
        get(s"files/$id/export", Vector("mimeType" -> "text/markdown"))
      else if (file.mimeType == DriveFile.Spreadsheet)
        get(s"files/$id/export", Vector("mimeType" -> "text/csv"))
      else if (file.mimeType.startsWith("text/")) get(s"files/$id", Vector("alt" -> "media"))
      else Left(DriveError.Unreadable(file.kind))
    read.map(_.stripPrefix("\uFEFF"))
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
    authorizer
      .bearer(clock)
      .left
      .map {
        case McpError.NoToken(why) => DriveError.NoToken(why)
        case other => DriveError.NoToken(other.message)
      }
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
          val body =
            try {
              val bytes = in.readNBytes(DriveRest.MaxRead)
              val text = new String(bytes, UTF_8)
              // A body cut at MaxRead ends partway through a line: keep its whole lines.
              if (bytes.length < DriveRest.MaxRead) text else text.take(text.lastIndexOf('\n') + 1)
            } finally in.close()
          DriveRest.answered(response.statusCode, body)
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

  /** The most shared drives named: 100, one page. */
  val DrivesMost: Int = 100

  /** The most grants read of a file: 100, one page; a file shared more widely is read as not. */
  val GrantsMost: Int = 100

  /** The most bytes of an answer read: 1 MiB, far past what the model is shown. */
  val MaxRead: Int = 1024 * 1024

  /** The 403 reasons Drive gives for a rate or quota limit. */
  private val Limits: Set[String] =
    Set("userRateLimitExceeded", "rateLimitExceeded", "dailyLimitExceeded", "quotaExceeded")

  /** `status` and `body` as a request's answer. */
  private def answered(status: Int, body: String): Either[DriveError, String] = {
    def reasons = scala.util.Try(ujson.read(body)).toOption.toVector.flatMap(DriveJson.reasons)
    status match {
      case 200 => Right(body)
      case 401 => Left(DriveError.NoToken("Drive refused grit's token"))
      case 403 if reasons.exists(Limits) => Left(DriveError.Busy(status))
      case 403 if reasons.contains("fileNotExportable") => Left(DriveError.Unreadable("this file"))
      case 403 | 404 => Left(DriveError.Hidden)
      case 429 => Left(DriveError.Busy(status))
      case s if s >= 500 => Left(DriveError.Busy(status))
      case s => Left(DriveError.Malformed(s"Drive answered $s"))
    }
  }
}
