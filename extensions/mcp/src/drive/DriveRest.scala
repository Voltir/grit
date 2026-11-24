package grit.mcp.drive

import java.net.URI

import grit.core.clock.Clock
import grit.core.files.{FileError, Unread}
import grit.mcp.client.Authorizer
import grit.mcp.google.{GoogleFailure, GoogleRequest, GoogleRest}

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

  /** Drive could not be reached in [[GoogleRest.Timeout]]: `why`, never the request. */
  case Unreachable(why: String)

  /** Drive's answer was not in the form it documents: `why`, for the operator's log alone. */
  case Malformed(why: String)

  /** The file is of a type grit does not read, `kind` in words ("Slides"). */
  case Unreadable(kind: String)

  /** Drive has flagged the file as possibly harmful, and will not give its bytes. */
  case Flagged

  /** Drive will not export the file: the export would pass its cap on one, 10 MB. */
  case ExportTooLarge

  /** The Slides API is not enabled for the account's project, so a deck's own answer is not
    * given: until the operator enables it, a deck is read as its PDF where that is read.
    */
  case SlidesOff

  /** The file was downloaded, or would have been, and could not be read: `unread` says why. */
  case Unread(unread: grit.core.files.Unread)

  /** The file area did not keep the file: `error` says why. */
  case Unkept(error: FileError)

  /** Whether it is the file's own, so a read of it meets it again until the file changes: the
    * file is of no type read, flagged, too large to export or to keep, or a format would not
    * read it ([[grit.core.files.Unread]]); not a failure of access, of Drive, or of this machine.
    */
  def lasting: Boolean = this match {
    case Unreadable(_) | Flagged | ExportTooLarge | Unread(_) => true
    case Unkept(FileError.TooLarge(_, _)) => true
    case NoToken(_) | Hidden | Busy(_) | Unreachable(_) | Malformed(_) | SlidesOff | Unkept(_) =>
      false
  }

  /** What the operator's log is told of it beside [[message]]: its cause; `None` for an answer
    * a person's own ask explains (a file not shared, or of a type not read).
    */
  def logged: Option[String] = this match {
    case NoToken(why) => Some(s"Drive gave no access: $why")
    case Hidden | Unreadable(_) | Flagged | ExportTooLarge | Unread(_) => None
    case SlidesOff => Some("the Slides API is not enabled for this account's project")
    case Busy(status) => Some(s"Drive answered $status")
    case Unreachable(why) => Some(s"Drive could not be reached: $why")
    case Malformed(why) => Some(s"Drive's answer could not be read: $why")
    case Unkept(FileError.Disk(why)) =>
      Some(s"a Drive file's bytes could not be kept to read: $why")
    case Unkept(FileError.Store(e)) => Some(s"a Drive file could not be kept to read: $e")
    case Unkept(FileError.Full) => Some("a Drive file was not kept: the file area is full")
    // Drive's own refusals are answered as Drive's; this is a download cut partway.
    case Unkept(FileError.Fetch(why)) => Some(s"a Drive file's bytes stopped arriving: $why")
    case Unkept(FileError.Gone(_, _) | FileError.TooLarge(_, _)) => None
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
    case SlidesOff => "Slides decks can't be read here just now; tell an administrator."
    // Names only the kind refused: which kinds are read depends on the deployment's formats.
    case Unreadable(kind) => s"${kind.capitalize} can't be read here."
    case Flagged => "Drive has flagged this file as possibly harmful, so it isn't read here."
    case ExportTooLarge =>
      "This file is too large for Drive to hand over to be read here (Drive passes on at most " +
        "10 MB of a Doc, Sheet or Slides deck). A shorter copy, or the file split in parts, could be."
    case Unread(unread) => unread.message
    case Unkept(error) => error.message
  }
}

/** Drive's v3 REST API at `api`, and the Slides API at `slides` for a deck's own answer,
  * read-only, over every drive shared with the account `authorizer` authorises, each request
  * carrying a token got at `clock`'s time and timing out after [[GoogleRest.Timeout]].
  */
final class DriveRest(api: URI, slides: URI, authorizer: Authorizer, clock: Clock) {

  private val google = new GoogleRest[DriveError](authorizer, clock, DriveRest.refined)

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
    * as CSV, a file whose type is `text/` anything as it is; its first [[GoogleRest.MaxRead]] bytes when longer.
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

  /** `body` over a stream of `file`'s bytes, open only within it; [[DriveError.Unread]] with
    * [[grit.core.files.Unread.TooLarge]] when Drive gives its size over `max`, before its bytes
    * are asked for (a file whose size Drive does not give is streamed whole: `body` bounds what
    * it reads); [[DriveError.Flagged]] when Drive will not give the bytes of a file it flagged
    * as possibly harmful; any other failure as [[text]]'s.
    */
  def media[A](file: DriveFile, max: Long)(body: java.io.InputStream^ => A): Either[DriveError, A] =
    file.size.filter(_ > max) match {
      case Some(size) => Left(DriveError.Unread(Unread.TooLarge(size, max)))
      case None => streamed(api, s"files/${FileId.value(file.id)}", Vector("alt" -> "media"))(body)
    }

  /** `body` over a stream of `file` in the form `as`, open only within it, its size not known
    * before (`body` bounds what it reads): an office file Drive exports, or, for
    * [[DriveExport.Deck]], the Slides API's answer at [[DriveRest.DeckFields]].
    * [[DriveError.SlidesOff]] when the Slides API is not enabled for the account's project (by
    * Google's reason, never a bare status: a deck not shared is [[DriveError.Hidden]]);
    * [[DriveError.ExportTooLarge]] past Drive's cap on an export, 10 MB;
    * [[DriveError.Unreadable]], before Drive is asked, when `as` is not among
    * `file.kindOf.exportsAs`; any other failure as [[text]]'s.
    */
  def exported[A](file: DriveFile, as: DriveExport)(
      body: java.io.InputStream^ => A
  ): Either[DriveError, A] = {
    val id = FileId.value(file.id)
    if (!file.kindOf.exportsAs.contains(as)) Left(DriveError.Unreadable(file.kindOf.words))
    else
      as match {
        case DriveExport.Deck =>
          streamed(
            slides,
            s"presentations/$id",
            Vector("fields" -> DriveRest.DeckFields),
            DriveError.SlidesOff
          )(body)
        case DriveExport.Word | DriveExport.Excel | DriveExport.Pdf =>
          streamed(api, s"files/$id/export", Vector("mimeType" -> as.mimeType))(body)
      }
  }

  /** `body` over the body of a GET of `path` below `base` with `query` when it answers 200;
    * otherwise its failure, as [[DriveError]] says, an API not enabled as `off`.
    */
  private def streamed[A](
      base: URI,
      path: String,
      query: Vector[(String, String)],
      off: DriveError = DriveError.Hidden
  )(
      body: java.io.InputStream^ => A
  ): Either[DriveError, A] =
    failed(google.streamed(request(base, path, query))(body), off)

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
    failed(google.json(request(api, path, query)))

  /** The body of a GET of `path` with `query`, as [[DriveError]] says. */
  private def get(path: String, query: Vector[(String, String)]): Either[DriveError, String] =
    failed(google.text(request(api, path, query)))

  private def request(base: URI, path: String, query: Vector[(String, String)]): GoogleRequest =
    // Only files' requests take supportsAllDrives; drives.list documents no such parameter.
    GoogleRequest(
      "Drive",
      base,
      path,
      if (path.startsWith("files")) query :+ ("supportsAllDrives" -> "true") else query
    )

  /** `answer`'s failure as Drive's, an API not enabled as `off`. */
  private def failed[A](
      answer: Either[GoogleFailure | DriveError, A],
      off: DriveError = DriveError.Hidden
  ): Either[DriveError, A] = answer.left.map {
    case GoogleFailure.NoToken(why) => DriveError.NoToken(why)
    case GoogleFailure.Hidden => DriveError.Hidden
    case GoogleFailure.Off => off
    case GoogleFailure.Busy(status) => DriveError.Busy(status)
    case GoogleFailure.Unreachable(why) => DriveError.Unreachable(why)
    case GoogleFailure.Malformed(why) => DriveError.Malformed(why)
    case own: DriveError => own
  }
}

object DriveRest {

  /** Drive's v3 REST API. */
  val Api: String = "https://www.googleapis.com/drive/v3/"

  /** The Slides API, which answers a deck under the scope [[ReadOnly]] asks for. */
  val Slides: String = "https://slides.googleapis.com/v1/"

  /** The fields a deck is asked for with: what the Slides reader reads, and no more. */
  val DeckFields: String =
    "presentationId,title,layouts(objectId,layoutProperties(name,displayName))," +
      "slides(objectId,slideProperties(layoutObjectId,isSkipped,notesPage(notesProperties," +
      s"pageElements($Elements))),pageElements($Elements))"

  /** The fields a slide's or a notes page's elements are asked for with, a group's children
    * among them.
    */
  private def Elements: String =
    "objectId,title,description,shape(placeholder(type),text),table(rows,columns," +
      "tableRows(tableCells(rowSpan,columnSpan,text))),elementGroup,image(sourceUrl),video(id)," +
      "sheetsChart(chartId),wordArt(renderedText),line(lineCategory)"

  /** The scope grit's token asks for: reading every file shared with the account. */
  val ReadOnly: Set[String] = Set("https://www.googleapis.com/auth/drive.readonly")

  /** The most hits a search reads: 20. */
  val PageSize: Int = 20

  /** The most files and folders a listing of one folder reads: 100, one page. */
  val ListMost: Int = 100

  /** The most shared drives named: 100, one page. */
  val DrivesMost: Int = 100

  /** The most grants read of a file: 100, one page; a file shared more widely is read as not. */
  val GrantsMost: Int = 100

  /** Drive's own reading of a refusal, before the table its APIs share. */
  private val refined: (Int, Vector[String]) -> Option[DriveError] = (status, reasons) =>
    if (status != 403) None
    else if (reasons.contains("fileNotExportable")) Some(DriveError.Unreadable("this file"))
    else if (reasons.contains("cannotDownloadAbusiveFile")) Some(DriveError.Flagged)
    else if (reasons.contains("exportSizeLimitExceeded")) Some(DriveError.ExportTooLarge)
    else None
}
