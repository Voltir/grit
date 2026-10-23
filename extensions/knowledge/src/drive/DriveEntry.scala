package grit.knowledge.drive

import java.time.Instant

import scala.util.Try

/** A file, a folder or a shared drive as a Drive answer's facts name it: its id, its kind, its
  * name when the answer's line shows it, when it last changed and the folder or shared drive it
  * lies in, each when Drive says.
  */
final case class DriveEntry(
    id: String,
    kind: DriveEntry.Kind,
    name: Option[String],
    changed: Option[Instant],
    within: Option[String]
) extends caps.Pure

object DriveEntry {

  /** What an entry is, as its facts' `kind` writes it: a folder, a Doc, a Sheet, a Slides deck,
    * a text file, a PDF, a Word document, an Excel workbook or a shared drive; any other kind, a
    * shortcut among them, is [[Other]], its written form kept.
    */
  enum Kind extends caps.Pure {
    case Folder, Document, Spreadsheet, Slides, Text, Pdf, Word, Excel, Drive
    case Other(written: String)

    /** How a card is made of a file of this kind: [[Kind.Read.Exported]] for those Drive
      * exports as text (a Doc, a Sheet, a text file; a Doc or a Sheet is read as the Word or
      * Excel file Drive exports it to instead when a deployment's format reads those);
      * [[Kind.Read.Formatted]] for those only a deployment's format reads, naming the file
      * area's kinds (a Slides deck's, the Slides API's answer, then the PDF Drive exports it
      * to); none for the rest.
      */
    def read: Option[Kind.Read] = this match {
      case Document => Some(Kind.Read.Exported(grit.core.files.Kind.Word))
      case Spreadsheet => Some(Kind.Read.Exported(grit.core.files.Kind.Excel))
      case Text => Some(Kind.Read.Exported(grit.core.files.Kind.Text))
      case Slides =>
        Some(
          Kind.Read.Formatted(grit.core.files.Kind.GoogleSlides, Vector(grit.core.files.Kind.Pdf))
        )
      case Pdf => Some(Kind.Read.Formatted(grit.core.files.Kind.Pdf, Vector.empty))
      case Word => Some(Kind.Read.Formatted(grit.core.files.Kind.Word, Vector.empty))
      case Excel => Some(Kind.Read.Formatted(grit.core.files.Kind.Excel, Vector.empty))
      case Folder | Drive | Other(_) => None
    }
  }

  object Kind {

    /** How a file's text is had ([[Kind.read]]). */
    enum Read extends caps.Pure {

      /** The file area's kind the file is read as first, which is the kind its pieces record
        * ([[grit.core.files.PiecedFile.kind]]) whatever it was read as.
        */
      def first: grit.core.files.Kind

      /** Read as the file area's kind `first` when a deployment's format reads it, else as the
        * text Drive exports it as.
        */
      case Exported(first: grit.core.files.Kind)

      /** Read as the file area's kind `first` when a deployment's format reads it, else as the
        * first of `then` one does; unread when none is.
        */
      case Formatted(first: grit.core.files.Kind, `then`: Vector[grit.core.files.Kind])
    }

    /** The kind its facts write as `written`. */
    def of(written: String): Kind = written match {
      case "folder" => Folder
      case "document" => Document
      case "spreadsheet" => Spreadsheet
      case "slides" => Slides
      case "text" => Text
      case "pdf" => Pdf
      case "word" => Word
      case "excel" => Excel
      case "drive" => Drive
      case other => Other(other)
    }
  }

  /** `facts`, the `n`th part's, as an entry, or why not, naming the key. */
  private[drive] def read(facts: ujson.Value, n: Int): Either[String, DriveEntry] = {
    val where = s"part $n"
    for {
      o <- Fields.obj(facts, where)
      id <- Fields.str(o, "id", where)
      kind <- Fields.str(o, "kind", where)
      name <- Fields.optStr(o, "name", where)
      changed <- Fields.optStr(o, "changed", where).flatMap {
        case None => Right(None)
        case Some(at) =>
          Try(Instant.parse(at)).toOption
            .map(Some(_))
            .toRight(s"$where's `changed` is no ISO-8601 instant")
      }
      within <- Fields.optStr(o, "within", where)
    } yield DriveEntry(id, Kind.of(kind), name, changed, within)
  }
}

/** Reading the keys of a Drive answer's facts, each refusal naming where and which key. */
private[drive] object Fields {

  type Obj = collection.Map[String, ujson.Value]

  def obj(v: ujson.Value, where: String): Either[String, Obj] =
    v.objOpt.toRight(s"$where states no object")

  def str(o: Obj, key: String, where: String): Either[String, String] =
    o.get(key).toRight(s"$where states no `$key`").flatMap(_.strOpt.toRight(other(key, where)))

  def optStr(o: Obj, key: String, where: String): Either[String, Option[String]] =
    o.get(key) match {
      case None => Right(None)
      case Some(v) => v.strOpt.map(Some(_)).toRight(other(key, where))
    }

  def bool(o: Obj, key: String, where: String): Either[String, Boolean] =
    o.get(key)
      .toRight(s"$where states no `$key`")
      .flatMap(_.boolOpt.toRight(s"$where's `$key` is not true or false"))

  private def other(key: String, where: String): String = s"$where's `$key` is no string"
}
