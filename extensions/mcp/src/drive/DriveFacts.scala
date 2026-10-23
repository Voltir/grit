package grit.mcp.drive

import java.time.Instant

import grit.core.id.FileHandle

/** What a Drive answer says as data, its text rendered from it (docs/extending.md). Written by
  * [[DriveFacts.written]]:
  *   - an entry, a file or folder listed: `{"id","kind","name"?,"changed"?,"within"?}`, `kind`
  *     as [[DriveKind.written]] writes it, and `"type"`, its MIME type, when that is `other`;
  *   - a shared drive: `{"id","kind":"drive","name"?}`;
  *   - an opened file: its entry's keys, then `"text"`, `"complete"`, and, for a file a format
  *     read, `"covered"`, what of the file the text covers in words ("pages 2–3 of 4"), and
  *     `"file"`, the handle the file area keeps it under (`"f41"`);
  *   - a file read and not readable: its entry's keys, then `"unread"`, why, as the model reads it;
  *   - a listing's own: `{"complete"}` alone, never counting or naming its parts.
  *
  * `name` only when [[DriveItems.shown]] admits it, as its line shows it; `changed` ISO-8601, as
  * Drive gives it; `within`, the folder or shared drive it lies in.
  */
enum DriveFacts derives CanEqual {

  /** A file or folder, as its line names it. */
  case Entry(
      id: FileId,
      kind: DriveKind,
      name: Option[String],
      changed: Option[Instant],
      within: Option[FileId]
  )

  /** A shared drive, as its line names it. */
  case Drive(id: FileId, name: Option[String])

  /** A file read: `text`, the leading whole lines of it its part shows; `complete` when that is
    * all of it; `kept`, for a file a format read.
    */
  case Opened(
      entry: DriveFacts.Entry,
      text: String,
      complete: Boolean,
      kept: Option[DriveFacts.Kept]
  )

  /** A listing's own: `complete` when Drive had no more and every part it gave was kept. */
  case Listing(complete: Boolean)

  /** A file read and not readable, for `why`, in the words the model reads ([[DriveCalled.Unread]]). */
  case Unread(entry: DriveFacts.Entry, why: String)
}

object DriveFacts {

  /** A file a format read: what of it the text covers, in words, and the handle the file area
    * keeps it under.
    */
  final case class Kept(covered: String, file: FileHandle) derives CanEqual

  /** `file` as its line names it: its name only when [[DriveItems.shown]] admits it. */
  def entry(file: DriveFile): Entry =
    new Entry(
      file.id,
      file.kindOf,
      Some(file.name).filter(DriveItems.shown),
      file.changed,
      file.within
    )

  /** `facts` as JSON, as [[DriveFacts]] says. */
  def written(facts: DriveFacts): ujson.Value = facts match {
    case e: Entry => entryJson(e)
    case Drive(id, name) =>
      val o = ujson.Obj("id" -> FileId.value(id), "kind" -> "drive")
      name.foreach(n => o("name") = n)
      o
    case Opened(e, text, complete, kept) =>
      val o = entryJson(e)
      o("text") = text
      o("complete") = complete
      kept.foreach { k =>
        o("covered") = k.covered
        o("file") = k.file.written
      }
      o
    case Listing(complete) => ujson.Obj("complete" -> complete)
    case Unread(e, why) =>
      val o = entryJson(e)
      o("unread") = why
      o
  }

  private def entryJson(e: Entry): ujson.Obj = {
    val o = ujson.Obj("id" -> FileId.value(e.id), "kind" -> e.kind.written)
    e.kind match {
      case DriveKind.Other(mimeType) => o("type") = mimeType
      case _ => ()
    }
    e.name.foreach(n => o("name") = n)
    e.changed.foreach(t => o("changed") = t.toString)
    e.within.foreach(w => o("within") = FileId.value(w))
    o
  }
}

/** A file's type, as its facts and its line say it, from its MIME type ([[DriveFile.Folder]]
  * and the rest, their one home).
  */
enum DriveKind derives CanEqual {
  case Folder, Document, Spreadsheet, Pdf, Word, Excel, Slides, Image, Text

  /** Any other, by its MIME type. */
  case Other(mimeType: String)

  /** As its facts write it: `folder`, `document`, `spreadsheet`, `pdf`, `word`, `excel`,
    * `slides`, `image`, `text` or `other`.
    */
  def written: String = this match {
    case Folder => "folder"
    case Document => "document"
    case Spreadsheet => "spreadsheet"
    case Pdf => "pdf"
    case Word => "word"
    case Excel => "excel"
    case Slides => "slides"
    case Image => "image"
    case Text => "text"
    case Other(_) => "other"
  }

  /** In words a person reads: "a Doc", "a PDF", "a folder"; "a file of type …" for one with
    * no plainer name.
    */
  def words: String = this match {
    case Folder => "a folder"
    case Document => "a Doc"
    case Spreadsheet => "a Sheet"
    case Pdf => "a PDF"
    case Word => "a Word file"
    case Excel => "an Excel file"
    case Slides => "Slides"
    case Image => "an image"
    case Text => "a text file"
    case Other(mimeType) => s"a file of type $mimeType"
  }

  /** The forms Drive gives a file of this kind in ([[DriveRest.exported]]), the first preferred:
    * a Doc as a Word document, a Sheet as an Excel workbook, every sheet in it, a Slides deck as
    * the Slides API's answer, then as a PDF; empty for a kind Drive keeps the bytes of, or does
    * not export.
    */
  def exportsAs: Vector[DriveExport] = this match {
    case Document => Vector(DriveExport.Word)
    case Spreadsheet => Vector(DriveExport.Excel)
    case Slides => Vector(DriveExport.Deck, DriveExport.Pdf)
    case Folder | Pdf | Word | Excel | Image | Text | Other(_) => Vector.empty
  }
}

/** A form Drive gives one of its own files in, by the MIME type asked for, and the kind of file
  * the file area reads it as: an office file it exports, or a deck as the Slides API answers it.
  */
enum DriveExport(val mimeType: String, val kind: grit.core.files.Kind) derives CanEqual {
  case Word extends DriveExport(DriveFile.Word, grit.core.files.Kind.Word)
  case Excel extends DriveExport(DriveFile.Excel, grit.core.files.Kind.Excel)
  case Pdf extends DriveExport(DriveFile.Pdf, grit.core.files.Kind.Pdf)
  case Deck extends DriveExport(grit.core.files.Kind.SlidesType, grit.core.files.Kind.GoogleSlides)
}
