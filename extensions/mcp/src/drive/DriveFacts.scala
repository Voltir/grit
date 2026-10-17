package grit.mcp.drive

import java.time.Instant

/** What a Drive answer says as data, its text rendered from it (docs/extending.md). Written by
  * [[DriveFacts.written]]:
  *   - an entry, a file or folder listed: `{"id","kind","name"?,"changed"?,"within"?}`, `kind`
  *     as [[DriveKind.written]] writes it, and `"type"`, its MIME type, when that is `other`;
  *   - a shared drive: `{"id","kind":"drive","name"?}`;
  *   - an opened file: its entry's keys, then `"text"` and `"complete"`;
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
    * all of it.
    */
  case Opened(entry: DriveFacts.Entry, text: String, complete: Boolean)

  /** A listing's own: `complete` when Drive had no more and every part it gave was kept. */
  case Listing(complete: Boolean)
}

object DriveFacts {

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
    case Opened(e, text, complete) =>
      val o = entryJson(e)
      o("text") = text
      o("complete") = complete
      o
    case Listing(complete) => ujson.Obj("complete" -> complete)
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
  case Folder, Document, Spreadsheet, Pdf, Slides, Image, Text

  /** Any other, by its MIME type. */
  case Other(mimeType: String)

  /** As its facts write it: `folder`, `document`, `spreadsheet`, `pdf`, `slides`, `image`,
    * `text` or `other`.
    */
  def written: String = this match {
    case Folder => "folder"
    case Document => "document"
    case Spreadsheet => "spreadsheet"
    case Pdf => "pdf"
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
    case Slides => "Slides"
    case Image => "an image"
    case Text => "a text file"
    case Other(mimeType) => s"a file of type $mimeType"
  }
}
