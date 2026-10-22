package grit.core.files

import grit.prose.form.Doc

/** What a reader made of a file: the document, its spans noted where they lie; what of the file
  * it covers; what it saw and did not read; the format and version that made it.
  */
final case class Understood(
    doc: Doc[Locus],
    covers: Covers,
    missed: Vector[Missed],
    by: FormatName,
    version: Int
)

/** Where in its file a span lies. */
enum Locus extends caps.Pure derives CanEqual {

  /** On page `number`, counted from 1. */
  case Page(number: Int)
}

/** How much of its file a read covers. */
enum Covers derives CanEqual {

  /** All of it. */
  case Whole

  /** The first `read` of its `of` pages or sheets, stopped by `by`. */
  case Part(read: Int, of: Int, unit: CoverUnit, by: Limit)

  /** Its start, up to where `by` stopped it: a file not counted in pages or sheets. */
  case Start(by: Limit)

  /** In words the model reads, `None` for [[Whole]]: "only its first 312 of 492 pages were
    * read: it is too long to read whole here".
    */
  def words: Option[String] = this match {
    case Whole => None
    case Part(read, of, unit, by) =>
      Some(s"only its first $read of $of ${unit.words(of)} were read: ${by.words}")
    case Start(by) => Some(s"only its start was read: ${by.words}")
  }
}

/** What a file is counted in. */
enum CoverUnit derives CanEqual {
  case Pages, Sheets

  /** Its plural, or its singular for `n` of 1. */
  def words(n: Int): String = (this, n) match {
    case (Pages, 1) => "page"
    case (Pages, _) => "pages"
    case (Sheets, 1) => "sheet"
    case (Sheets, _) => "sheets"
  }
}

/** Something a reader saw and did not read, in words a person reads ("comments", "a chart on
  * sheet 'Q3'").
  */
final case class Missed(what: String)

/** Why a reader read nothing. [[message]] is what the model reads, naming no setting. */
enum Unread derives CanEqual {

  /** Its bytes are not the format's: the next format is tried. */
  case NotMine

  /** No format here reads its kind. */
  case Unsupported(kind: Kind)

  /** It needs a password to open. */
  case Locked

  /** A PDF of `pages` pages with no text in it: pictures of pages, as a scan is. */
  case Scanned(pages: Int)

  /** It is damaged, or not what its bytes began by saying. */
  case Damaged(kind: Kind)

  /** Nothing was read of it in [[ReadLimits.time]]. */
  case TooSlow

  /** It is `bytes` long, over the `max` read here. */
  case TooLarge(bytes: Long, max: Long)

  def message: String = this match {
    case NotMine => "That file isn't in a form this reader knows."
    case Unsupported(Kind.OldOffice) =>
      "That file is an older Office file (.doc, .xls or .ppt), or one locked with a password, " +
        "so it can't be read here. A .docx or .xlsx copy, saved without a password, could be."
    case Unsupported(Kind.ExcelBinary) =>
      "Excel binary workbooks (.xlsb) can't be read here; an .xlsx copy could be."
    case Unsupported(kind) => s"That file is ${kind.words}, which can't be read here."
    case Locked =>
      "That file is locked with a password, so it can't be read here. An unlocked copy could be."
    case Scanned(_) =>
      "That PDF has no text in it, only pictures of its pages, so it may be a scan. A copy " +
        "with text could be read: for example one saved again from the program that made it, " +
        "or one run through text recognition."
    case Damaged(kind) =>
      s"That file looks damaged, or isn't really ${kind.words}, so it couldn't be read. " +
        "Opening it and saving it again may fix it."
    case TooSlow => "That file is too complicated to read here in reasonable time."
    case TooLarge(bytes, max) =>
      s"That file is too big to read here (${Unread.mb(bytes)}; files up to ${Unread.mb(max)} " +
        "can be read)."
  }
}

object Unread {

  /** `bytes` in whole megabytes of 2^20, at least 1. */
  private def mb(bytes: Long): String = s"${(bytes + (1L << 20) - 1) / (1L << 20)} MB"
}
