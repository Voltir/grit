package grit.core.files

/** What of a file to read: from the start; pages `from` to `to` (1-based, inclusive); the
  * section whose heading path ends with `heading`; from the character `offset` of its text; or
  * around where `anchor`'s passage lies now ([[Anchor.find]]).
  */
enum FileRange derives CanEqual {
  case Start
  case Pages private[files] (from: Int, to: Int)
  case Section private[files] (heading: String)
  case From private[files] (offset: Int)
  case Near private[files] (anchor: Anchor)

  /** As a reader asks for it again: `pages="212-214"`, `section="3.2 Access control"`,
    * `from=51234`, `near="<the anchor as written>"` ([[Anchor.written]]); empty for [[Start]].
    */
  def asked: String = this match {
    case Start => ""
    case Pages(a, b) => if (a == b) s"pages=\"$a\"" else s"pages=\"$a-$b\""
    case Section(h) => s"section=\"$h\""
    case From(o) => s"from=$o"
    case Near(a) => s"near=\"${a.written}\""
  }
}

object FileRange {

  /** The range a reader's arguments ask for: [[FileRange.Start]] when none is given. `Left`, in
    * words the model reads, for more than one, `pages` not a page (`"212"`) or a run of them
    * (`"212-214"`, an en dash too) counted from 1, the run's end before its start, a blank
    * `section`, a negative `from`, or a `near` not an anchor as written ([[Anchor.read]]).
    */
  def parse(
      pages: Option[String],
      section: Option[String],
      from: Option[Int],
      near: Option[String]
  ): Either[String, FileRange] =
    (pages, section, from, near) match {
      case (None, None, None, None) => Right(Start)
      case (None, None, None, Some(n)) => Anchor.read(n).map(Near(_))
      case (Some(p), None, None, None) => pagesOf(p)
      case (None, Some(s), None, None) =>
        if (s.trim.isEmpty) Left("Name the section by its heading.") else Right(Section(s.trim))
      case (None, None, Some(o), None) =>
        if (o < 0) Left("Give from as a character offset, 0 or more.") else Right(From(o))
      case _ =>
        Left("Ask for pages, a section, a starting offset or near: one of them, not several.")
    }

  /** Pages `from` to `to`, or `None` unless 1 ≤ `from` ≤ `to`. */
  def pages(from: Int, to: Int): Option[FileRange] =
    Option.when(from >= 1 && from <= to)(Pages(from, to))

  /** Reading on from character `offset`, or `None` when it is negative. */
  def from(offset: Int): Option[FileRange] = Option.when(offset >= 0)(From(offset))

  private val Run = """\s*(\d{1,6})\s*(?:[-–]\s*(\d{1,6})\s*)?""".r

  private def pagesOf(p: String): Either[String, FileRange] = {
    val refused = Left(
      s"Give pages as one page, \"212\", or a run of them, \"212-214\", not \"$p\"."
    )
    p match {
      case Run(a, b) =>
        val from = a.toInt
        pages(from, Option(b).fold(from)(_.toInt)).toRight(refused.value)
      case _ => refused
    }
  }
}

/** A read as the model is shown it: `text` (markdown, `[page n]` lines where pages begin), what
  * it covers in words ("pages 212–214 of 492"), where the next read would start, and the file's
  * outline when the read began at its start or asked for a section it lacks; `whole` when
  * `text` is all of the file and its reader read all of it.
  */
final case class FileRead(
    text: String,
    covered: String,
    next: Option[FileRange],
    outline: Option[String],
    whole: Boolean
) {

  /** What the model reads: what it covers, the outline under `Outline:`, the text, and how to
    * read on, each apart; within [[ModelText.Room]] when [[ModelText.of]] made it.
    */
  def shown: String =
    (Vector(s"Read: $covered.") ++
      outline.map(o => s"Outline:\n$o") ++
      Option.when(text.nonEmpty)(text) ++
      next.map(n => s"[To read on, ask for ${n.asked}.]")).mkString("\n\n")
}
