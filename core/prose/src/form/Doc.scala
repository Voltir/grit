package grit.prose.form

/** A heading's rank, 1 (the title) to 6. */
opaque type Level = Int

object Level {

  /** `n` as a level, pulled into 1 to 6. */
  def of(n: Int): Level = math.max(1, math.min(6, n))

  extension (l: Level) {
    def value: Int = l
  }
}

/** One block of prose: the structure a reply is written in, with nothing in it about how
  * any edge draws it -- no widths, cells or colours. A terminal, Slack and a web page each
  * render the same blocks their own way ([[Renderer]]). Its spans carry notes of `A`.
  */
enum Block[+A <: caps.Pure] {

  /** Running text. */
  case Paragraph(text: Text[A])

  /** A heading, of rank `level`. */
  case Heading(level: Level, text: Text[A])

  /** An unordered list. */
  case Bullets(items: Vector[Item[A]])

  /** An ordered list, counting from `start`. */
  case Numbered(start: Int, items: Vector[Item[A]])

  /** Quoted prose, which may hold any blocks. */
  case Quote(blocks: Vector[Block[A]])

  /** A code listing, its lines exactly as written; `language` is the author's tag for
    * it, if they gave one.
    */
  case Code(language: Option[String], lines: Vector[String]) extends Block[Nothing]

  /** A table: a row of column names, then rows of cells. A row may be shorter or longer
    * than the header; an edge decides what that looks like.
    */
  case Table(header: Vector[Text[A]], rows: Vector[Vector[Text[A]]])

  /** A break between sections. */
  case Rule
}

/** One item of a list: the blocks under its marker, a nested list among them. */
final case class Item[+A <: caps.Pure](blocks: Vector[Block[A]])

/** A whole piece of prose, its spans carrying notes of `A` beside their marks: plain prose is
  * `Doc[Nothing]`, which every renderer takes.
  */
final case class Doc[+A <: caps.Pure](blocks: Vector[Block[A]]) {
  def isEmpty: Boolean = blocks.isEmpty
}

object Doc {
  val empty: Doc[Nothing] = Doc(Vector.empty)
}
