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
  * render the same blocks their own way ([[Renderer]]).
  */
enum Block {

  /** Running text. */
  case Paragraph(text: Text)

  /** A heading, of rank `level`. */
  case Heading(level: Level, text: Text)

  /** An unordered list. */
  case Bullets(items: Vector[Item])

  /** An ordered list, counting from `start`. */
  case Numbered(start: Int, items: Vector[Item])

  /** Quoted prose, which may hold any blocks. */
  case Quote(blocks: Vector[Block])

  /** A code listing, its lines exactly as written; `language` is the author's tag for
    * it, if they gave one.
    */
  case Code(language: Option[String], lines: Vector[String])

  /** A table: a row of column names, then rows of cells. A row may be shorter or longer
    * than the header; an edge decides what that looks like.
    */
  case Table(header: Vector[Text], rows: Vector[Vector[Text]])

  /** A break between sections. */
  case Rule
}

/** One item of a list: the blocks under its marker, a nested list among them. */
final case class Item(blocks: Vector[Block])

/** A whole piece of prose: a reply, or as much of one as has arrived. */
final case class Doc(blocks: Vector[Block]) {
  def isEmpty: Boolean = blocks.isEmpty
}

object Doc {
  val empty: Doc = Doc(Vector.empty)
}
