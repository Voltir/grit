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
  * any edge draws it -- no widths or colours. A terminal, Slack and a web page each
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

  /** A table: its columns, then rows of cells. [[Block.table]] makes each row as wide as the
    * columns; a row built here may be shorter or longer, and an edge decides what that looks
    * like.
    */
  case Table(columns: Vector[Column[A]], rows: Vector[Vector[Cell[A]]])

  /** A break between sections. */
  case Rule
}

object Block {

  /** A table, each row cut or filled with [[Cell.Empty]] to the columns' width. */
  def table[A <: caps.Pure](columns: Vector[Column[A]], rows: Vector[Vector[Cell[A]]]): Block[A] =
    Table(columns, rows.map(r => r.take(columns.length).padTo(columns.length, Cell.Empty())))
}

/** One item of a list: the blocks under its marker, a nested list among them. */
final case class Item[+A <: caps.Pure](blocks: Vector[Block[A]])

/** A whole piece of prose, its spans carrying notes of `A` beside their marks: plain prose is
  * `Doc[Nothing]`, which every renderer takes.
  */
final case class Doc[+A <: caps.Pure](blocks: Vector[Block[A]]) {
  def isEmpty: Boolean = blocks.isEmpty

  /** This doc with each note replaced by `f` of it, everything else as it was. */
  def map[B <: caps.Pure](f: A => B): Doc[B] =
    mapSpans(s => Span(s.text, s.marks, s.notes.map(f)))

  /** This doc with each span replaced by `f` of it, the blocks around it as they were. */
  def mapSpans[B <: caps.Pure](f: Span[A] => Span[B]): Doc[B] = {
    def text(t: Text[A]): Text[B] = Text(t.spans.map(f))
    def items(is: Vector[Item[A]]): Vector[Item[B]] = is.map(i => Item(i.blocks.map(block)))
    def block(b: Block[A]): Block[B] = b match {
      case Block.Paragraph(t) => Block.Paragraph(text(t))
      case Block.Heading(l, t) => Block.Heading(l, text(t))
      case Block.Bullets(is) => Block.Bullets(items(is))
      case Block.Numbered(start, is) => Block.Numbered(start, items(is))
      case Block.Quote(bs) => Block.Quote(bs.map(block))
      case c: Block.Code => c
      case Block.Table(cs, rows) =>
        Block.Table(cs.map(c => Column(text(c.name), c.kind)), rows.map(_.map(_.mapText(text))))
      case Block.Rule => Block.Rule
    }
    Doc(blocks.map(block))
  }

  /** `f` of each note in [[notes]]' order, combined from `zero` by `plus`. */
  def foldMap[M](zero: M, plus: (M, M) => M)(f: A => M): M =
    notes.foldLeft(zero)((m, a) => plus(m, f(a)))

  /** Every note in reading order: block by block, a list's items and a quote's blocks where
    * they stand, a table's column names then its rows' cells; a note on several spans once per
    * span.
    */
  def notes: Vector[A] = {
    def text(t: Text[A]): Vector[A] = t.spans.flatMap(_.notes)
    def block(b: Block[A]): Vector[A] = b match {
      case Block.Paragraph(t) => text(t)
      case Block.Heading(_, t) => text(t)
      case Block.Bullets(is) => is.flatMap(_.blocks.flatMap(block))
      case Block.Numbered(_, is) => is.flatMap(_.blocks.flatMap(block))
      case Block.Quote(bs) => bs.flatMap(block)
      case Block.Code(_, _) | Block.Rule => Vector.empty
      case Block.Table(cs, rows) =>
        cs.flatMap(c => text(c.name)) ++ rows.flatten.flatMap(c => text(c.shown))
    }
    blocks.flatMap(block)
  }
}

object Doc {
  val empty: Doc[Nothing] = Doc(Vector.empty)
}
