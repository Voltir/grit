package grit.prose.index

import grit.prose.form.{Block, Doc, Renderer, Text}

/** The text a search index ranks for a doc: each block's words after its heading path
  * (headings joined by ` > `), a table row as `column: value` pairs, marks and code fences
  * dropped, one block a line.
  */
object IndexText extends Renderer[String] {

  def render(doc: Doc[Nothing]): String = {
    // The headings above the block being written, each with its level, outermost first.
    val (_, written) = doc.blocks.foldLeft((Vector.empty[(Int, String)], Vector.empty[String])) {
      case ((path, out), Block.Heading(level, text)) =>
        val here = path.takeWhile(_._1 < level.value) :+ (level.value -> words(text))
        (here, out :+ here.map(_._2).mkString(" > "))
      case ((path, out), block) => (path, out ++ lines(block))
    }
    written.filter(_.nonEmpty).mkString("\n")
  }

  /** The lines of a block that is not a top-level heading. */
  private def lines(block: Block[Nothing]): Vector[String] = block match {
    case Block.Paragraph(text) => Vector(words(text))
    case Block.Heading(_, text) => Vector(words(text))
    case Block.Bullets(items) => items.flatMap(_.blocks.flatMap(lines))
    case Block.Numbered(_, items) => items.flatMap(_.blocks.flatMap(lines))
    case Block.Quote(blocks) => blocks.flatMap(lines)
    case Block.Code(_, code) => Vector(code.map(_.trim).filter(_.nonEmpty).mkString(" "))
    case Block.Table(columns, rows) =>
      rows.map { row =>
        columns
          .zip(row)
          .map((c, cell) => (words(c.name), words(cell.printed)))
          .collect { case (name, value) if value.nonEmpty => s"$name: $value" }
          .mkString("; ")
      }
    case Block.Rule => Vector.empty
  }

  /** `text`'s characters on one line. */
  private def words(text: Text[Nothing]): String = text.plain.replace('\n', ' ').trim
}
