package grit.formats.tree

import grit.prose.form.{Block, Cell, Column, Item, Text}

/** The blocks more than one reader builds the same way from what it has read. */
private[formats] object Blocks {

  /** Items at their depths as nested lists: an item followed by deeper ones holds them. */
  def nest[A <: caps.Pure](items: List[(Int, Text[A])]): Vector[Item[A]] = items match {
    case Nil => Vector.empty
    case (d, t) :: rest =>
      val (children, after) = rest.span(_._1 > d)
      val inner = if (children.isEmpty) Vector.empty else Vector(Block.Bullets(nest(children)))
      Item(Block.Paragraph(t) +: inner) +: nest(after)
  }

  /** `rows` as a table as wide as its widest row, the first its columns' names. */
  def table[A <: caps.Pure](rows: Vector[Vector[Text[A]]]): Option[Block[A]] =
    rows.headOption.map { head =>
      val width = rows.map(_.size).max
      Block.table(
        head.padTo(width, Text.empty).map(Column(_)),
        rows.drop(1).map(_.map(t => Cell.Words(t): Cell[A]))
      )
    }
}
