package grit.formats.tree

import grit.prose.form.{Block, Cell, Column, Item, Text}

/** One item of a levelled list: its depth, from 0 at the top; the number its marker shows,
  * when the list is numbered; and its text.
  */
private[formats] final case class Levelled[+A <: caps.Pure](
    depth: Int,
    number: Option[Int],
    text: Text[A]
)

/** The blocks more than one reader builds the same way from what it has read. */
private[formats] object Blocks {

  /** `items` as one list, an item followed by deeper ones holding them as a list of its own;
    * each list numbered from its first item's number when that item has one, else bulleted.
    */
  def list[A <: caps.Pure](items: List[Levelled[A]]): Block[A] = {
    val nested = nest(items)
    items.headOption.flatMap(_.number).fold(Block.Bullets(nested))(Block.Numbered(_, nested))
  }

  private def nest[A <: caps.Pure](items: List[Levelled[A]]): Vector[Item[A]] = items match {
    case Nil => Vector.empty
    case i :: rest =>
      val (children, after) = rest.span(_.depth > i.depth)
      val inner = if (children.isEmpty) Vector.empty else Vector(list(children))
      Item(Block.Paragraph(i.text) +: inner) +: nest(after)
  }

  /** `rows` as a table as wide as its widest row, the first its columns' names (each cell's
    * printed text); `None` for no rows.
    */
  def table[A <: caps.Pure](rows: Vector[Vector[Cell[A]]]): Option[Block[A]] =
    rows.headOption.map { head =>
      val width = rows.map(_.size).max
      Block.table(head.map(c => Column(c.printed)).padTo(width, Column(Text.empty)), rows.drop(1))
    }
}
