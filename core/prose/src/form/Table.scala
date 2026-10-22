package grit.prose.form

import java.time.{LocalDate, LocalDateTime}

/** What most cells of a table's column hold. A cell of another kind is kept as its source
  * had it (a stray word in a number column), not converted.
  */
enum CellKind {
  case Words, Number, Date, Truth
}

/** A table's column: its `name` and the kind most of its cells are. */
final case class Column[+A <: caps.Pure](name: Text[A], kind: CellKind)

/** One table cell: [[shown]], its value as its source displayed it, never reformatted, and
  * `formula`, when its source computed it.
  */
sealed trait Cell[+A <: caps.Pure] {
  def shown: Text[A]
  def formula: Option[String]

  /** What every renderer prints for this cell: [[shown]], then its formula in parentheses; a
    * formula whose source kept no value ([[Cell.Empty]]) alone, as `=F (not calculated)`.
    */
  def printed: Text[A] = (this, formula) match {
    case (_, None) => shown
    case (Cell.Empty(_), Some(f)) => Text.plain(s"$f (not calculated)")
    case (_, Some(f)) => Text.of(shown.spans :+ Span.plain(s" ($f)"))
  }

  /** This cell with its words' spans replaced by `f`; a number's, a date's or a truth's
    * shown text, which carries no notes, as it was.
    */
  private[form] def mapText[B <: caps.Pure](f: Text[A] => Text[B]): Cell[B] = this match {
    case Cell.Words(t, formula) => Cell.Words(f(t), formula)
    case c: Cell.Number => c
    case c: Cell.Date => c
    case c: Cell.Truth => c
    case c: Cell.Empty => c
  }
}

object Cell {
  final case class Words[+A <: caps.Pure](shown: Text[A], formula: Option[String] = None)
      extends Cell[A]

  final case class Number(value: BigDecimal, shown: Text[Nothing], formula: Option[String] = None)
      extends Cell[Nothing]

  final case class Date(
      value: LocalDate | LocalDateTime,
      shown: Text[Nothing],
      formula: Option[String] = None
  ) extends Cell[Nothing]

  /** A truth value, shown `TRUE` or `FALSE`, as a spreadsheet shows one. */
  final case class Truth(value: Boolean, formula: Option[String] = None) extends Cell[Nothing] {
    def shown: Text[Nothing] = Text.plain(if (value) "TRUE" else "FALSE")
  }

  /** A cell holding nothing: shown as no text. */
  final case class Empty(formula: Option[String] = None) extends Cell[Nothing] {
    def shown: Text[Nothing] = Text.empty
  }
}
