package grit.prose.form

import java.time.{LocalDate, LocalDateTime}

/** A table's column: its `name`. */
final case class Column[+A <: caps.Pure](name: Text[A])

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

  object Number {

    /** `shown` as a number cell, shown as written less the spaces around it, when it reads as
      * one: an optional `-` or `+`, an optional currency sign (`$`, `€` or `£`), then digits,
      * plain or grouped in threes by `,`, then an optional `.` and digits, then an optional
      * `%`. Its value is the number as written: `12%` is 12, `$1,200` is 1200. `None` for
      * anything else: a suffix such as `1.2k` (`k`, `M` and `B` scale by convention, not by
      * rule), a unit after the number, a `,` as the decimal mark.
      */
    def read(shown: String): Option[Number] = {
      val s = shown.trim
      Written.findFirstMatchIn(s).map { m =>
        val sign = if (m.group(1) == "-") "-" else ""
        val digits = m.group(2).replace(",", "") + Option(m.group(3)).getOrElse("")
        Number(BigDecimal(sign + digits), Text.plain(s))
      }
    }

    private val Written = """^([-+])?[$€£]?(\d{1,3}(?:,\d{3})+|\d+)(\.\d+)?%?$""".r
  }

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
