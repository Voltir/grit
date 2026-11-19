package grit.std.literal

import scala.quoted.{Expr, Quotes}

/** A test's id type whose reader trims, so a literal's value differs from its text. */
opaque type Trimmed = String

object Trimmed {

  def of(text: String): Either[String, Trimmed] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) Left("blank") else Right(trimmed)
  }

  inline def literal(inline text: String): Trimmed = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[Trimmed] =
    Literal.checked(text, of)

  def value(t: Trimmed): String = t
}
