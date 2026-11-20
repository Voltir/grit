package grit.core.occasion

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A plugin's wording of an occasion: why grit posts, and what to write; 1 to 2,000 characters,
  * not blank. A plugin makes one only from a constant ([[OccasionBrief.literal]]); core reads
  * back those it stored.
  */
opaque type OccasionBrief = String

object OccasionBrief {

  /** `text` as a brief; text [[read]] refuses, or that is not a constant, does not compile. */
  inline def literal(inline text: String): OccasionBrief = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[OccasionBrief] =
    Literal.checked(text, read)

  def value(brief: OccasionBrief): String = brief

  /** `text` as a brief, or why not: blank, or over 2,000 characters. */
  private[core] def read(text: String): Either[String, OccasionBrief] =
    if (text.isBlank) Left("an occasion's brief is not blank")
    else if (text.length > 2000) Left("an occasion's brief is at most 2,000 characters")
    else Right(text)
}
