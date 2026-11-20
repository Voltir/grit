package grit.core.occasion

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A plugin's wording of an occasion: why grit posts, and what to write; 1 to 2,000 characters,
  * not blank. A plugin makes one only by [[OccasionBrief.literal]], from a constant in its code;
  * core's reader of stored entries makes one from what it recorded.
  */
opaque type OccasionBrief = String

object OccasionBrief {

  /** `text` as a brief. Text [[read]] refuses does not compile, with its reason; nor does text
    * that is not a constant.
    */
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
