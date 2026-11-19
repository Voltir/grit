package grit.core.id

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A job's name (ADR 0029), which names its task place `task:{name}` and every schedule of
  * it: lowercase letters, digits and dashes, starting with a letter.
  */
opaque type JobName = String

object JobName {

  /** The name `text` is, or why it is none. */
  def of(text: String): Either[String, JobName] =
    Slug
      .check("a job's name", text, None, leadingDigit = false)
      .left
      .map(_ => "a job's name is lowercase letters, digits and dashes, starting with a letter")

  /** [[of]]'s value for the constant `text`. Text [[of]] refuses does not compile, with
    * [[of]]'s reason; nor does text that is not a constant.
    */
  inline def literal(inline text: String): JobName = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[JobName] = Literal.checked(text, of)

  def value(name: JobName): String = name
}
