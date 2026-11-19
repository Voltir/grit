package grit.core.act

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A move's name within its run, and its steps': `ask:{name}`, `call:{name}` or `keep:{name}`,
  * then each with `:{phase}` after it. 1 to 200 characters, none a control character or `:`.
  */
opaque type MoveName = String

object MoveName {

  /** `text`, or why not: empty, over 200 characters, or holding a control character or `:`. */
  def of(text: String): Either[String, MoveName] =
    if (text.isEmpty) Left("a move's name is not empty")
    else if (text.length > 200) Left("a move's name is at most 200 characters")
    else if (text.exists(c => c == ':' || c.isControl))
      Left("a move's name holds no ':' and no control character")
    else Right(text)

  /** [[of]]'s value for the constant `text`. Text [[of]] refuses does not compile, with
    * [[of]]'s reason; nor does text that is not a constant.
    */
  inline def literal(inline text: String): MoveName = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[MoveName] =
    Literal.checked(text, of)

  def value(name: MoveName): String = name
}
