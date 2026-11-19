package grit.core.id

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A document's key, as its plugin chose it: one line of 1 to 200 characters. */
opaque type DocKey = String

object DocKey {

  /** `text` as a key, or why not ([[Bounded.text]], at most 200 characters). */
  def of(text: String): Either[String, DocKey] = Bounded.text("a document's key", text, 200)

  /** [[of]]'s value for the constant `text`. Text [[of]] refuses does not compile, with
    * [[of]]'s reason; nor does text that is not a constant.
    */
  inline def literal(inline text: String): DocKey = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[DocKey] = Literal.checked(text, of)

  def value(k: DocKey): String = k

}
