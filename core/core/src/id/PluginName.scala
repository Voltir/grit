package grit.core.id

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

/** A plugin's name, which keys its documents, its cursor and its workflows: lowercase
  * letters, digits and dashes, starting with a letter.
  */
opaque type PluginName = String

object PluginName {

  /** The name `text` is, or why it is none. */
  def of(text: String): Either[String, PluginName] =
    Slug
      .check("a plugin's name", text, None, leadingDigit = false)
      .left
      .map(_ => "a plugin's name is lowercase letters, digits and dashes, starting with a letter")

  /** [[of]]'s value for the constant `text`. Text [[of]] refuses does not compile, with
    * [[of]]'s reason; nor does text that is not a constant.
    */
  inline def literal(inline text: String): PluginName = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[PluginName] =
    Literal.checked(text, of)

  def value(name: PluginName): String = name
}
