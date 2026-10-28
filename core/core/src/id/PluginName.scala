package grit.core.id

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

  def value(name: PluginName): String = name
}
