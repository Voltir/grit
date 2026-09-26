package grit.core.id

/** A plugin's name, which keys its documents, its cursor and its workflows: lowercase
  * letters, digits and dashes, starting with a letter.
  */
opaque type PluginName = String

object PluginName {

  private val Valid = "[a-z][a-z0-9-]*".r

  /** The name `text` is, or why it is none. */
  def of(text: String): Either[String, PluginName] =
    Either.cond(
      Valid.matches(text),
      text,
      "a plugin's name is lowercase letters, digits and dashes, starting with a letter"
    )

  def value(name: PluginName): String = name
}
