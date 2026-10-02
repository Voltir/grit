package grit.core.id

/** A shadow variant's name, which keys its recorded answers and its workflows: lowercase
  * letters, digits and dashes.
  */
opaque type ShadowName = String

object ShadowName {

  private val Valid = "[a-z0-9-]+".r

  /** The name `text` is, or why it is none. */
  def of(text: String): Either[String, ShadowName] =
    Either.cond(
      Valid.matches(text),
      text,
      "a shadow's name is lowercase letters, digits and dashes"
    )

  def value(name: ShadowName): String = name
}
