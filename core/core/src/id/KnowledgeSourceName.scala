package grit.core.id

/** A knowledge source's name in a deployment's catalog: a lowercase letter, then lowercase
  * letters, digits or `_`, as a service's.
  */
opaque type KnowledgeSourceName = String

object KnowledgeSourceName {

  /** `text` as a knowledge source's name, or why not, naming the rule it breaks. */
  def of(text: String): Either[String, KnowledgeSourceName] =
    Either.cond(
      text.headOption.exists(c => c >= 'a' && c <= 'z') &&
        text.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'),
      text,
      s"a knowledge source's name is a lowercase letter, then lowercase letters, digits or _: $text"
    )

  def value(name: KnowledgeSourceName): String = name
}
