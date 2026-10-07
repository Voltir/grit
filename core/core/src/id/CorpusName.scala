package grit.core.id

/** A corpus's name in a deployment's catalog: a lowercase letter, then lowercase
  * letters, digits or `_`, as a service's.
  */
opaque type CorpusName = String

object CorpusName {

  /** `text` as a corpus's name, or why not, naming the rule it breaks. */
  def of(text: String): Either[String, CorpusName] =
    Either.cond(
      text.headOption.exists(c => c >= 'a' && c <= 'z') &&
        text.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'),
      text,
      s"a corpus's name is a lowercase letter, then lowercase letters, digits or _: $text"
    )

  def value(name: CorpusName): String = name
}
