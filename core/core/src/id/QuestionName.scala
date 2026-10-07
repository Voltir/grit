package grit.core.id

/** A question's name in a question set, kept with its answer: a lowercase letter, then
  * lowercase letters, digits or `-`; or a question asked once per corpus,
  * `<prefix>:<source>` ([[QuestionName.per]]), which no declared name can be.
  */
opaque type QuestionName = String

object QuestionName {

  private def declared(text: String): Boolean =
    text.headOption.exists(c => c >= 'a' && c <= 'z') &&
      text.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-')

  /** `text` as a declared question's name, or why not, naming the rule it breaks. */
  def of(text: String): Either[String, QuestionName] =
    Either.cond(
      declared(text),
      text,
      s"a question's name is a lowercase letter, then lowercase letters, digits or -: $text"
    )

  /** The name of `prefix`'s question asked of `source`: `<prefix>:<source>`. */
  def per(prefix: QuestionName, source: CorpusName): QuestionName =
    s"$prefix:${CorpusName.value(source)}"

  /** `text` as a name [[of]] or [[per]] makes, as a stored answer names it, or why not. */
  def read(text: String): Either[String, QuestionName] =
    text.lastIndexOf(':') match {
      case -1 => of(text)
      case i =>
        for {
          prefix <- read(text.take(i))
          source <- CorpusName.of(text.drop(i + 1))
        } yield per(prefix, source)
    }

  def value(name: QuestionName): String = name
}
