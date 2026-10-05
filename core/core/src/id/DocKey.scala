package grit.core.id

/** A document's key, as its plugin chose it: one line of 1 to 200 characters. */
opaque type DocKey = String

object DocKey {

  /** `text` as a key, or why not: empty, over 200 characters, or holding a control character. */
  def of(text: String): Either[String, DocKey] =
    if (text.isEmpty || text.length > 200) Left("a document's key is 1 to 200 characters")
    else if (text.exists(_.isControl)) Left("a document's key holds no control character")
    else Right(text)

  def value(k: DocKey): String = k

}
