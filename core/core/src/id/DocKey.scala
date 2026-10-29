package grit.core.id

/** A document's key, as its plugin chose it: one line of 1 to 200 characters. */
opaque type DocKey = String

object DocKey {

  /** `text` as a key, or why not ([[Bounded.text]], at most 200 characters). */
  def of(text: String): Either[String, DocKey] = Bounded.text("a document's key", text, 200)

  def value(k: DocKey): String = k

}
