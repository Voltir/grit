package grit.knowledge.ref

import grit.core.id.{CorpusName, DocKey, Sha256}

/** A card's key: the SHA-256 of `{corpus}\n{path}`, as 64 lowercase hex digits, so a unit keeps
  * one key however long its path.
  */
opaque type CardKey = String

object CardKey {

  /** The key of `corpus`'s unit at `path`. */
  def of(corpus: CorpusName, path: String): CardKey =
    Sha256.text(s"${CorpusName.value(corpus)}\n$path").hex

  /** `text` as a key, read back from where one was written, or why not: not 64 lowercase hex
    * digits.
    */
  def parse(text: String): Either[String, CardKey] =
    Either.cond(Hex64.matches(text), text, s"a card's key is 64 lowercase hex digits: $text")

  def value(key: CardKey): String = key

  /** The key of the document holding `key`'s card: its hex digits. */
  def docKey(key: CardKey): DocKey =
    // 64 hex digits, one line within 200 characters: DocKey.of never refuses them.
    DocKey.of(key).fold(why => throw new IllegalStateException(why), identity)

  private val Hex64 = "[0-9a-f]{64}".r
}
