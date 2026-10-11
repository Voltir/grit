package grit.knowledge.card

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import grit.core.id.CorpusName

/** A card's key: the SHA-256 of `{corpus}\n{path}`, as 64 lowercase hex digits, so a unit keeps
  * one key however long its path.
  */
opaque type CardKey = String

object CardKey {

  /** The key of `corpus`'s unit at `path`. */
  def of(corpus: CorpusName, path: String): CardKey = {
    val digest = MessageDigest
      .getInstance("SHA-256")
      .digest(s"${CorpusName.value(corpus)}\n$path".getBytes(StandardCharsets.UTF_8))
    digest.map(b => f"${b & 0xff}%02x").mkString
  }

  /** `text` as a key, read back from where one was written, or why not: not 64 lowercase hex
    * digits.
    */
  def parse(text: String): Either[String, CardKey] =
    Either.cond(Hex64.matches(text), text, s"a card's key is 64 lowercase hex digits: $text")

  def value(key: CardKey): String = key

  private val Hex64 = "[0-9a-f]{64}".r
}
