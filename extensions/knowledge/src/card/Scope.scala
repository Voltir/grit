package grit.knowledge.card

import grit.core.id.Sha256

/** What of its source a directory's card was made under, as a digest: the SHA-256 of the terms
  * that decide which units become cards, sorted, one to a line, as 64 lowercase hex digits. A
  * directory's card made under another scope than its source's now does not say what of the
  * directory the source holds.
  */
opaque type Scope = String

object Scope {

  /** The scope of `terms`, whatever their order. */
  def of(terms: Set[String]): Scope = Sha256.text(terms.toVector.sorted.mkString("\n")).hex

  /** `text` as a scope, read back from where one was written, or why not: not 64 lowercase hex
    * digits.
    */
  def parse(text: String): Either[String, Scope] =
    Either.cond(Hex64.matches(text), text, s"a scope is 64 lowercase hex digits: $text")

  def value(scope: Scope): String = scope

  private val Hex64 = "[0-9a-f]{64}".r
}
