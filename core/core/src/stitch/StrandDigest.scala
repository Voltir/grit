package grit.core.stitch

import grit.core.id.{EntryId, ShortHash}

/** A strand's messages as one content hash: 16 hex digits over their entry ids, sorted, one to
  * a line ([[grit.core.id.ShortHash]]): the same messages, the same digest, in any order.
  */
opaque type StrandDigest = String

object StrandDigest {
  def of(entries: Vector[EntryId]): StrandDigest =
    ShortHash.of(entries.map(EntryId.value).sorted.mkString("\n"))

  /** `text` as a digest; `Left` when it is not 16 lowercase hex digits. */
  def read(text: String): Either[String, StrandDigest] =
    Either.cond(Hex.matches(text), text, s"not a strand digest: $text")

  def value(d: StrandDigest): String = d

  private val Hex = "[0-9a-f]{16}".r
}
