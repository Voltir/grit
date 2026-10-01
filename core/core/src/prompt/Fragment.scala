package grit.core.prompt

import grit.core.id.ShortHash

/** One piece of a system prompt: `text`, from `source` ([[Fragment.Grit]] for grit's own
  * words, a file's absolute path, or for a place's declared instructions the place as
  * written).
  */
final case class Fragment(layer: Layer, source: String, text: String) {

  /** A content hash of all three ([[Fragment.write]]): equal for equal fragments, and
    * different when any of the three differs.
    */
  def id: FragmentId = FragmentId(ShortHash.of(ujson.write(Fragment.write(this))))
}

object Fragment {

  /** The source of a fragment in grit's own words. */
  val Grit = "grit"

  /** The source of a fragment the person chose. */
  val Person = "person"

  /** The stored form, `{"layer","source","text"}`, which the id hashes: a change here changes
    * every fragment's id.
    */
  def write(f: Fragment): ujson.Value =
    ujson.Obj("layer" -> f.layer.key, "source" -> f.source, "text" -> f.text)
}

/** A fragment's content hash ([[Fragment.id]]). */
opaque type FragmentId = String

object FragmentId {

  private[prompt] def apply(value: String): FragmentId = value

  def value(id: FragmentId): String = id

  /** `text` as an id, or why not: not 16 lowercase hex digits. */
  def of(text: String): Either[String, FragmentId] =
    if (text.length == 16 && text.forall(c => c.isDigit || ('a' to 'f').contains(c))) Right(text)
    else Left(s"not a fragment id: $text")
}
