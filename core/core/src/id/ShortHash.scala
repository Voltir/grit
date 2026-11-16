package grit.core.id

/** A short content hash over a string's UTF-8 bytes. The one definition behind every
  * content-addressed id in grit, so equal content always gets an equal id, whichever id it is.
  */
object ShortHash {

  /** The first `digits` hex digits of SHA-256 over `text`'s UTF-8 bytes, all 64 when `digits` is
    * more.
    */
  def of(text: String, digits: Int = 16): String = Sha256.text(text).hex.take(digits)
}
