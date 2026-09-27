package grit.core.id

/** A short content hash: the first 16 hex digits of SHA-256 over a string's UTF-8 bytes. The
  * one definition behind every content-addressed id in core, so equal content always gets an
  * equal id, whichever id it is.
  */
private[core] object ShortHash {
  def of(text: String): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    digest.digest(text.getBytes("UTF-8")).take(8).map(b => f"$b%02x").mkString
  }
}
