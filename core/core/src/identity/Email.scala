package grit.core.identity

/** An email address, trimmed and lowercased, and otherwise as given: two that differ
  * otherwise are two addresses.
  */
opaque type Email = String

object Email {

  /** `raw` trimmed and lowercased, or why not: it has whitespace inside, no `@`, more than
    * one, or nothing on either side of it.
    */
  def of(raw: String): Either[String, Email] = {
    val address = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if (address.exists(_.isWhitespace)) Left(s"an email address has no whitespace inside: $raw")
    else
      address.split("@", -1) match {
        case Array(local, domain) if local.nonEmpty && domain.nonEmpty => Right(address)
        case _ => Left(s"an email address is one @ with something on either side: $raw")
      }
  }

  def value(e: Email): String = e
}
