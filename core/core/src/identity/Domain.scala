package grit.core.identity

/** A domain an email may be in, trimmed and lowercased: `example.com`. A deployment claims the
  * domains whose addresses link accounts ([[Identities]]); each is matched exactly, so a
  * subdomain is a domain of its own.
  */
opaque type Domain = String

object Domain {

  /** `raw` trimmed and lowercased, or why not: blank, holding `@` or whitespace, or with no
    * `.` between two labels.
    */
  def of(raw: String): Either[String, Domain] = {
    val domain = raw.trim.toLowerCase(java.util.Locale.ROOT)
    if (domain.isEmpty) Left(s"a domain is not blank: $raw")
    else if (domain.exists(c => c == '@' || c.isWhitespace))
      Left(s"a domain holds no @ or whitespace: $raw")
    else {
      val labels = domain.split("\\.", -1)
      if (labels.length < 2 || labels.exists(_.isEmpty))
        Left(s"a domain is labels joined by ., at least two: $raw")
      else Right(domain)
    }
  }

  def value(d: Domain): String = d

  /** An email's domain as [[Email.of]] kept it: already trimmed and lowercased. */
  private[identity] def within(emailDomain: String): Domain = emailDomain

  given CanEqual[Domain, Domain] = CanEqual.derived
}
