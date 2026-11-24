package grit.mcp.searchconsole

import java.net.URI

/** A Search Console property as Google names it: `sc-domain:` and a host for a domain property,
  * or an http(s) URL ending in `/` for a URL-prefix one.
  */
opaque type SiteUrl = String

object SiteUrl {

  /** `raw`, trimmed, as a property's name, or why not, in plain words. */
  def of(raw: String): Either[String, SiteUrl] = {
    val t = raw.trim
    val domain = "sc-domain:"
    if (t.startsWith(domain)) {
      val host = t.drop(domain.length)
      if (host.nonEmpty && host.forall(c => c.isLetterOrDigit || c == '.' || c == '-')) Right(t)
      else Left(s"$raw is not sc-domain: and a bare host")
    } else if (t.startsWith("https://") || t.startsWith("http://")) {
      val hasHost = scala.util.Try(URI.create(t)).toOption.flatMap(u => Option(u.getHost)).nonEmpty
      if (!hasHost) Left(s"$raw names no host")
      else if (!t.endsWith("/")) Left(s"$raw does not end in /, as a URL-prefix property does")
      else Right(t)
    } else Left(s"$raw is neither sc-domain: and a host nor an http(s) URL ending in /")
  }

  def value(s: SiteUrl): String = s

  given CanEqual[SiteUrl, SiteUrl] = CanEqual.derived
}

/** The account's standing on a property, as Google names it. */
enum Permission derives CanEqual {
  case Owner, Full, Restricted, Unverified

  /** Whether it may read the property's search traffic: Unverified may not. */
  def reads: Boolean = this != Unverified
}

final case class Site(url: SiteUrl, permission: Permission) derives CanEqual
