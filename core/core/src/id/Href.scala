package grit.core.id

/** A link as grit keeps it: `{scheme}://{host}…`, its scheme and host lower-cased and its
  * fragment dropped, so two links to one place are one href.
  */
opaque type Href = String

object Href {

  /** 500: the longest link [[in]] finds, and the longest an inbound message is kept with
    * ([[grit.core.inbox.Inbox.ingest]]; `grit.inbound.link` holds none longer).
    */
  val Chars: Int = 500

  /** `text` as an href, normalised, or `None` when it is not `{scheme}://{host}…`. */
  def of(text: String): Option[Href] = text match {
    case Link(scheme, authority, rest) =>
      val at = authority.lastIndexOf('@') + 1
      Some(s"${scheme.toLowerCase}://${authority.take(at)}${authority.drop(at).toLowerCase}$rest")
    case _ => None
  }

  /** Each `http://` or `https://` link in `text`, in order, each once: the run of characters up
    * to whitespace or one of `<>()"'` and a backtick, less a trailing `.`, `,`, `;`, `:`, `!` or
    * `?`; one over [[Chars]] characters left out.
    */
  def in(text: String): Vector[Href] =
    Found
      .findAllIn(text)
      .map(_.reverse.dropWhile(c => ".,;:!?".contains(c)).reverse)
      .filter(_.length <= Chars)
      .flatMap(of)
      .toVector
      .distinct

  def value(h: Href): String = h

  private val Link = "([A-Za-z][A-Za-z0-9+.-]*)://([^/?#]+)([^#]*)(?:#.*)?".r
  private val Found = "(?i)https?://[^\\s<>()\"'`]+".r
}
