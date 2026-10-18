package grit.knowledge.ref

/** A link as knowledge keeps it: `{scheme}://{host}…`, its scheme and host lower-cased and its
  * fragment dropped, so two links to one place are one href.
  */
opaque type Href = String

object Href {

  /** `text` as an href, normalised, or `None` when it is not `{scheme}://{host}…`. */
  def of(text: String): Option[Href] = text match {
    case Link(scheme, authority, rest) =>
      val at = authority.lastIndexOf('@') + 1
      Some(s"${scheme.toLowerCase}://${authority.take(at)}${authority.drop(at).toLowerCase}$rest")
    case _ => None
  }

  def value(h: Href): String = h

  private val Link = "([A-Za-z][A-Za-z0-9+.-]*)://([^/?#]+)([^#]*)(?:#.*)?".r
}
