package grit.core.prompt

object Stripped {

  /** `text` as grit shows a model a third party's or a steward's words outside any fence: with
    * no C0 or C1 control character or DEL but line feed and tab, no Unicode tag character
    * (U+E0000–U+E007F), no bidirectional override or isolate (U+202A–U+202E, U+2066–U+2069) and
    * no zero-width character (U+200B–U+200D, U+2060, U+FEFF); then with no opening, closing or
    * self-closing tag of a [[Tag]] whose kind is [[Tag.Kind.Guidance]], in any case, with any
    * whitespace or attributes inside it, including one that removing another would form.
    * Material inside a [[Fenced]] is never stripped.
    */
  def of(text: String): String = untagged(Unseen.replaceAllIn(text, ""))

  private val Unseen =
    ("[\\x{0}-\\x{8}\\x{B}-\\x{1F}\\x{7F}-\\x{9F}\\x{200B}-\\x{200D}\\x{2060}\\x{FEFF}" +
      "\\x{202A}-\\x{202E}\\x{2066}-\\x{2069}\\x{E0000}-\\x{E007F}]").r

  private val Guidance = {
    val names = Tag.values.toVector.filter(_.kind == Tag.Kind.Guidance).map(_.name)
    s"(?i)<\\s*/?\\s*(?:${names.mkString("|")})(?=[\\s/>])[^>]*>".r
  }

  /** `text` with no guidance tag, removed until removing one forms no other. */
  @annotation.tailrec
  private def untagged(text: String): String = {
    val next = Guidance.replaceAllIn(text, "")
    if (next == text) text else untagged(next)
  }
}
