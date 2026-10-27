package grit.core.prompt

object Stripped {

  /** `text` as grit shows a model a third party's or a steward's words outside any fence: with
    * no character a reader cannot see ([[hides]]); then with no opening, closing or
    * self-closing tag of a [[Tag]] whose kind is [[Tag.Kind.Guidance]], in any case, with any
    * whitespace or attributes inside it, including one that removing another would form.
    * Material inside a [[Fenced]] is never stripped.
    */
  def of(text: String): String = untagged(Unseen.replaceAllIn(text, ""))

  /** Whether `text` holds a character a reader cannot see: a C0 or C1 control character or DEL
    * but line feed, tab and carriage return; a zero-width or invisible formatting character
    * (U+200B–U+200F, U+2060–U+2064, U+FEFF); a bidirectional override or isolate
    * (U+202A–U+202E, U+2066–U+2069); the soft hyphen; or a Unicode tag character
    * (U+E0000–U+E007F). [[of]] removes each, and [[grit.core.sanitise.HardFlags]] flags a text
    * holding one as hidden.
    */
  def hides(text: String): Boolean = Unseen.findFirstIn(text).nonEmpty

  private val Unseen =
    ("[\\x{0}-\\x{8}\\x{B}\\x{C}\\x{E}-\\x{1F}\\x{7F}-\\x{9F}\\x{AD}\\x{200B}-\\x{200F}" +
      "\\x{2060}-\\x{2064}\\x{FEFF}\\x{202A}-\\x{202E}\\x{2066}-\\x{2069}\\x{E0000}-\\x{E007F}]").r

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
