package grit.core.flow

/** One to `N` characters of `[A-Za-z0-9_-]`. */
opaque type Token[N <: Int] = String

object Token {

  /** `text` as a token, or `None` when it is empty, longer than `N` or has another character. */
  def of[N <: Int](text: String)(using n: ValueOf[N]): Option[Token[N]] =
    Option.when(text.nonEmpty && text.length <= n.value && text.forall(allowed))(text)

  /** Whether `c` may stand in a token. */
  def allowed(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' ||
      c == '-'

  extension [N <: Int](t: Token[N]) {
    def text: String = t
  }
}
