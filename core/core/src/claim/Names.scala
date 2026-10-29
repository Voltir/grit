package grit.core.claim

/** The role a claim is made as, in its plugin's own name for it: one line of 1 to 64
  * characters.
  */
opaque type ClaimRole = String

object ClaimRole {

  /** 64: the most characters a role holds. */
  val Longest: Int = 64

  /** `text` as a role, or why not ([[grit.core.id.Bounded.text]], at most [[Longest]]
    * characters).
    */
  def of(text: String): Either[String, ClaimRole] =
    grit.core.id.Bounded.text("a claim's role", text, Longest)

  def value(r: ClaimRole): String = r
}
