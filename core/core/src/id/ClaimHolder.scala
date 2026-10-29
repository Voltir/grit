package grit.core.id

/** Who or what works a claim, in its edge's own name for it (a directory's slot, or an agent
  * type's in it): one line of 1 to 160 characters. A session names the holder it works as, and
  * its work keeps that holder's claims from lapsing ([[grit.core.claim.Claim.lapses]]).
  */
opaque type ClaimHolder = String

object ClaimHolder {

  /** 160: the most characters a holder holds. */
  val Longest: Int = 160

  /** `text` as a holder, or why not ([[Bounded.text]], at most [[Longest]] characters). */
  def of(text: String): Either[String, ClaimHolder] =
    Bounded.text("a claim's holder", text, Longest)

  def value(h: ClaimHolder): String = h
}
