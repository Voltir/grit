package grit.core.id

/** Who or what works a claim, in its edge's own name for it (a directory's slot, or an agent
  * type's in it): one line of 1 to 160 characters. A session names the holder it works as, and
  * its work keeps that holder's claims from lapsing ([[grit.core.claim.Claim.lapses]]).
  */
opaque type ClaimHolder = String

object ClaimHolder {

  /** 160: the most characters a holder holds. */
  val Longest: Int = 160

  /** `text` as a holder, or why not: empty, over [[Longest]] characters, or holding a control
    * character.
    */
  def of(text: String): Either[String, ClaimHolder] =
    if (text.isEmpty || text.length > Longest)
      Left(s"a claim's holder is 1 to $Longest characters")
    else if (text.exists(_.isControl)) Left("a claim's holder holds no control character")
    else Right(text)

  def value(h: ClaimHolder): String = h
}
