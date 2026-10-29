package grit.core.claim

/** The role a claim is made as, in its plugin's own name for it: one line of 1 to 64
  * characters.
  */
opaque type ClaimRole = String

object ClaimRole {

  /** 64: the most characters a role holds. */
  val Longest: Int = 64

  /** `text` as a role, or why not: empty, over [[Longest]] characters, or holding a control
    * character.
    */
  def of(text: String): Either[String, ClaimRole] =
    if (text.isEmpty || text.length > Longest) Left(s"a claim's role is 1 to $Longest characters")
    else if (text.exists(_.isControl)) Left("a claim's role holds no control character")
    else Right(text)

  def value(r: ClaimRole): String = r
}
