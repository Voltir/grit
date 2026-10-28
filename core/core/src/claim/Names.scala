package grit.core.claim

/** Who or what a claim is for, in its edge's own name for it (a session, or an agent type in
  * one): one line of 1 to 160 characters.
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
