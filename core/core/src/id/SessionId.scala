package grit.core.id

/** What runs a working session: grit itself, or a harness beside it (`claude-code`). Not an
  * [[EdgeName]], which names the edge a message arrives through: a grit turn's session has the
  * harness [[Harness.Grit]] whatever edge its conversation is on.
  */
opaque type Harness = String

object Harness {

  /** 64: the most characters a harness's name holds. */
  val Longest: Int = 64

  /** grit's own turns. */
  val Grit: Harness = "grit"

  /** `s` as a harness's name, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, Harness] =
    Slug.check("a harness's name", s, Some(Longest), leadingDigit = false)

  def value(h: Harness): String = h
}

/** A working session, by its harness and the harness's own id for it, `local`: one line of 1
  * to 200 characters. A grit turn's `local` is its conversation's id.
  */
final case class SessionId private (harness: Harness, local: String)

object SessionId {

  /** 200: the most characters a session's `local` id holds. */
  val Longest: Int = 200

  /** The session `local` of `harness`, or why not: `local` empty, over [[Longest]] characters,
    * or holding a control character.
    */
  def of(harness: Harness, local: String): Either[String, SessionId] =
    if (local.isEmpty || local.length > Longest)
      Left(s"a session's id is 1 to $Longest characters")
    else if (local.exists(_.isControl)) Left("a session's id holds no control character")
    else Right(new SessionId(harness, local))
}
