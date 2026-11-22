package grit.core.id

/** Who or what works a claim, in its edge's own name for it (a directory's slot, or an agent
  * type's in it), a session's or an agent's when of their forms ([[ClaimHolder.form]]) and any
  * other text too: one line of 1 to 160 characters. A session names the holder it works as, and
  * its work keeps that holder's claims from lapsing ([[grit.core.claim.Claim.lapses]]). Not a
  * [[grit.core.edge.Holder]], the engine holding a database's lock.
  */
opaque type ClaimHolder = String

object ClaimHolder {

  /** 160: the most characters a holder holds. */
  val Longest: Int = 160

  /** `text` as a holder, or why not ([[Bounded.text]], at most [[Longest]] characters). */
  def of(text: String): Either[String, ClaimHolder] =
    Bounded.text("a claim's holder", text, Longest)

  /** The holder of a session at `place` on `machine`: `<machine>/<place>`; or why not: either is
    * empty or holds `/`, or [[of]] refuses it.
    */
  def session(machine: String, place: String): Either[String, ClaimHolder] =
    for {
      m <- part("machine", machine)
      p <- part("place", place)
      h <- of(s"$m/$p")
    } yield h

  /** The holder of an agent of type `kind` that a session working as `session` starts, `tag`
    * telling its starting sessions apart: `<session>/<kind>/<tag>`; or why not: `session` is not
    * a session's ([[form]]), `kind` or `tag` is empty or holds `/`, or [[of]] refuses it.
    */
  def agent(session: ClaimHolder, kind: String, tag: String): Either[String, ClaimHolder] =
    for {
      _ <- Either.cond(
        form(session) == HolderForm.Session,
        (),
        s"an agent's holder is started by a session's, not $session"
      )
      k <- part("agent's type", kind)
      t <- part("tag", tag)
      h <- of(s"$session/$k/$t")
    } yield h

  /** What `h` is the holder of, by its form: two parts a session's (an edge's own two-part
    * holder reads so too), four an agent's.
    */
  def form(h: ClaimHolder): HolderForm = h.split("/", -1).toVector match {
    case parts if parts.exists(_.isEmpty) => HolderForm.Other
    case Vector(_, _) => HolderForm.Session
    case Vector(_, _, kind, _) => HolderForm.Agent(AgentType.of(kind).toOption)
    case _ => HolderForm.Other
  }

  def value(h: ClaimHolder): String = h

  private def part(what: String, text: String): Either[String, String] =
    Either.cond(
      text.nonEmpty && !text.contains('/'),
      text,
      s"a holder's $what is empty or holds '/': $text"
    )
}
