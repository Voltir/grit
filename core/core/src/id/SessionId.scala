package grit.core.id

import scala.quoted.{Expr, Quotes}

import grit.std.literal.Literal

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

  /** [[of]]'s value for the constant `text`. Text [[of]] refuses does not compile, with
    * [[of]]'s reason; nor does text that is not a constant.
    */
  inline def literal(inline text: String): Harness = ${ literalOf('text) }

  private def literalOf(text: Expr[String])(using Quotes): Expr[Harness] = Literal.checked(text, of)

  def value(h: Harness): String = h
}

/** A working session, by its harness and the harness's own id for it, `local`: one line of 1
  * to 200 characters. A grit turn's `local` is its conversation's id.
  */
final case class SessionId private (harness: Harness, local: String) {

  /** `<harness>:<local>`, as a session is written in text: a store's keys, its reasons, a
    * holder's hash.
    */
  def written: String = s"${Harness.value(harness)}:$local"
}

object SessionId {

  /** 200: the most characters a session's `local` id holds. */
  val Longest: Int = 200

  /** The session `local` of `harness`, or why not ([[Bounded.text]] of `local`, at most
    * [[Longest]] characters).
    */
  def of(harness: Harness, local: String): Either[String, SessionId] =
    Bounded.text("a session's id", local, Longest).map(new SessionId(harness, _))

  /** `text` as [[SessionId.written]] writes a session, or why not. */
  def read(text: String): Either[String, SessionId] =
    // A harness's name is a slug, holding no `:`: the first one ends it.
    text.indexOf(':') match {
      case -1 => Left("not a session's id, written <harness>:<id>")
      case i => Harness.of(text.take(i)).flatMap(of(_, text.drop(i + 1)))
    }
}
