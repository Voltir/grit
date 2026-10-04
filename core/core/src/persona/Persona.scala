package grit.core.persona

/** Who grit presents as to the people it talks with: the `name` it goes by, trimmed, one
  * line, at most [[Persona.MaxChars]] characters, never blank.
  */
final case class Persona private (name: String)

object Persona {

  /** The longest a name may be: triage asks about it in every heard message's question. */
  val MaxChars = 40

  /** grit itself: named grit. */
  val Grit: Persona = new Persona("grit")

  /** The persona named `name`, trimmed, or why not: blank, holding a line break or another
    * control character, or longer than [[MaxChars]].
    */
  def of(name: String): Either[String, Persona] = {
    val trimmed = name.trim
    if (trimmed.isEmpty) Left("a persona's name is blank")
    else if (trimmed.exists(_.isControl))
      Left("a persona's name is one line, with no control character")
    else if (trimmed.length > MaxChars) Left(s"a persona's name is at most $MaxChars characters")
    else Right(new Persona(trimmed))
  }
}
