package grit.core.persona

import grit.core.prompt.{Fragment, Layer, Voice}

/** Who grit presents as to the people it talks with: the `name` it goes by, trimmed, one
  * line, at most [[Persona.MaxChars]] characters, never blank; and its `voice`, how it sounds
  * where people read it, [[Voice.Named.Plain]] unless its deployment gives one.
  */
final case class Persona private (name: String, voice: Voice) {

  /** This persona sounding as `voice` where people read it; [[Voice.Named.Plain]] for none, as
    * a deployment's quiet mode is.
    */
  def speaking(voice: Voice): Persona = new Persona(name, voice)

  /** What a model writing for people to read (a Slack reply, a direct message, a job's post)
    * is told of how to sound: this persona's voice in its words ([[Voice.words]]). An
    * edge-layer fragment from [[Fragment.Persona]]; `None` for [[Voice.Named.Plain]].
    */
  def voiced: Option[Fragment] =
    Voice
      .words(voice)
      .map(words =>
        Fragment(
          Layer.Edge,
          Fragment.Persona,
          s"How you sound here, as the people running you chose it:\n$words"
        )
      )
}

object Persona {

  /** The longest a name may be: triage asks about it in every heard message's question. */
  val MaxChars = 40

  /** grit itself: named grit. */
  val Grit: Persona = new Persona("grit", Voice.Named.Plain)

  /** The persona named `name`, trimmed, or why not: blank, holding a line break or another
    * control character, or longer than [[MaxChars]].
    */
  def of(name: String): Either[String, Persona] = {
    val trimmed = name.trim
    if (trimmed.isEmpty) Left("a persona's name is blank")
    else if (trimmed.exists(_.isControl))
      Left("a persona's name is one line, with no control character")
    else if (trimmed.length > MaxChars) Left(s"a persona's name is at most $MaxChars characters")
    else Right(new Persona(trimmed, Voice.Named.Plain))
  }
}
