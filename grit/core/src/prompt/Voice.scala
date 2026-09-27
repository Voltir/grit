package grit.core.prompt

/** How grit talks to the person, as they set it: one of grit's [[Voice.Named]] voices, or
  * their own words. It is shown to the model only as a turn's person fragment
  * ([[Voice.fragment]]).
  */
sealed trait Voice

object Voice {

  /** grit's voices, by the name `/set voice` takes. */
  enum Named(val key: String, private[Voice] val words: Option[String]) extends Voice {
    case Sassy
        extends Named(
          "sassy",
          Some(
            "Talk with some sass: dry wit, a raised eyebrow at a shaky plan, short replies " +
              "that get to the point. Tease the idea, never the person. The sass stops at the " +
              "facts: a failure, a declined call, a refusal or something you do not know is " +
              "said plainly and first, never played for a laugh or softened. The sass lives " +
              "in your replies, never in files, commands or anything written for the record."
          )
        )
    case Colleague
        extends Named(
          "colleague",
          Some(
            "Talk like a senior engineer pairing with a peer: blunt, brief, no hand-holding, " +
              "and never narrate what a standard command does. Lead with the answer, disagree " +
              "out loud, and say \"I don't know\" when you don't."
          )
        )
    case Teacher
        extends Named(
          "teacher",
          Some(
            "Talk like a good teacher on a good day: answer first, then explain. Define a " +
              "term the first time you use it, say why as well as what, and reach for a small " +
              "concrete example. Enthusiasm welcome; lectures not."
          )
        )
    case Skeptic
        extends Named(
          "skeptic",
          Some(
            "Talk like a cheerful skeptic: poke at every claim, the person's and your own. " +
              "Name the weakest assumption, say what evidence would settle it, and mark " +
              "anything unverified as unverified."
          )
        )
    case Plain extends Named("plain", None)
  }

  /** The voice when none is set. */
  val Default: Voice = Named.Sassy

  /** A person's own words for how to talk to them: trimmed, never blank, at most
    * [[MaxChars]] characters.
    */
  final case class Own private[Voice] (text: String) extends Voice

  val MaxChars = 2000

  /** The voice `written` sets: the named voice whose key it is, ignoring case and
    * surrounding space; otherwise `written`, trimmed, as the person's own words. `Left`,
    * saying why, when it is blank or longer than [[MaxChars]].
    */
  def of(written: String): Either[String, Voice] = {
    val text = written.trim
    if (text.isEmpty) Left("a voice is a name or some words, not nothing")
    else if (text.length > MaxChars)
      Left(s"a voice is at most $MaxChars characters, not ${text.length}")
    else Named.values.find(_.key.equalsIgnoreCase(text)).fold(Right(Own(text)))(Right(_))
  }

  /** The named voice whose key is `key` exactly, as a stored voice is written; `None` for a
    * name this build does not know.
    */
  def named(key: String): Option[Named] = Named.values.find(_.key == key)

  /** The person-layer fragment for `voice`: "How to talk to this person:", a line break,
    * then the voice's words, from source [[Fragment.Person]]. `None` for [[Named.Plain]].
    */
  def fragment(voice: Voice): Option[Fragment] = {
    val words = voice match {
      case n: Named => n.words
      case Own(text) => Some(text)
    }
    words.map(w => Fragment(Layer.Person, Fragment.Person, s"How to talk to this person:\n$w"))
  }
}
