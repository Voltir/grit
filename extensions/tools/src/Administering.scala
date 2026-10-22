package grit.tools

import scala.concurrent.duration.*

import grit.core.admin.{Administration, Answer, Command, PlaceNames}
import grit.core.clock.Clock
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Tool,
  ToolName,
  ToolSpec
}
import grit.core.visibility.Label

/** `label_room` and `quiet_room`: a room's label set, or the room made quiet or not, by asking in
  * a conversation, for the room the conversation is in. Each asks its turn's asker first and
  * waits [[AnswerWithin]], showing the change in words; once approved it runs through
  * `administration` as that asker ([[grit.core.admin.Administration.runFor]]), and answers the
  * model with [[grit.core.admin.Answer]]'s words, or the refusal's. A label that does not read
  * is refused before anyone is asked, with the words the slash command's `help` shows. What
  * either answers is grit's own words.
  */
object Administering {

  /** 15 minutes: a waiting turn holds its thread, and a card's `response_url` lives 30 minutes
    * after a click.
    */
  val AnswerWithin: FiniteDuration = 15.minutes

  /** `label_room`'s name. */
  val LabelRoom: ToolName = ToolName("label_room")

  /** `quiet_room`'s name. */
  val QuietRoom: ToolName = ToolName("quiet_room")

  /** Both tools, acting through `administration` at the time `clock` tells, each told its turn
    * by its call ([[grit.core.tool.Hosted.calling]], `CallSlot.turn`).
    */
  def tools(
      administration: Administration^,
      clock: Clock^
  ): Vector[Tool.Offered^{administration, clock}] =
    Vector[Tool.Offered^{administration, clock}](
      labelling.calling((command, at) => ran(administration.runFor(at.turn, command, clock.now()))),
      quieting.calling((command, at) => ran(administration.runFor(at.turn, command, clock.now())))
    )

  /** What the model is told of `label_room`, and how a call reads and is shown. */
  private val labelling: Hosted[Command] =
    Hosted(
      ToolSpec(
        LabelRoom,
        "Set the label of the room this conversation is in, which decides who may read what " +
          "is said here, or set it back to its default. Call it when the person asks to label " +
          "this room, or to make it confidential, internal or the like. `label` is a level " +
          "(public, internal, confidential or restricted) then any compartments, separated by " +
          "spaces, or `default`. The change is made as the person who asked, under the rules " +
          "the room's slash command keeps, and is refused if they may not make it.",
        Args
          .of((label = Field.text("The level then any compartments, or `default`.")))
          .refine(a => labelled(a.label))
      ),
      Gate.Ask(
        {
          case Command.SetLabel(label) =>
            s"Set this room's label to ${Label.written(label).split('+').mkString(", ")}."
          case _ => "Set this room's label back to its default."
        },
        AnswerWithin
      ),
      {
        case Command.SetLabel(label) => Label.written(label)
        case _ => "default"
      },
      Believed.Own
    )

  /** What the model is told of `quiet_room`, and how a call reads and is shown. */
  private val quieting: Hosted[Command] =
    Hosted(
      ToolSpec(
        QuietRoom,
        "Make the room this conversation is in quiet, so grit posts nothing there unasked and " +
          "still answers when mentioned, or not quiet again. Call it when the person asks grit " +
          "to stay quiet here, or to speak up again. The change is made as the person who " +
          "asked, and is refused if they may not make it.",
        Args
          .of((on = Field.flag("Whether the room is to be quiet.")))
          .map(a => Command.Quiet(a.on))
      ),
      Gate.Ask(
        {
          case Command.Quiet(false) => "Let grit post in this room unasked again."
          case _ => "Make this room quiet: grit posts nothing here unasked."
        },
        AnswerWithin
      ),
      {
        case Command.Quiet(false) => "speak"
        case _ => "quiet"
      },
      Believed.Own
    )

  /** The command `words` give `label_room`: `default` sets the room back to its default,
    * anything else is read as the slash command's `label` reads it; refused when it names
    * another place, names no level, or does not read.
    */
  private def labelled(words: String): Either[ArgsError, Command] =
    if (words.trim.equalsIgnoreCase("default")) Right(Command.Unlabel)
    else
      Command.read(s"label $words", _ => None, PlaceNames.None) match {
        case Right(set: Command.SetLabel) => Right(set)
        case Right(Command.At(_, _)) => Left(ArgsError.Refused("label", ThisRoomOnly))
        case Right(_) => Left(ArgsError.Refused("label", Wanted))
        case Left(why) => Left(ArgsError.Refused("label", why))
      }

  /** Why a label naming a place is refused: these tools change only the conversation's room. */
  private val ThisRoomOnly: String =
    "label_room labels only the room this conversation is in; it names no other place."

  /** Why a label naming nothing is refused. */
  private val Wanted: String = "label_room takes a level, then any compartments, or default."

  /** A change's answer as the model reads it: a refusal's words as a failed call. */
  private def ran(answer: Either[?, Answer]): Outcome =
    answer match {
      case Right(Answer.Refused(refusal)) => Outcome.Failed(refusal.text)
      case Right(done) => Outcome.Done(done.text)
      case Left(_) => Outcome.Failed(Unchanged)
    }

  /** The answer when the store cannot be written: it names nothing of what failed. */
  private val Unchanged: String = "I could not change this room just now. Ask again in a moment."
}
