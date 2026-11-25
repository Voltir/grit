package grit.turn

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ToolSchema
import grit.core.recipe.Briefing
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}

/** The `task_status` tool, where a turn briefed for one task that is ending, its input limit
  * met or its calls run out, says whether its task is done ([[TurnLoop.declared]]). It is offered on every call of such a
  * turn ([[TurnLoop.offered]]), and never run: a reply that declares ends the turn, its handoff
  * the reply's text; a call before the turn is ending is refused ([[Early]]).
  */
object TaskStatus {

  /** The tool's name. */
  val Name: ToolName = ToolName("task_status")

  /** Whether the task is finished, or work is left: for a next turn to go on with, or, on a
    * spawned child's, left undone.
    */
  enum State derives CanEqual {
    case Done, More
  }

  /** What the model declared: the task's `state`, and `handoff`, not blank: on [[State.Done]]
    * the answer, on [[State.More]] what is done and left, as [[Handoff]] says who reads it.
    */
  final case class Declared(state: State, handoff: String)

  /** Whether a turn's calls offer `task_status`, and who reads its handoff. */
  enum Handoff derives CanEqual {

    /** Not offered: a turn briefed for a conversation ([[Briefing.Conversing]]). */
    case Unoffered

    /** Offered to a turn briefed for one task: `done`'s handoff is the answer to the person,
      * and on `more` a next turn goes on from it.
      */
    case ToNextTurn

    /** Offered to a spawned child's turn: its handoff, on `done` and `more` alike, is passed
      * back to the conversation that spawned it, and nothing goes on from it.
      */
    case ToParent
  }

  object Handoff {

    /** A turn's, from its recipe's `briefing` and its `root`: [[ToParent]] for one briefed for
      * one task whose root is [[TurnOffer.Root.Spawned]].
      */
    def of(briefing: Briefing, root: TurnOffer.Root): Handoff = (briefing, root) match {
      case (Briefing.Conversing, _) => Unoffered
      case (Briefing.Tasked, TurnOffer.Root.Spawned(_)) => ToParent
      case (Briefing.Tasked, _) => ToNextTurn
    }
  }

  /** The tool's spec, worded for whoever reads the handoff as `handoff` says: `state` is
    * `done` or `more`; `handoff` is trimmed, and refused blank. Its arguments read alike
    * whatever `handoff` is.
    */
  def spec(handoff: Handoff): ToolSpec[Declared] = {
    val toParent = handoff == Handoff.ToParent
    val handed = Field.text(
      if (toParent) ToParentHandoff
      else
        "For done: your answer to the person. For more: what the next turn needs. It sees the " +
          "task and this handoff, none of your earlier replies, so say what is done so far, " +
          "what is left, and where things are."
    )
    ToolSpec(
      Name,
      if (toParent) ToParentDescription
      else
        "Say whether the task is done, once you are told this turn is ending; before then it " +
          "is not recorded. Call it alone: done gives your " +
          "answer to the person; more hands the work to a new turn, which goes on from your handoff.",
      Args
        .of(
          (
            state = Field.oneOf(
              if (toParent) ToParentState
              else "done: the task is finished. more: work is left for the next turn.",
              "done",
              "more"
            ),
            handoff = handed
          )
        )
        .refine(a =>
          Option(a.handoff.trim)
            .filter(_.nonEmpty)
            .map(h => Declared(if (a.state == "done") State.Done else State.More, h))
            .toRight(ArgsError.Missing("handoff", handed.accepts))
        )
    )
  }

  private val ToParentDescription =
    "Say whether the task is done, once you are told this run is ending; before then it is " +
      "not recorded. Call it alone. Either way the run ends, and your handoff is passed back " +
      "to the conversation that started this run: done says the task is finished, more says " +
      "work is left, which this run will not continue."

  private val ToParentState =
    "done: the task is finished. more: work is left that this run did not get to."

  private val ToParentHandoff =
    "Your reply, passed back to the conversation that started this run, which sees none of " +
      "your earlier replies. For done: what you did and what you found. For more: what is " +
      "done so far, what is left, and where things are."

  /** What the model is offered of it, worded as `handoff` says, its arguments held to the schema
    * when `strict`.
    */
  def schema(strict: Boolean, handoff: Handoff): ToolSchema = spec(handoff).schema(strict)

  /** The calls to it in `reply`, in order. */
  def calls(reply: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    reply.blocks.collect { case c: AssistantBlock.ToolCall if c.name == ToolName.value(Name) => c }

  /** What the first call to it in `reply` declares, when its arguments read. */
  def of(reply: Message.Assistant): Option[Declared] =
    calls(reply).headOption.flatMap(c => spec(Handoff.ToNextTurn).args.read(c.arguments).toOption)

  /** What a call to it is answered when it does not end the turn: it was made beside other
    * calls, or its arguments did not read.
    */
  val Unrecorded: String =
    "Not recorded. Call task_status alone, with state done or more and a handoff that is not " +
      "blank."

  /** What a call to it is answered before its turn is ending: before its budget's last call
    * with tools ([[TurnLoop.asksStatus]]).
    */
  val Early: String =
    "Not recorded: task_status is for a turn that is ending, and you will be told when this " +
      "one is. Go on with the task, and answer in text when it is done."
}
