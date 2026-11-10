package grit.turn

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ToolSchema
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}

/** The `task_status` tool, where a turn briefed for one task that is ending, its input limit
  * met or its calls run out, says whether its task is done ([[TurnLoop.declared]]). It is offered on every call of such a
  * turn ([[TurnLoop.offered]]), and never run: a reply that declares ends the turn, its handoff
  * the reply's text; a call before the turn is ending is refused ([[Early]]).
  */
object TaskStatus {

  /** The tool's name. */
  val Name: ToolName = ToolName("task_status")

  /** Whether the task is finished, or work is left for a next turn. */
  enum State derives CanEqual {
    case Done, More
  }

  /** What the model declared: the task's `state`, and `handoff`, not blank: on [[State.Done]]
    * the answer to the person, on [[State.More]] what the next turn needs.
    */
  final case class Declared(state: State, handoff: String)

  /** The tool's spec: `state` is `done` or `more`; `handoff` is trimmed, and refused blank. */
  val Spec: ToolSpec[Declared] = {
    val handoff = Field.text(
      "For done: your answer to the person. For more: what the next turn needs. It sees the " +
        "task and this handoff, none of your earlier replies, so say what is done so far, " +
        "what is left, and where things are."
    )
    ToolSpec(
      Name,
      "Say whether the task is done, once you are told this turn is ending; before then it " +
        "is not recorded. Call it alone: done gives your " +
        "answer to the person; more hands the work to a new turn, which goes on from your handoff.",
      Args
        .of(
          (
            state = Field.oneOf(
              "done: the task is finished. more: work is left for the next turn.",
              "done",
              "more"
            ),
            handoff = handoff
          )
        )
        .refine(a =>
          Option(a.handoff.trim)
            .filter(_.nonEmpty)
            .map(h => Declared(if (a.state == "done") State.Done else State.More, h))
            .toRight(ArgsError.Missing("handoff", handoff.accepts))
        )
    )
  }

  /** What the model is offered of it, its arguments held to the schema when `strict`. */
  def schema(strict: Boolean): ToolSchema = Spec.schema(strict)

  /** The calls to it in `reply`, in order. */
  def calls(reply: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    reply.blocks.collect { case c: AssistantBlock.ToolCall if c.name == ToolName.value(Name) => c }

  /** What the first call to it in `reply` declares, when its arguments read. */
  def of(reply: Message.Assistant): Option[Declared] =
    calls(reply).headOption.flatMap(c => Spec.args.read(c.arguments).toOption)

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
