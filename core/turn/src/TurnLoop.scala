package grit.turn

import scala.annotation.tailrec

import grit.core.message.{AssistantBlock, Message, StopReason}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.tool.Outcome

/** A turn's tool loop: the model is called, the tools its reply calls are settled, and it is
  * called again, until a reply calls no tool or the budget's last call, made with tools off.
  * Each call sees the turn's own messages so far, after its window. Pure, but for [[run]],
  * whose effects the turn makes through [[Moves]].
  */
object TurnLoop {

  /** How many model calls one turn may make. The last is made with tools off, so a turn
    * that keeps calling tools still ends in text.
    */
  final case class Budget private (calls: Int)

  object Budget {

    /** `calls` as a budget; `Left` when it is under 2, which leaves no call with tools on. */
    def of(calls: Int): Either[String, Budget] =
      if (calls >= 2) Right(Budget(calls))
      else Left(s"a turn's budget is at least 2 model calls, not $calls")
  }

  /** Which model call of a turn this is, counting from 0. */
  opaque type Round = Int

  object Round {

    val First: Round = 0

    /** The round of `index`, from 0; `First` for a negative one. */
    private[turn] def at(index: Int): Round = math.max(0, index)

    extension (round: Round) {
      def index: Int = round

      def next: Round = round + 1

      /** The name of this round's model-call step: `call-model` for the first, as a turn
        * with no tools has always named it, and `call-model:n` for the n-th after.
        */
      def step: String = if (round == 0) "call-model" else s"call-model:$round"
    }
  }

  /** One tool call of a reply, and how it is settled. */
  enum Pending {

    /** Run the call, asking a person first when its tool is gated. */
    case Run(call: AssistantBlock.ToolCall)

    /** Answer the call with `outcome`, without running it. */
    case Refused(call: AssistantBlock.ToolCall, outcome: Outcome)

    def call: AssistantBlock.ToolCall
  }

  /** What the loop does after a reply. */
  enum Next {

    /** `reply` answers the turn: it has text that is not blank, and holds no tool call
      * (those of a reply to the budget's last call are dropped).
      */
    case Answer(reply: Message.Assistant)

    /** `reply` says nothing, only blank text or reasoning, and calls no tool (or only
      * dropped ones): the round failed.
      */
    case Silent(reply: Message.Assistant)

    /** Settle `calls`, in order, then make the call `round`, its tools on or off as [[use]]
      * says.
      */
    case Settle(calls: Vector[Pending], round: Round)
  }

  /** What `CutOff` calls are answered: a reply cut off at max tokens may hold a call whose
    * arguments were cut short.
    */
  val CutOff: Outcome = Outcome.Failed(
    "Your reply was cut off at the output token limit before this call was complete, so it " +
      "was not run. Make the call again with shorter arguments, or split the work."
  )

  /** What the loop does after `reply`, the reply to `round`. A reply that calls no tool, or
    * any reply to the budget's last call (or a later one), answers, or is silent when no
    * text that is not blank is left once its calls are dropped. Otherwise its calls are
    * settled, each run, or each refused with [[CutOff]] when the reply stopped at max
    * tokens; then the next call is made.
    */
  def next(budget: Budget, round: Round, reply: Message.Assistant): Next = {
    val calls = reply.blocks.collect { case c: AssistantBlock.ToolCall => c }
    val last = budget.calls - 1
    if (calls.isEmpty || round >= last) {
      val answer: Message.Assistant = reply.copy(blocks = reply.blocks.filter {
        case AssistantBlock.ToolCall(_, _, _) => false
        case _ => true
      })
      val said = answer.blocks.exists {
        case AssistantBlock.Text(text) => text.trim.nonEmpty
        case _ => false
      }
      if (said) Next.Answer(answer) else Next.Silent(answer)
    } else {
      val pending =
        if (reply.stop == StopReason.MaxTokens) calls.map(Pending.Refused(_, CutOff))
        else calls.map(Pending.Run(_))
      Next.Settle(pending, round + 1)
    }
  }

  /** Whether `round`'s call offers tools the model may call: [[ToolUse.Off]] for the
    * budget's last call and any after it, [[ToolUse.Auto]] before.
    */
  def use(budget: Budget, round: Round): ToolUse =
    if (round >= budget.calls - 1) ToolUse.Off else ToolUse.Auto

  /** What the model is told on a call made with tools off, after everything else it is
    * sent ([[told]]).
    */
  val LastCall: String =
    "[grit: this is your last call in this turn, and it has no tools: call none. Answer " +
      "now from what you have found so far: say what you found, and plainly what you did " +
      "not find or could not check. \"I could not find it\" is an answer; do not fill the " +
      "gap with a guess.]"

  /** `request` as `use` says to send it: unchanged when `use` is [[ToolUse.Auto]]; when it is
    * [[ToolUse.Off]], with [[LastCall]] after its messages, as a user message, or, under
    * [[AfterToolResult.InLastResult]] when its last message is a tool result, at the end of
    * that result's content, after a blank line.
    */
  def told(use: ToolUse, request: ModelRequest, after: AfterToolResult): ModelRequest =
    use match {
      case ToolUse.Auto => request
      case ToolUse.Off =>
        (after, request.messages.lastOption) match {
          case (AfterToolResult.InLastResult, Some(r: Message.ToolResult)) =>
            request.copy(messages =
              request.messages.init :+ r.copy(content = s"${r.content}\n\n$LastCall")
            )
          case _ => request.copy(messages = request.messages :+ Message.User(LastCall))
        }
    }

  /** `request` with its tools named in its system prompt under
    * [[ToolGuidance.SystemLines]]: after a blank line, `Tools you may call:`, then a line per
    * tool, `- name: ` and the first line of its description. Unchanged under
    * [[ToolGuidance.SchemaOnly]], or when it offers none.
    */
  def guided(request: ModelRequest, guidance: ToolGuidance): ModelRequest = guidance match {
    case ToolGuidance.SchemaOnly => request
    case ToolGuidance.SystemLines if request.tools.isEmpty => request
    case ToolGuidance.SystemLines =>
      val lines = request.tools.map(t =>
        s"- ${t.name}: ${t.description.linesIterator.nextOption().getOrElse("")}"
      )
      request.copy(system = (s"${request.system}\n\nTools you may call:" +: lines).mkString("\n"))
  }

  /** The effects the loop needs. The turn makes each a durable step. */
  trait Moves {

    /** The model's reply on `round`, offered the tools as `use` says, shown its window and
      * then the turn's own messages so far: every earlier reply that called tools, each
      * followed by its calls' results, as [[record]] and [[settle]] kept them; then, as
      * [[told]] says, [[LastCall]].
      */
    def call(round: Round, use: ToolUse): Either[TurnFailure, Message.Assistant]

    /** Keeps `reply`, the reply to `round` that called tools, before any call is settled;
      * `calls` are its calls, as they will be settled.
      */
    def record(
        round: Round,
        reply: Message.Assistant,
        calls: Vector[Pending]
    ): Either[TurnFailure, Unit]

    /** Settles `pending`, the call at `index` (from 0) of `round`'s reply, and keeps its
      * result, paired with its call's id, after those of the calls before it. Only a store
      * that cannot be written fails: every failure of the tool is an outcome the model reads.
      */
    def settle(round: Round, index: Int, pending: Pending): Either[TurnFailure, Unit]
  }

  /** What a loop came to: its `answer`, the reply to `round`, the loop's last call; and
    * `replies`, the reply to every round as the model sent it, the first first.
    */
  final case class Looped(
      answer: Message.Assistant,
      round: Round,
      replies: Vector[Message.Assistant]
  )

  /** Calls the model through `moves` from the first round, then goes on as [[from]] does. */
  def run(budget: Budget, moves: Moves^): Either[TurnFailure, Looped] =
    moves.call(Round.First, use(budget, Round.First)).flatMap(from(budget, _, moves))

  /** Goes on after `first`, the reply to the first round's call: settles each reply's tool
    * calls through `moves`, in order, and calls the model again, until [[next]] answers.
    * Fails when a move does, making no move after it, and with [[TurnFailure.Model]] when a
    * reply is silent.
    */
  def from(budget: Budget, first: Message.Assistant, moves: Moves^): Either[TurnFailure, Looped] = {
    @tailrec
    def loop(
        round: Round,
        reply: Message.Assistant,
        before: Vector[Message.Assistant]
    ): Either[TurnFailure, Looped] =
      next(budget, round, reply) match {
        case Next.Answer(answer) => Right(Looped(answer, round, before :+ reply))
        case Next.Silent(_) =>
          Left(
            TurnFailure.Model(s"the reply to ${Round.step(round)} said nothing and called no tool")
          )
        case Next.Settle(calls, after) =>
          settled(round, reply, calls).flatMap(_ => moves.call(after, use(budget, after))) match {
            case Left(failure) => Left(failure)
            case Right(again) => loop(after, again, before :+ reply)
          }
      }

    def settled(
        round: Round,
        reply: Message.Assistant,
        calls: Vector[Pending]
    ): Either[TurnFailure, Unit] =
      moves.record(round, reply, calls).flatMap { _ =>
        calls.zipWithIndex.foldLeft[Either[TurnFailure, Unit]](Right(())) {
          case (acc, (pending, i)) => acc.flatMap(_ => moves.settle(round, i, pending))
        }
      }

    loop(Round.First, first, Vector.empty)
  }
}
