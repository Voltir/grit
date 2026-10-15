package grit.turn

import grit.core.allot.Allotment
import grit.core.message.{AssistantBlock, Message, StopReason}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.tool.Outcome

/** What a turn's tool loop decides: the model is called, the tools its reply calls are
  * settled, and it is called again, until a reply calls no tool or the budget's last call, made
  * with tools off. Each call sees the turn's own messages so far, after its window. The turn's
  * rounds make its steps ([[TurnRound]]).
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

    /** Two calls: the fewest a turn makes, the first with tools on. */
    val Fewest: Budget = Budget(2)

    /** `default` as `calls` lays over it: `Scaled(n)` is `n` times its calls. */
    def allotted(default: Budget, calls: Allotment.Calls): Budget = calls match {
      case Allotment.Calls.Default => default
      // Scaled is at least once by construction, so this is at least `default`, at least 2.
      case Allotment.Calls.Scaled(times) => Budget(default.calls * times)
      case Allotment.Calls.Fewest => Fewest
    }
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

  /** `request` as `use` says to send it: unchanged when `use` is [[ToolUse.Auto]] or
    * [[ToolUse.Required]]; when it is [[ToolUse.Off]], with [[LastCall]] after its messages,
    * as a user message, or, under
    * [[AfterToolResult.InLastResult]] when its last message is a tool result, at the end of
    * that result's content, after a blank line.
    */
  def told(use: ToolUse, request: ModelRequest, after: AfterToolResult): ModelRequest =
    use match {
      case ToolUse.Auto | ToolUse.Required => request
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

}
