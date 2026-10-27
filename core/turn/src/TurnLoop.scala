package grit.turn

import grit.core.allot.Allotment
import grit.core.message.{AssistantBlock, Cut, Message, StopReason}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.prompt.{Fenced, Stripped, Tag}
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
  final case class Rounds private (calls: Int)

  object Rounds {

    /** `calls` as a budget; `Left` when it is under 2, which leaves no call with tools on. */
    def of(calls: Int): Either[String, Rounds] =
      if (calls >= 2) Right(Rounds(calls))
      else Left(s"a turn's budget is at least 2 model calls, not $calls")

    /** Two calls: the fewest a turn makes, the first with tools on. */
    val Fewest: Rounds = Rounds(2)

    /** `default` as `calls` lays over it: `Scaled(n)` is `n` times its calls. */
    def allotted(default: Rounds, calls: Allotment.Calls): Rounds = calls match {
      case Allotment.Calls.Default => default
      // Scaled is at least once by construction, so this is at least `default`, at least 2.
      case Allotment.Calls.Scaled(times) => Rounds(default.calls * times)
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
  def next(budget: Rounds, round: Round, reply: Message.Assistant): Next = {
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
  def use(budget: Rounds, round: Round): ToolUse =
    if (round >= budget.calls - 1) ToolUse.Off else ToolUse.Auto

  /** What the model is told on a call made with tools off, after everything else it is
    * sent ([[told]]).
    */
  val LastCall: String =
    "[grit: this is your last call in this turn, and it has no tools: call none. Answer " +
      "now from what you have found so far: say what you found, and plainly what you did " +
      "not find or could not check. \"I could not find it\" is an answer; do not fill the " +
      "gap with a guess.]"

  /** `request` as `use` and `after` say to send it: when `use` is [[ToolUse.Off]], with
    * [[LastCall]] after its messages, as a user message; then, under
    * [[AfterToolResult.InLastResult]], each user message right after a tool result moved to the
    * end of that result's content, after a blank line (a declined call's reason, the note).
    */
  def told(use: ToolUse, request: ModelRequest, after: AfterToolResult): ModelRequest = {
    val noted = use match {
      case ToolUse.Auto | ToolUse.Required => request.messages
      case ToolUse.Off => request.messages :+ Message.User(LastCall)
    }
    val sent = after match {
      case AfterToolResult.UserMessage => noted
      case AfterToolResult.InLastResult =>
        noted.foldLeft(Vector.empty[Message]) {
          case (init :+ (r: Message.ToolResult), Message.User(text)) =>
            init :+ r.copy(content = s"${r.content}\n\n$text")
          case (out, m) => out :+ m
        }
    }
    request.copy(messages = sent)
  }

  /** 1,000: the most characters of a tool's description a model is offered, after
    * [[Stripped.of]], cut at a word ([[Cut.head]]).
    */
  val DescribedChars: Int = 1000

  /** `request` as a model is offered its tools: each tool's description [[Stripped.of]], then
    * cut at a word to [[DescribedChars]]; and under [[ToolGuidance.SystemLines]], when it offers
    * any, its tools named in its system prompt: after a blank line, `Tools you may call:`, then
    * the tools fenced in `<grit_tool_descriptions>` ([[Fenced]]), a line per tool, `- name: `
    * and the first line of that description, trimmed.
    */
  def guided(request: ModelRequest, guidance: ToolGuidance): ModelRequest = {
    val offered = request.copy(tools =
      request.tools.map(t =>
        t.copy(description = Cut.head(Stripped.of(t.description), DescribedChars))
      )
    )
    guidance match {
      case ToolGuidance.SchemaOnly => offered
      case ToolGuidance.SystemLines if offered.tools.isEmpty => offered
      case ToolGuidance.SystemLines =>
        val lines = offered.tools.map(t =>
          Fenced.Part.Material(
            s"- ${t.name}: ${t.description.trim.linesIterator.nextOption().getOrElse("")}"
          )
        )
        val listed = Fenced(Tag.ToolDescriptions, Vector.empty, lines).rendered
        offered.copy(system = s"${offered.system}\n\nTools you may call:\n$listed")
    }
  }

}
