package grit.turn

import grit.core.allot.Allotment
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.prompt.{Fenced, Stripped, Tag}
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.tool.{Outcome, ToolName}

/** What a turn's tool loop decides: the model is called, the tools its reply calls are
  * settled, and it is called again, until a reply calls no tool or the budget's last call, made
  * with tools off. Each call sees the turn's own messages so far, after its window. The turn's
  * rounds make its steps ([[TurnRound]]).
  */
object TurnLoop {

  /** How far one turn's loop may go: `calls` model calls, the last made with tools off, so a
    * turn that keeps calling tools still ends in text; and, with `limit`, the input a request
    * may reach as its provider counts it, less room for one round's results the turn has not
    * yet seen: once a reply's input plus the largest growth of a round so far meets `limit`, the
    * call after it is the last with tools and the one after that the last ([[after]]). A turn
    * can still pass `limit` when one round grows more than every round before it and the room
    * kept; a provider that reports no input never meets it. `cut`: whether `limit` cut its
    * calls short so ([[after]]).
    */
  final case class Rounds private (calls: Int, limit: Option[Tokens], cut: Boolean = false) {

    /** This budget, its requests' input bounded by `limit`; `Left` when it is not above 0. */
    def within(limit: Tokens): Either[String, Rounds] =
      if (Tokens.value(limit) > 0) Right(copy(limit = Some(limit)))
      else Left(s"a turn's input limit is above 0 tokens, not ${Tokens.value(limit)}")

    /** This budget once a turn's replies so far are `replies`, the first first: its calls cut
      * to three past the first reply whose input, plus the largest growth of input from one
      * reply's request to the next's up to it, meets `limit`, when that is fewer (that reply's
      * round, then the last call with tools, then the last); else unchanged.
      */
    def after(replies: Vector[Message.Assistant]): Rounds =
      limit.fold(this) { bound =>
        Rounds
          .reach(replies.map(_.usage.input))
          .indexWhere(r => Tokens.value(r) >= Tokens.value(bound)) match {
          case grown if grown >= 0 && grown + 3 < calls => copy(calls = grown + 3, cut = true)
          case _ => this
        }
      }
  }

  object Rounds {

    /** `calls` as a budget, its input unbounded; `Left` when it is under 2, which leaves no
      * call with tools on.
      */
    def of(calls: Int): Either[String, Rounds] =
      if (calls >= 2) Right(Rounds(calls, None))
      else Left(s"a turn's budget is at least 2 model calls, not $calls")

    /** Each of `inputs`, a turn's requests' inputs as counted, the first first, plus the
      * largest growth from one request's input to the next's up to it (none for the first):
      * what [[Rounds.after]] holds against the limit.
      */
    def reach(inputs: Vector[Tokens]): Vector[Tokens] = {
      val counted = inputs.map(Tokens.value)
      val growths = 0L +: counted.zip(counted.drop(1)).map((from, to) => to - from)
      counted
        .zip(growths.scanLeft(0L)(math.max).drop(1))
        .map((input, growth) => Tokens(input + growth))
    }

    /** Two calls: the fewest a turn makes, the first with tools on. */
    val Fewest: Rounds = Rounds(2, None)

    /** `default` as `calls` lays over it: `Scaled(n)` is `n` times its calls; its input limit
      * kept.
      */
    def allotted(default: Rounds, calls: Allotment.Calls): Rounds = calls match {
      case Allotment.Calls.Default => default
      // Scaled is at least once by construction, so this is at least `default`, at least 2.
      case Allotment.Calls.Scaled(times) => default.copy(calls = default.calls * times)
      case Allotment.Calls.Fewest => Fewest.copy(limit = default.limit)
    }
  }

  /** The input a turn's requests are held to ([[Rounds.limit]]): `window`, its assembler's limit,
    * plus `prefix`, what its system prompt and tools cost; capped, when the pin's
    * `contextLength` is known, by that less `maxTokens`, the pin's output budget; less
    * `clipped`, room for one clipped tool result the loop has not yet seen. Never below one
    * token.
    */
  def inputLimit(
      window: Tokens,
      prefix: Tokens,
      contextLength: Option[Tokens],
      maxTokens: Int,
      clipped: Tokens
  ): Tokens = {
    val drawn = Tokens.value(window) + Tokens.value(prefix)
    val held = contextLength.fold(drawn)(l => math.min(drawn, Tokens.value(l) - maxTokens))
    Tokens(math.max(1L, held - Tokens.value(clipped)))
  }

  /** The notes the loop's calls up to `round` add, as a gate's are ([[TurnShown.loopShape]]):
    * [[LastTools]] at `budget`'s last call with tools, once `round` has reached it, unless that
    * is the turn's first call, or [[LimitTools]] when its limit cut it; else none.
    */
  def notes(budget: Rounds, round: Round): Vector[(Round, String)] = {
    val lastTools = budget.calls - 2
    val note = if (budget.cut) LimitTools else LastTools
    Vector(lastTools -> note).filter(_ => lastTools > 0 && round >= lastTools)
  }

  /** What the model is told on the last call with tools. */
  val LastTools: String = Fenced.noted(
    Vector(
      "This is your last call with tools in this turn; the next has none. Stop reading: " +
        "make the changes the work needs now, or answer, and say plainly what you did not get to."
    )
  )

  /** What the model is told on the last call with tools of a turn its limit cut
    * ([[Rounds.cut]]), in place of [[LastTools]].
    */
  val LimitTools: String = Fenced.noted(
    Vector(
      "This turn is ending: this is your last call with tools, and the next has none. Make " +
        "the changes the work needs now. When nothing is left to change, call task_status " +
        "alone: done with your answer if the task is finished, or more with what the next " +
        "turn needs."
    )
  )

  /** What the model is told on the last call of a turn its limit cut, in place of
    * [[LastCall]].
    */
  val Ending: String = Fenced.noted(
    Vector(
      "This turn is ending, and this is its last call. Call task_status now: done with your " +
        "answer if the task is finished, or more with what the next turn needs: what you did, " +
        "what is left, and where things are. If you cannot call it, answer in text with what " +
        "the next turn needs."
    )
  )

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

    /** `reply` stopped at its output budget with no text that is not blank and no tool call:
      * its thinking used the whole budget. The round failed, and nothing asks again.
      */
    case OutOfOutput(reply: Message.Assistant)

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

  /** What the loop does after `reply`, the reply to `round`. A reply that [[declared]] through
    * [[TaskStatus]] answers with its handoff as its text, its calls dropped. A reply that calls no tool, or
    * any reply to the budget's last call (or a later one), answers, or is silent when no
    * text that is not blank is left once its calls are dropped; out of output instead when
    * it stopped at max tokens with no tool call at all. Otherwise its calls are
    * settled, each run, or each refused with [[CutOff]] when the reply stopped at max
    * tokens, and a call to [[TaskStatus]] where it is offered refused with
    * [[TaskStatus.Unrecorded]]; then the next call is made.
    */
  def next(budget: Rounds, round: Round, reply: Message.Assistant): Next = {
    val calls = reply.blocks.collect { case c: AssistantBlock.ToolCall => c }
    val last = budget.calls - 1
    val status = declared(budget, round, reply)
    if (status.isDefined) {
      // Its call becomes the reply's text: no tool call reaches the conversation.
      val kept = reply.blocks.filter(_.isInstanceOf[AssistantBlock.Reasoning])
      Next.Answer(reply.copy(blocks = kept ++ status.map(d => AssistantBlock.Text(d.handoff))))
    } else if (calls.isEmpty || round >= last) {
      val answer: Message.Assistant = reply.copy(blocks = reply.blocks.filter {
        case AssistantBlock.ToolCall(_, _, _) => false
        case _ => true
      })
      val said = answer.blocks.exists {
        case AssistantBlock.Text(text) => text.trim.nonEmpty
        case _ => false
      }
      if (said) Next.Answer(answer)
      else if (reply.stop == StopReason.MaxTokens && calls.isEmpty) Next.OutOfOutput(answer)
      else Next.Silent(answer)
    } else {
      val offersStatus = asksStatus(budget, round)
      val pending =
        if (reply.stop == StopReason.MaxTokens) calls.map(Pending.Refused(_, CutOff))
        else
          calls.map(c =>
            // Offered, it is never run: beside other calls, or unreadable, it is refused.
            if (offersStatus && c.name == ToolName.value(TaskStatus.Name))
              Pending.Refused(c, Outcome.Failed(TaskStatus.Unrecorded))
            else Pending.Run(c)
          )
      Next.Settle(pending, round + 1)
    }
  }

  /** `schemas`, the tools `round`'s call offers, with [[TaskStatus]] after them, its schema
    * strict when `strict`, on a budget its limit cut ([[Rounds.cut]]) from its last call with
    * tools on.
    */
  def offered(
      budget: Rounds,
      round: Round,
      schemas: Vector[ToolSchema],
      strict: Boolean
  ): Vector[ToolSchema] =
    if (asksStatus(budget, round)) schemas :+ TaskStatus.schema(strict) else schemas

  /** What `reply`, the reply to `round`, declares through [[TaskStatus]] when that ends the
    * turn: on a budget its limit cut, from its last call with tools on, a reply whose readable
    * calls to it are its only calls, or any reply to the last call that calls it; else `None`.
    */
  def declared(
      budget: Rounds,
      round: Round,
      reply: Message.Assistant
  ): Option[TaskStatus.Declared] = {
    val calls = reply.blocks.count(_.isInstanceOf[AssistantBlock.ToolCall])
    val status = TaskStatus.calls(reply).size
    Option
      .when(
        asksStatus(budget, round) && status > 0 && (status == calls || round >= budget.calls - 1)
      )(
        TaskStatus.of(reply)
      )
      .flatten
  }

  /** Whether `round` of `budget` offers [[TaskStatus]]: its limit cut it, and this is its last
    * call with tools or after.
    */
  private def asksStatus(budget: Rounds, round: Round): Boolean =
    budget.cut && round >= budget.calls - 2

  /** Whether `round`'s call offers tools the model may call: [[ToolUse.Auto]] before the
    * budget's last call; from it, [[ToolUse.Off]], or on a budget its limit cut
    * ([[Rounds.cut]]) [[TaskStatus]] alone.
    */
  def use(budget: Rounds, round: Round): ToolUse =
    if (round < budget.calls - 1) ToolUse.Auto
    else if (budget.cut) ToolUse.Only(ToolName.value(TaskStatus.Name))
    else ToolUse.Off

  /** What the model is told on a call made with tools off, after everything else it is
    * sent ([[told]]).
    */
  val LastCall: String = Fenced.noted(
    Vector(
      "This is your last call in this turn, and it has no tools: call none. Answer " +
        "now from what you have found so far: say what you found, and plainly what you did " +
        "not find or could not check. \"I could not find it\" is an answer; do not fill the " +
        "gap with a guess."
    )
  )

  /** `request` as `use` and `after` say to send it: when `use` is [[ToolUse.Off]], with
    * [[LastCall]] after its messages, as a user message, and when it is [[ToolUse.Only]], with
    * [[Ending]]; then, under
    * [[AfterToolResult.InLastResult]], each user message right after a tool result moved to the
    * end of that result's content, after a blank line (a declined call's reason, the note).
    */
  def told(use: ToolUse, request: ModelRequest, after: AfterToolResult): ModelRequest = {
    val noted = use match {
      case ToolUse.Auto | ToolUse.Required => request.messages
      case ToolUse.Off => request.messages :+ Message.User(LastCall)
      case ToolUse.Only(_) => request.messages :+ Message.User(Ending)
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

  /** `request` as a model is offered its tools: each tool's description [[Stripped.of]], whole;
    * and under [[ToolGuidance.SystemLines]], when it offers
    * any, its tools named in its system prompt: after a blank line, `Tools you may call:`, then
    * the tools fenced in `<grit_tool_descriptions>` ([[Fenced]]), a line per tool, `- name: `
    * and the first line of that description, trimmed.
    */
  def guided(request: ModelRequest, guidance: ToolGuidance): ModelRequest = {
    val offered =
      request.copy(tools = request.tools.map(t => t.copy(description = Stripped.of(t.description))))
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
