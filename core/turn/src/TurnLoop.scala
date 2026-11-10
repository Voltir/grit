package grit.turn

import grit.core.allot.Allotment
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.prompt.{Fenced, Stripped, Tag}
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.core.recipe.Briefing
import grit.core.tool.{Outcome, ToolName}

/** What a turn's tool loop decides: the model is called, the tools its reply calls are
  * settled, and it is called again, until a reply calls no tool or the budget's last call, told
  * to call none. Every call of a turn offers the same tools the same way; what a call may not
  * call is refused here, never withdrawn from its request. Each call sees the turn's own
  * messages so far, after its window. The turn's rounds make its steps ([[TurnRound]]).
  */
object TurnLoop {

  /** How far one turn's loop may go: `calls` model calls, the last told to call no tool (or, on
    * a turn briefed for one task, task_status alone), so a turn that keeps calling tools still
    * ends: a reply to the last call that calls a tool is refused, and that call made once more,
    * so a turn makes at most `calls + 1`, and a second such reply that says nothing ends the
    * turn silent ([[next]]); and, with `limit`, the input a request may reach as its provider
    * counts it, less room for one round's results the turn has not yet seen: once a reply's
    * input plus the largest growth of a round so far meets `limit`, the call after it is the
    * last with tools and the one after that the last ([[after]]). A turn can still pass `limit`
    * when one round grows more than every round before it and the room kept. Under a provider
    * that reports no input, `limit` is never met, and `calls` alone ends the turn.
    * `endsAtLimit`: whether its last two calls end its turn at its limit, its task going on in a
    * new turn unless it declares done ([[asksStatus]]). Every budget's do; one `record-window`
    * recorded before that rule (ADR 0004) did only once `limit` cut its calls ([[after]]).
    */
  final case class Rounds private (calls: Int, limit: Option[Tokens], endsAtLimit: Boolean) {

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
          case grown if grown >= 0 && grown + 3 < calls =>
            copy(calls = grown + 3, endsAtLimit = true)
          case _ => this
        }
      }
  }

  object Rounds {

    /** `calls` as a budget, its input unbounded, its last two calls ending its turn at its
      * limit; `Left` when it is under 2, which leaves no call with tools on.
      */
    def of(calls: Int): Either[String, Rounds] =
      if (calls >= 2) Right(Rounds(calls, None, endsAtLimit = true))
      else Left(s"a turn's budget is at least 2 model calls, not $calls")

    /** A budget as `record-window` recorded it: `endsAtLimit` false for one recorded before a
      * budget's last calls ended its turn at its limit. `Left` as [[of]] and [[Rounds.within]]
      * refuse.
      */
    private[turn] def recorded(
        calls: Int,
        limit: Option[Tokens],
        endsAtLimit: Boolean
    ): Either[String, Rounds] =
      for {
        counted <- of(calls)
        bounded <- limit.fold(Right(counted))(counted.within)
      } yield bounded.copy(endsAtLimit = endsAtLimit)

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
    val Fewest: Rounds = Rounds(2, None, endsAtLimit = true)

    /** `default` as `calls` lays over it: `Scaled(n)` is `n` times its calls; its input limit
      * kept.
      */
    def allotted(default: Rounds, calls: Allotment.Calls): Rounds = calls match {
      case Allotment.Calls.Default => default
      // Scaled is at least once by construction, so this is at least `default`, at least 2.
      case Allotment.Calls.Scaled(times) => default.copy(calls = default.calls * times)
      case Allotment.Calls.Fewest =>
        Fewest.copy(limit = default.limit, endsAtLimit = default.endsAtLimit)
    }
  }

  /** The input a turn's requests are held to ([[Rounds.limit]]): the pin's `contextLength` less
    * `maxTokens`, its output budget, and `clipped`, room for one clipped tool result the loop
    * has not yet seen; at most `own`, the deployment's own limit ([[Rounds.within]]), when it
    * sets one. For a pin whose context length no profile gives, `drawn`, its assembler's limit
    * plus what its system prompt and tools cost, stands in for it, `clipped` taken from it
    * alone. Never below one token. With the context known, the window bounds only what a
    * boundary draws (ADR 0052), not how far the turn grows.
    */
  def inputLimit(
      own: Option[Tokens],
      contextLength: Option[Tokens],
      maxTokens: Int,
      clipped: Tokens,
      drawn: Tokens
  ): Tokens = {
    val held = contextLength.fold(Tokens.value(drawn))(l => Tokens.value(l) - maxTokens)
    val roomed = math.max(1L, held - Tokens.value(clipped))
    Tokens(own.fold(roomed)(o => math.min(Tokens.value(o), roomed)))
  }

  /** The notes the loop's calls up to `round` add, each at the round it is first sent, as a
    * gate's are ([[TurnShown.loopShape]]): at `budget`'s last call with tools, unless that is the
    * turn's first, [[LimitTools]] when its last calls end it at its limit
    * ([[Rounds.endsAtLimit]]) on a turn `briefing` says is for one task, else [[LastTools]]; at
    * its last call, [[Ending]] or [[LastCall]] alike.
    */
  def notes(budget: Rounds, briefing: Briefing, round: Round): Vector[(Round, String)] = {
    val lastTools = budget.calls - 2
    val last = budget.calls - 1
    val ending = budget.endsAtLimit && declares(briefing)
    Vector(
      lastTools -> (if (ending) LimitTools else LastTools),
      last -> (if (ending) Ending else LastCall)
    ).filter((at, _) => at > 0 && round >= at)
  }

  /** What the model is told on the last call with tools. */
  val LastTools: String = Fenced.noted(
    Vector(
      "This is your last call with tools in this turn; the next may call none. Stop reading: " +
        "make the changes the work needs now, or answer, and say plainly what you did not get to."
    )
  )

  /** What the model is told on the last call with tools of a turn briefed for one task whose
    * last calls end it at its limit ([[Rounds.endsAtLimit]]), in place of [[LastTools]].
    */
  val LimitTools: String = Fenced.noted(
    Vector(
      "This turn is ending: this is your last call with tools, and the next may call " +
        "task_status alone. Make " +
        "the changes the work needs now. When nothing is left to change, call task_status " +
        "alone: done with your answer if the task is finished, or more with what the next " +
        "turn needs."
    )
  )

  /** What the model is told on the last call of a turn briefed for one task whose last calls
    * end it at its limit, in place of [[LastCall]].
    */
  val Ending: String = Fenced.noted(
    Vector(
      "This turn is ending, and this is its last call. Call task_status now, and no other " +
        "tool: done with your " +
        "answer if the task is finished, or more with what the next turn needs. It sees the " +
        "task and this handoff, none of your earlier replies, so say what is done so far, " +
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

    /** The turn ends as `ending` says. */
    case Ends(ending: Ended)

    /** Settle `calls`, in order, then make the call `round`. */
    case Settle(calls: Vector[Pending], round: Round)

    /** Settle `calls`, each refused, then make the budget's last call once more, as `round`;
      * a history recorded before the patch `last-call-refused` ends as `before` instead
      * (ADR 0004).
      */
    case Again(calls: Vector[Pending], round: Round, before: Ended)
  }

  /** How a reply ends its turn: one round's reply, as the loop reads it. How the whole turn
    * ended, as its outcome says it, is [[grit.core.inbox.TurnEnding]].
    */
  enum Ended {

    /** `reply` answers the turn: it has text that is not blank, and holds no tool call
      * (those of a reply to the budget's last call are dropped).
      */
    case Answer(reply: Message.Assistant)

    /** `reply` says nothing, only blank text or reasoning, and calls no tool (or only
      * dropped ones): the round failed.
      */
    case Silent(reply: Message.Assistant)

    /** `reply` stopped at its output budget with no text that is not blank and no tool call:
      * its thinking used the whole budget. The round failed, as [[TurnFailure.OutOfOutput]],
      * and nothing asks again.
      */
    case OutOfOutput(reply: Message.Assistant)
  }

  /** How `reply` ends its turn as a reply that calls no tool, or whose calls are dropped:
    * answers, its calls dropped; silent when no text that is not blank is left; out of output
    * when it stopped at max tokens with no tool call at all.
    */
  def ended(reply: Message.Assistant): Ended = {
    val answer: Message.Assistant = reply.copy(blocks = reply.blocks.filter {
      case AssistantBlock.ToolCall(_, _, _) => false
      case _ => true
    })
    val said = answer.blocks.exists {
      case AssistantBlock.Text(text) => text.trim.nonEmpty
      case _ => false
    }
    val called = reply.blocks.exists(_.isInstanceOf[AssistantBlock.ToolCall])
    if (said) Ended.Answer(answer)
    else if (reply.stop == StopReason.MaxTokens && !called) Ended.OutOfOutput(answer)
    else Ended.Silent(answer)
  }

  /** What `CutOff` calls are answered: a reply cut off at max tokens may hold a call whose
    * arguments were cut short.
    */
  val CutOff: Outcome = Outcome.Failed(
    "Your reply was cut off at the output token limit before this call was complete, so it " +
      "was not run. Make the call again with shorter arguments, or split the work."
  )

  /** What a call on the last call of a turn told [[Ending]] is answered, unless it declares. */
  val EndingRefused: Outcome = Outcome.Failed(
    "Not run: this turn is ending, and its last call may call task_status alone. Call it " +
      "now: done with your answer if the task is finished, or more with what the next turn " +
      "needs."
  )

  /** What a call on the last call of a turn told [[LastCall]] is answered. */
  val LastRefused: Outcome = Outcome.Failed(
    "Not run: this is your last call in this turn, and it may call no tool. Answer in text " +
      "now from what you have found so far, and say plainly what you did not get to."
  )

  /** What the loop does after `reply`, the reply to `round` of a turn briefed as `briefing`. A
    * reply that [[declared]] through [[TaskStatus]] answers with its handoff as its text, its
    * calls dropped. A reply that calls no tool, or any reply after the budget's last call (its
    * one retry), ends as [[ended]] says. A reply to the budget's last call that calls a tool:
    * [[Next.Again]], each call refused with [[CutOff]] when the reply stopped at max tokens,
    * else with [[EndingRefused]] on a turn told [[Ending]], else [[LastRefused]]. Otherwise its
    * calls are settled, each run, or each refused with [[CutOff]] when the reply stopped at max
    * tokens; a call to [[TaskStatus]] on a turn briefed for one task refused with
    * [[TaskStatus.Early]] before the cut's last call with tools, and with
    * [[TaskStatus.Unrecorded]] from it; then the next call is made.
    */
  def next(budget: Rounds, briefing: Briefing, round: Round, reply: Message.Assistant): Next = {
    val calls = reply.blocks.collect { case c: AssistantBlock.ToolCall => c }
    val last = budget.calls - 1
    val status = declared(budget, briefing, round, reply)
    if (status.isDefined) {
      // Its call becomes the reply's text: no tool call reaches the conversation.
      val kept = reply.blocks.filter(_.isInstanceOf[AssistantBlock.Reasoning])
      Next.Ends(
        Ended.Answer(reply.copy(blocks = kept ++ status.map(d => AssistantBlock.Text(d.handoff))))
      )
    } else if (calls.isEmpty || round > last) Next.Ends(ended(reply))
    else if (round == last) {
      val refusal =
        if (reply.stop == StopReason.MaxTokens) CutOff
        else if (budget.endsAtLimit && declares(briefing)) EndingRefused
        else LastRefused
      Next.Again(calls.map(Pending.Refused(_, refusal)), round + 1, ended(reply))
    } else {
      val ending = asksStatus(budget, round)
      val pending =
        if (reply.stop == StopReason.MaxTokens) calls.map(Pending.Refused(_, CutOff))
        else
          calls.map(c =>
            // Offered, it is never run: early, beside other calls, or unreadable, it is refused.
            if (declares(briefing) && c.name == ToolName.value(TaskStatus.Name))
              Pending.Refused(
                c,
                Outcome.Failed(if (ending) TaskStatus.Unrecorded else TaskStatus.Early)
              )
            else Pending.Run(c)
          )
      Next.Settle(pending, round + 1)
    }
  }

  /** Whether a turn briefed as `briefing` is offered [[TaskStatus]]: one briefed for one task. */
  private[turn] def declares(briefing: Briefing): Boolean = briefing match {
    case Briefing.Tasked => true
    case Briefing.Conversing => false
  }

  /** `schemas`, the tools every call of a turn briefed as `briefing` offers, with
    * [[TaskStatus]] after them, its schema strict when `strict`, when `briefing` is
    * [[Briefing.Tasked]].
    */
  def offered(
      briefing: Briefing,
      schemas: Vector[ToolSchema],
      strict: Boolean
  ): Vector[ToolSchema] =
    if (declares(briefing)) schemas :+ TaskStatus.schema(strict) else schemas

  /** What `reply`, the reply to `round` of a turn briefed as `briefing`, declares through
    * [[TaskStatus]] when that ends the turn: on a turn briefed for one task, from its budget's
    * ending on ([[asksStatus]]), a reply whose readable calls to it are its only calls, or any
    * reply to the last call that calls it; else `None`.
    */
  def declared(
      budget: Rounds,
      briefing: Briefing,
      round: Round,
      reply: Message.Assistant
  ): Option[TaskStatus.Declared] = {
    val calls = reply.blocks.count(_.isInstanceOf[AssistantBlock.ToolCall])
    val status = TaskStatus.calls(reply).size
    Option
      .when(
        declares(briefing) && asksStatus(budget, round) && status > 0 &&
          (status == calls || round >= budget.calls - 1)
      )(
        TaskStatus.of(reply)
      )
      .flatten
  }

  /** Whether `round` of `budget` is its ending, from which a turn briefed for one task may end
    * through [[TaskStatus]]: its last calls end its turn at its limit ([[Rounds.endsAtLimit]]),
    * and this is its last call with tools or after, never the turn's first.
    */
  private[turn] def asksStatus(budget: Rounds, round: Round): Boolean =
    budget.endsAtLimit && round >= budget.calls - 2 && round > 0

  /** What the model is told on the budget's last call ([[notes]]). */
  val LastCall: String = Fenced.noted(
    Vector(
      "This is your last call in this turn: call no tool. Answer " +
        "now from what you have found so far: say what you found, and plainly what you did " +
        "not find or could not check. \"I could not find it\" is an answer; do not fill the " +
        "gap with a guess."
    )
  )

  /** `request` as `after` says to send it: under [[AfterToolResult.InLastResult]], each user
    * message right after a tool result moved to the end of that result's content, after a
    * blank line (a declined call's reason, a note).
    */
  def told(request: ModelRequest, after: AfterToolResult): ModelRequest = {
    val sent = after match {
      case AfterToolResult.UserMessage => request.messages
      case AfterToolResult.InLastResult =>
        request.messages.foldLeft(Vector.empty[Message]) {
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
