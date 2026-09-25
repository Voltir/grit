package grit.turn

import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}
import grit.core.topic.{Placement, TopicEvent, TopicId, Verdict, Weights}

/** The main model asked where a message went, when the classifier was unsure
  * ([[TurnTopics.Classification.uncertain]]). Its request carries the `topic` tool and a
  * one-line note on the user's message; a call to the tool is its verdict, answered
  * "noted", and the model is called again for the reply. At most one more round: if that
  * round calls a tool again and says nothing, or fails, a plain call with neither tool nor
  * exchange answers instead. The exchange stays in the journal and the verdict's record,
  * never among the conversation's messages. Pure, but for [[round]], whose calls the turn
  * makes through [[Calls]].
  */
object TurnVerdict {

  /** The tool's name. */
  val Name: ToolName = ToolName("topic")

  /** The `topic` tool, as offered when the classification is `c`: `about` is `current`,
    * `earlier` or `new`; `earlier` is one of `c`'s earlier topics by key, required when
    * `about` is `earlier`; `name` is a new topic's name. With no earlier topics there is no
    * `earlier` field, and `about` is `current` or `new`. A name read is trimmed; a blank one
    * is none.
    */
  def topic(c: TurnTopics.Classification): ToolSpec[Verdict] = {
    val name = Field.text("For new: a short name for the subject, at most four words.").optional
    def named(n: Option[String]): Option[String] = n.map(_.trim).filter(_.nonEmpty)
    val args = c.earlier.map(_.key) match {
      case first +: rest =>
        val earlier = Field.oneOf("For earlier: the earlier topic it goes back to.", first, rest*)
        Args
          .of(
            (
              about = Field.oneOf(
                "current: it carries on the current topic. earlier: it goes back to an " +
                  "earlier topic. new: a subject not discussed before.",
                "current",
                "earlier",
                "new"
              ),
              earlier = earlier.optional,
              name = name
            )
          )
          .refine(a =>
            a.about match {
              case "current" => Right(Verdict.Current)
              case "earlier" =>
                a.earlier
                  .map(Verdict.Earlier(_))
                  .toRight(ArgsError.Missing("earlier", earlier.accepts))
              case _ => Right(Verdict.New(named(a.name)))
            }
          )
      case _ =>
        Args
          .of(
            (
              about = Field.oneOf(
                "current: it carries on the current topic. new: a subject not discussed before.",
                "current",
                "new"
              ),
              name = name
            )
          )
          .map(a => if (a.about == "current") Verdict.Current else Verdict.New(named(a.name)))
    }
    ToolSpec(
      Name,
      "Say which topic the user's latest message is about. Call it once, before answering, " +
        "when the message says the topic may have changed.",
      args
    )
  }

  /** The id of the entry recording `turn`'s verdict. */
  def verdictId(turn: TurnRef): EntryId =
    EntryId(s"topic:${WorkflowId.value(turn.workflowId)}:verdict")

  /** The note on the user's message: the topic may have changed, the current and earlier
    * topics by name, and the call to make first.
    */
  def tag(c: TurnTopics.Classification): String = {
    val current = c.current.fold("none")(_.key)
    val earlier = if (c.earlier.isEmpty) "none" else c.earlier.map(_.key).mkString("; ")
    // The whole instruction rides here, with the tool, and never in the system prompt: a
    // model told of a tool it was not offered writes the call out as text (seen live with
    // gpt-oss-20b, which answered a message with "topic(new,movie)").
    val earlierCall =
      if (c.earlier.isEmpty) "" else "`earlier` with that topic in `earlier`, "
    s"[grit: the topic may have changed. Current topic: $current. Earlier topics: $earlier. " +
      s"First call the `${ToolName.value(Name)}` tool once to say what this message is " +
      s"about: `current`, $earlierCall`new` with a short name. Then answer the message " +
      "as usual, without mentioning topics, this note or the tool.]"
  }

  /** `base` with the tool offered and [[tag]] after its last user message. */
  def offer(base: ModelRequest, c: TurnTopics.Classification): ModelRequest = {
    val last = base.messages.lastIndexWhere {
      case Message.User(_) => true
      case _ => false
    }
    val tagged = base.messages.zipWithIndex.map {
      case (Message.User(text), i) if i == last => Message.User(s"$text\n\n${tag(c)}")
      case (m, _) => m
    }
    base.copy(
      messages = tagged,
      tools = Vector(topic(c).schema(strict = false)),
      use = ToolUse.Auto
    )
  }

  /** How a request is built from the turn's plain one. */
  enum Shape {

    /** The plain request, unchanged. */
    case Plain

    /** The plain request with the tool offered for `c` ([[offer]]). */
    case Offered(c: TurnTopics.Classification)

    /** The second round's: the offer for `c`, then `first`, its reply ([[again]]). */
    case Again(c: TurnTopics.Classification, first: Message.Assistant)

    /** `base`, shaped. */
    def apply(base: ModelRequest): ModelRequest = this match {
      case Plain => base
      case Offered(c) => offer(base, c)
      case Again(c, first) => again(offer(base, c), c, first)
    }
  }

  /** A model's `reply` to the request `shape` built. */
  final case class Replied(reply: Message.Assistant, shape: Shape)

  /** What a round came to: the reply that answers the turn, or why none does; the verdict
    * of its first reply; what went wrong that the turn survived; and the replies spent on
    * the verdict alone, the answer aside.
    */
  final case class Round(
      answer: Either[TurnFailure, Replied],
      verdict: Verdict,
      anomaly: Option[String],
      spent: Vector[Replied]
  )

  /** The model calls a round may make after its first reply. */
  trait Calls {

    /** The model's reply to the request `shape` builds. */
    def again(shape: Shape.Again): Either[TurnFailure, Message.Assistant]

    /** The model's reply to the plain request. */
    def plain(): Either[TurnFailure, Message.Assistant]
  }

  /** The round after `first`, the reply to the request offered for `c`: `further` makes
    * each call the round needs, at most once each, `again` before `plain`. The answer
    * fails only when the plain call does.
    */
  def round(c: TurnTopics.Classification, first: Message.Assistant, further: Calls^): Round = {
    val verdict = of(c, first)
    val offered = Replied(first, Shape.Offered(c))
    if (calls(first).isEmpty) Round(Right(offered), verdict, None, Vector.empty)
    else {
      val shape = new Shape.Again(c, first)
      val second = further.again(shape)
      second.toOption.flatMap(answer) match {
        case Some(answered) =>
          val dropped = Option.when(second.exists(m => calls(m).nonEmpty))(
            "the second call called a tool again; its calls were dropped"
          )
          Round(Right(Replied(answered, shape)), verdict, dropped, Vector(offered))
        case None =>
          val why = second match {
            case Left(failure) => s"the second call failed ($failure)"
            case Right(_) => "the second call called a tool again and said nothing"
          }
          Round(
            further.plain().map(Replied(_, Shape.Plain)),
            verdict,
            Some(s"$why; a plain call answered"),
            offered +: second.toOption.map(Replied(_, shape)).toVector
          )
      }
    }
  }

  /** The tool calls in `reply`, in order. */
  def calls(reply: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    reply.blocks.collect { case c: AssistantBlock.ToolCall => c }

  /** The second round's request: `offered` (the offer for `c`), then the first round's
    * `reply` and a result for each of its calls, the tool no longer callable.
    */
  def again(
      offered: ModelRequest,
      c: TurnTopics.Classification,
      reply: Message.Assistant
  ): ModelRequest =
    offered.copy(messages = offered.messages ++ (reply +: results(c, reply)), use = ToolUse.Off)

  /** A result for each call in `reply`, the tool offered for `c`: the first `topic` call is
    * noted, or refused with [[ArgsError.message]] when its arguments cannot be read; a later
    * one already noted; any other tool unknown.
    */
  def results(
      c: TurnTopics.Classification,
      reply: Message.Assistant
  ): Vector[Message.ToolResult] = {
    val first = calls(reply).find(_.name == ToolName.value(Name)).map(_.id)
    calls(reply).map { call =>
      if (call.name != ToolName.value(Name))
        Message.ToolResult(call.id, s"There is no tool named ${call.name}.", true)
      else if (!first.contains(call.id)) Message.ToolResult(call.id, "Already noted.", false)
      else
        read(c, call.arguments) match {
          case Left(error) =>
            Message.ToolResult(call.id, s"${error.message} Answer the message now.", true)
          case Right(_) => Message.ToolResult(call.id, "Noted. Answer the message now.", false)
        }
    }
  }

  /** The verdict the model gave in `reply` to the tool offered for `c`: its first `topic`
    * call's ([[verdict]]), or unreadable when it made none.
    */
  def of(c: TurnTopics.Classification, reply: Message.Assistant): Verdict =
    calls(reply)
      .find(_.name == ToolName.value(Name))
      .fold[Verdict](Verdict.Unreadable("answered without calling topic"))(call =>
        verdict(c, call.arguments)
      )

  /** The verdict `arguments` give to the tool offered for `c`; unreadable when [[read]]
    * refuses them, saying why and what was sent (cut to 200 characters).
    */
  def verdict(c: TurnTopics.Classification, arguments: ujson.Value): Verdict =
    read(c, arguments).fold(
      error => Verdict.Unreadable(s"${error.message} Sent: ${arguments.render().take(200)}"),
      identity
    )

  /** `arguments`, read by [[topic]]`(c)`. */
  def read(c: TurnTopics.Classification, arguments: ujson.Value): Either[ArgsError, Verdict] =
    topic(c).args.read(arguments)

  /** `reply` with its tool calls dropped; `None` when no text is left. */
  def answer(reply: Message.Assistant): Option[Message.Assistant] = {
    val kept = reply.blocks.filter {
      case AssistantBlock.ToolCall(_, _, _) => false
      case _ => true
    }
    Option.when(kept.exists {
      case AssistantBlock.Text(t) => t.trim.nonEmpty
      case _ => false
    })(reply.copy(blocks = kept))
  }

  /** Where `verdict` places `turn`'s message: the current topic, the earlier one whose key
    * it names, or a new topic, opened (also for an earlier key that is none of `c`'s); an
    * unreadable verdict leaves the classifier's placement standing. `anomaly` goes on the
    * record.
    */
  def events(
      verdict: Verdict,
      anomaly: Option[String],
      c: TurnTopics.Classification,
      turn: TurnRef
  ): Vector[TopicEvent] = {
    val by = Placement.Asked(verdict, anomaly)
    def whole(t: TopicId): TopicEvent =
      TopicEvent.Placed(turn.turnSeq, Weights.whole(t), by)
    val opened = TopicId.openedBy(turn)
    def open: Vector[TopicEvent] = Vector(TopicEvent.Opened(opened), whole(opened))
    verdict match {
      case Verdict.Current => c.current.fold(open)(s => Vector(whole(s.id)))
      case Verdict.Earlier(key) =>
        c.earlier.find(_.key == key).fold(open)(s => Vector(whole(s.id)))
      case Verdict.New(_) => open
      case Verdict.Unreadable(_) =>
        c.placed.toVector.map(p => TopicEvent.Placed(turn.turnSeq, p.weights, by))
    }
  }

  /** What the calls made for the verdict cost, the reply's own call aside: their model,
    * summed usage, and the estimated input of their requests.
    */
  def cost(spent: Vector[(Message.Assistant, Tokens)]): Option[(String, Usage, Tokens)] =
    spent.headOption.map { (first, _) =>
      (
        first.model,
        Usage.total(spent.map(_._1.usage)),
        spent.map(_._2).foldLeft(Tokens.Zero)(_ + _)
      )
    }
}
