package grit.turn

import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.{ModelRequest, Tool, ToolUse}
import grit.core.topic.{Placement, TopicEvent, TopicId, Verdict, Weights}

/** The main model asked where a message went, when the classifier was unsure
  * ([[TurnTopics.Classification.uncertain]]). Its request carries the `topic` tool and a
  * one-line note on the user's message; a call to the tool is its verdict, answered
  * "noted", and the model is called again for the reply. At most one more round: if that
  * round calls a tool again and says nothing, or fails, a plain call with neither tool nor
  * exchange answers instead. The exchange stays in the journal and the verdict's record,
  * never among the conversation's messages. Pure: the turn runs the calls.
  */
object TurnVerdict {

  /** The tool's name. */
  val Name = "topic"

  /** The `topic` tool. */
  val Topic: Tool = Tool(
    Name,
    "Say which topic the user's latest message is about. Call it once, before answering, " +
      "when the message says the topic may have changed.",
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "about" -> ujson.Obj(
          "type" -> "string",
          "enum" -> ujson.Arr("current", "earlier", "new"),
          "description" -> ("current: it carries on the current topic. earlier: it goes back " +
            "to an earlier topic. new: a subject not discussed before.")
        ),
        "name" -> ujson.Obj(
          "type" -> "string",
          "description" -> ("For earlier: that topic's name, as the note gives it. For new: a " +
            "short name for the subject, at most four words.")
        )
      ),
      "required" -> ujson.Arr("about"),
      "additionalProperties" -> false
    )
  )

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
    s"[grit: the topic may have changed. Current topic: $current. Earlier topics: $earlier. " +
      s"First call the `$Name` tool once to say what this message is about: `current`, " +
      "`earlier` with that topic's name, or `new` with a short name. Then answer the message " +
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
    base.copy(messages = tagged, tools = Vector(Topic), use = ToolUse.Auto)
  }

  /** The tool calls in `reply`, in order. */
  def calls(reply: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    reply.blocks.collect { case c: AssistantBlock.ToolCall => c }

  /** The second round's request: `offered`, then the first round's `reply` and a result for
    * each of its calls, the tool no longer callable.
    */
  def again(offered: ModelRequest, reply: Message.Assistant): ModelRequest =
    offered.copy(messages = offered.messages ++ (reply +: results(reply)), use = ToolUse.Off)

  /** A result for each call in `reply`: the first `topic` call is noted (or refused, when
    * its arguments cannot be read), a later one already noted, any other tool unknown.
    */
  def results(reply: Message.Assistant): Vector[Message.ToolResult] = {
    val first = calls(reply).find(_.name == Name).map(_.id)
    calls(reply).map { c =>
      if (c.name != Name) Message.ToolResult(c.id, s"There is no tool named ${c.name}.", true)
      else if (!first.contains(c.id)) Message.ToolResult(c.id, "Already noted.", false)
      else
        verdict(c.arguments) match {
          case Verdict.Unreadable(_) =>
            Message.ToolResult(
              c.id,
              "Could not read that: `about` is current, earlier or new. Answer the message now.",
              true
            )
          case _ => Message.ToolResult(c.id, "Noted. Answer the message now.", false)
        }
    }
  }

  /** The verdict the model gave in `reply`: its first `topic` call's, or unreadable when
    * it made none.
    */
  def of(reply: Message.Assistant): Verdict =
    calls(reply)
      .find(_.name == Name)
      .fold[Verdict](Verdict.Unreadable("answered without calling topic"))(c =>
        verdict(c.arguments)
      )

  /** The verdict `arguments` give. */
  def verdict(arguments: ujson.Value): Verdict = {
    val o = arguments.objOpt
    val name = o.flatMap(_.get("name")).flatMap(_.strOpt).map(_.trim).filter(_.nonEmpty)
    o.flatMap(_.get("about")).flatMap(_.strOpt).map(_.trim.toLowerCase) match {
      case Some("current") => Verdict.Current
      case Some("earlier") =>
        name.fold[Verdict](Verdict.Unreadable(s"earlier, with no name: ${arguments.render()}"))(
          Verdict.Earlier(_)
        )
      case Some("new") => Verdict.New(name)
      case _ => Verdict.Unreadable(arguments.render().take(200))
    }
  }

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

  /** Where `verdict` places `turn`'s message: the current topic, the earlier one it names
    * (by its key, or its name without a number), or a new topic, opened; an unreadable
    * verdict leaves the classifier's placement standing. `anomaly` goes on the record.
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
      case Verdict.Earlier(name) => matching(name, c).fold(open)(s => Vector(whole(s.id)))
      case Verdict.New(_) => open
      case Verdict.Unreadable(_) =>
        c.placed.toVector.map(p => TopicEvent.Placed(turn.turnSeq, p.weights, by))
    }
  }

  private def matching(name: String, c: TurnTopics.Classification): Option[TurnTopics.Shown] = {
    def plain(s: String): String = s.trim.toLowerCase.replaceAll("""\s*\(\d+\)$""", "")
    c.earlier
      .find(_.key == name)
      .orElse(c.earlier.find(_.key.trim.equalsIgnoreCase(name.trim)))
      .orElse(c.earlier.find(s => plain(s.key) == plain(name)))
  }

  /** What the calls made for the verdict cost, the reply's own call aside: their model,
    * summed usage, and the estimated input of their requests.
    */
  def cost(spent: Vector[(Message.Assistant, Tokens)]): Option[(String, Usage, Tokens)] =
    spent.headOption.map { (first, _) =>
      val usage = spent.drop(1).map(_._1.usage).foldLeft(first.usage) { (a, b) =>
        Usage(
          a.input + b.input,
          a.output + b.output,
          a.cachedInput + b.cachedInput,
          a.costUsd.zip(b.costUsd).map(_ + _).orElse(a.costUsd).orElse(b.costUsd)
        )
      }
      (first.model, usage, spent.map(_._2).foldLeft(Tokens.Zero)(_ + _))
    }
}
