package grit.turn

import grit.core.agenda.{Agenda, Placement, Weights}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ModelRequest
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

/** The main model asked where a message went, when the classifier was unsure
  * ([[TurnAgenda.Classification.uncertain]]): `topic` is one of every turn's tools
  * ([[tool]]), offered alike whether or not the turn asks, so turns of one conversation offer
  * the same tools (ADR 0052); on a turn that asks, a one-line note on the user's message ([[tag]]) asks for it, the same on every
  * call ([[tagged]]), and the verdict is the first call's reply's ([[of]]). The exchange stays
  * in the journal and the verdict's record, never among the conversation's messages.
  */
object TurnVerdict {

  /** The tool's name. */
  val Name: ToolName = ToolName("topic")

  /** The `topic` tool's spec, on every call of every turn: `about` is `current`, `earlier` or
    * `new`; `earlier` an earlier topic's key, required when `about` is `earlier`; `name` a new
    * topic's name, trimmed, a blank one none. A key is not checked here ([[checked]]).
    */
  val Spec: ToolSpec[Agenda.Verdict] = {
    val earlier = Field.text("For earlier: the earlier topic's key, as the note lists them.")
    def named(n: Option[String]): Option[String] = n.map(_.trim).filter(_.nonEmpty)
    ToolSpec(
      Name,
      "Say which topic the user's latest message is about. Call it once, before answering, " +
        "when the message says the topic may have changed.",
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
            name = Field.text("For new: a short name for the subject, at most four words.").optional
          )
        )
        .refine(a =>
          a.about match {
            case "current" => Right(Agenda.Verdict.Current)
            case "earlier" =>
              a.earlier
                .map(Agenda.Verdict.Earlier(_))
                .toRight(ArgsError.Missing("earlier", earlier.accepts))
            case _ => Right(Agenda.Verdict.New(named(a.name)))
          }
        )
    )
  }

  /** `v`, unless it names an earlier topic none of `c`'s earlier items has: then
    * `ArgsError.Refused` on `earlier`, naming them, or saying there are none.
    */
  def checked(c: TurnAgenda.Classification, v: Agenda.Verdict): Either[ArgsError, Agenda.Verdict] =
    v match {
      case Agenda.Verdict.Earlier(key) if !c.earlier.exists(_.key == key) =>
        val there =
          if (c.earlier.isEmpty) "There are no earlier topics."
          else s"The earlier topics are: ${c.earlier.map(_.key).mkString("; ")}."
        Left(ArgsError.Refused("earlier", s"No earlier topic is called $key. $there"))
      case _ => Right(v)
    }

  /** The id of the entry recording `turn`'s verdict. */
  def verdictId(turn: TurnRef): EntryId =
    EntryId(s"topic:${WorkflowId.value(turn.workflowId)}:verdict")

  /** The note on the user's message: the item may have changed, the current and earlier
    * items by name, and the call to make first.
    */
  def tag(c: TurnAgenda.Classification): String = {
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

  /** `base` with [[tag]] after its last user message that is no note on the loop's exchange (a
    * user message right after a tool result, a reply that called tools, or another such note),
    * and nothing else changed: each call of a turn's loop is tagged at the same message, as its
    * first call was.
    */
  def tagged(base: ModelRequest, c: TurnAgenda.Classification): ModelRequest =
    base.copy(messages = taggedWith(base.messages, Some(tag(c))))

  /** `messages` with `note` after their [[asker]], after a blank line, as [[tagged]] adds a
    * tag; as they are with no `note` or no asker.
    */
  def taggedWith(messages: Vector[Message], note: Option[String]): Vector[Message] =
    note.fold(messages) { n =>
      val last = asker(messages)
      messages.zipWithIndex.map {
        case (Message.User(text), i) if last.contains(i) => Message.User(s"$text\n\n$n")
        case (m, _) => m
      }
    }

  /** The index of `messages`' last user message that is no note on the loop's exchange (a
    * user message right after a tool result, a reply that called tools, or another such note):
    * the one [[tagged]] tags. `None` when there is none.
    */
  def asker(messages: Vector[Message]): Option[Int] = {
    val notes = messages
      .foldLeft((Vector.empty[Boolean], false)) { case ((flags, afterExchange), m) =>
        val note = m match {
          case Message.User(_) => afterExchange
          case _ => false
        }
        val next = m match {
          case _: Message.ToolResult => true
          case a: Message.Assistant => calls(a).nonEmpty
          case Message.User(_) => note
        }
        (flags :+ note, next)
      }
      ._1
    Some(messages.zip(notes).lastIndexWhere {
      case (Message.User(_), false) => true
      case _ => false
    }).filter(_ >= 0)
  }

  /** What the `topic` tool answers when its call reads: the loop's model goes on. */
  val Noted = "Noted. Answer the message now."

  /** The `topic` tool, [[Spec]] on every call: a call that reads, and is [[checked]] against
    * `asked` when there is one, is answered [[Noted]]; one that does not, with the reason; and
    * nothing else happens. Its answers are grit's own words
    * ([[grit.core.tool.Believed.Own]]). The verdict is read from the first reply's call, not
    * from running it, and only when `asked` ([[of]]).
    */
  def tool(asked: Option[TurnAgenda.Classification]): Tool[Agenda.Verdict] = {
    val spec = asked.fold(Spec)(c => Spec.copy(args = Spec.args.refine(checked(c, _))))
    new Hosted(spec, Gate.Free, shown, Believed.Own).over(_ => Outcome.Done(Noted))
  }

  /** `verdict` as a transcript shows a `topic` call that gave it. */
  private def shown(verdict: Agenda.Verdict): String = verdict match {
    case Agenda.Verdict.Current => "current"
    case Agenda.Verdict.Earlier(key) => s"earlier $key"
    case Agenda.Verdict.New(name) => ("new" +: name.toVector).mkString(" ")
    case Agenda.Verdict.Unreadable(why) => s"unreadable: $why"
  }

  /** The tool calls in `reply`, in order. */
  def calls(reply: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    reply.blocks.collect { case c: AssistantBlock.ToolCall => c }

  /** What went wrong with the `topic` calls in `replies`, a loop's reply to each round, the
    * first first, that the verdict survived: no call in the first reply, a call only in a
    * later one (read nowhere: the verdict is the first reply's), or more than one call (only
    * the first counts); each said, in that order, and `None` when there was one call, in the
    * first reply.
    */
  def anomaly(replies: Vector[Message.Assistant]): Option[String] = {
    val verdictCalls = replies.map(calls(_).count(_.name == ToolName.value(Name)))
    val first = verdictCalls.headOption.getOrElse(0)
    val later = verdictCalls.zipWithIndex.drop(1).collect { case (n, round) if n > 0 => round }
    val total = verdictCalls.sum
    val said = Vector(
      Option.when(first == 0)("no topic call in the first reply"),
      Option.when(first == 0 && later.nonEmpty)(
        s"topic called only in a later round (${later.mkString(", ")}); not read"
      ),
      Option.when(total > 1)(s"topic called $total times; only the first counts")
    ).flatten
    Option.when(said.nonEmpty)(said.mkString("; "))
  }

  /** The verdict the model gave in `reply` to the tool offered for `c`: its first `topic`
    * call's ([[verdict]]), or unreadable when it made none.
    */
  def of(c: TurnAgenda.Classification, reply: Message.Assistant): Agenda.Verdict =
    calls(reply)
      .find(_.name == ToolName.value(Name))
      .fold[Agenda.Verdict](Agenda.Verdict.Unreadable("answered without calling topic"))(call =>
        verdict(c, call.arguments)
      )

  /** The verdict `arguments` give to the tool offered for `c`; unreadable when [[read]]
    * refuses them, saying why and what was sent (cut to 200 characters).
    */
  def verdict(c: TurnAgenda.Classification, arguments: ujson.Value): Agenda.Verdict =
    read(c, arguments).fold(
      error => Agenda.Verdict.Unreadable(s"${error.message} Sent: ${arguments.render().take(200)}"),
      identity
    )

  /** `arguments`, read by [[Spec]], then [[checked]] against `c`. */
  def read(
      c: TurnAgenda.Classification,
      arguments: ujson.Value
  ): Either[ArgsError, Agenda.Verdict] =
    Spec.args.read(arguments).flatMap(checked(c, _))

  /** Where `verdict` places `turn`'s message: the current item, the earlier one whose key
    * it names, or a new item, opened (also for an earlier key that is none of `c`'s); an
    * unreadable verdict leaves the classifier's placement standing. `anomaly` goes on the
    * record.
    */
  def events(
      verdict: Agenda.Verdict,
      anomaly: Option[String],
      c: TurnAgenda.Classification,
      turn: TurnRef
  ): Vector[Agenda.Event] = {
    val by = Placement.Asked(verdict, anomaly)
    def whole(t: Agenda.ItemId): Agenda.Event =
      Agenda.Event.Placed(turn.turnSeq, Weights.whole(t), by)
    val opened = Agenda.ItemId.openedBy(turn)
    def open: Vector[Agenda.Event] = Vector(Agenda.Event.Opened(opened), whole(opened))
    verdict match {
      case Agenda.Verdict.Current => c.current.fold(open)(s => Vector(whole(s.id)))
      case Agenda.Verdict.Earlier(key) =>
        c.earlier.find(_.key == key).fold(open)(s => Vector(whole(s.id)))
      case Agenda.Verdict.New(_) => open
      case Agenda.Verdict.Unreadable(_) =>
        c.placed.toVector.map(p => Agenda.Event.Placed(turn.turnSeq, p.weights, by))
    }
  }
}
