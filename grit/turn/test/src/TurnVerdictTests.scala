package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError, ToolUse}
import grit.core.store.{Entry, EntryStore, InMemoryEntryStore, InMemoryUsageLedger, StoreError, Tx}
import grit.core.topic.{Placement, TopicEvent, TopicId, Verdict}
import grit.models.StubProvider

import utest.*

/** The verdict round of a turn that passed the tool loop's patch before it shipped
  * ([[Turn.Patches.Tools]]): each asked turn here runs on [[before]]. The verdict under
  * the loop is in `TurnLoopTurnTests`.
  */
object TurnVerdictTests extends TestSuite {
  import TurnFixtures.*

  /** A durability whose turns take the old branch of the tool loop's patch. */
  private def before: InMemoryDurable = new InMemoryDurable(unpatched = Set(Turn.Patches.Tools))

  /** A provider answering each call with `script(request, call)`, calls counted from 0,
    * keeping every request.
    */
  final class Scripted(script: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant])
      extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val n = requests.size
      requests = requests :+ request
      script(request, n)
    }
  }

  /** `underlying`, refusing to insert the entry `id`. */
  final class Refusing(underlying: EntryStore, id: EntryId) extends EntryStore {
    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (entry.id == id) Left(StoreError.DatabaseError("no room for a verdict"))
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  private def said(text: String, calls: AssistantBlock*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls,
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  private def topicCall(args: ujson.Value) =
    AssistantBlock.ToolCall(grit.core.id.ToolCallId("t1"), "topic", args)

  /** Two earlier turns, the second a new topic; then `asked` answered by `provider`, with
    * the stub classifier. The last turn, its conversation's entries and ledger.
    */
  private def third(
      asked: String,
      provider: Provider^,
      durable: InMemoryDurable = before
  ): (TurnRef, InMemoryEntryStore, InMemoryUsageLedger) = {
    val entries = new InMemoryEntryStore
    val ledger = new InMemoryUsageLedger
    val classifier = new CountingClassifier
    Vector("hello", "knots? ~0.1").foreach { text =>
      val t = say(entries, text)
      runTurn(
        new InMemoryDurable,
        entries,
        new RecordingProvider,
        t,
        ledger,
        classifier = classifier
      )
    }
    val turn = say(entries, asked)
    runTurn(durable, entries, provider, turn, ledger, classifier = classifier)
    (turn, entries, ledger)
  }

  private def placements(entries: InMemoryEntryStore, turn: TurnRef): Vector[TopicEvent.Placed] =
    topics(entries).placements.getOrElse(turn.turnSeq, Vector.empty)

  private def lastBy(entries: InMemoryEntryStore, turn: TurnRef): Option[Placement] =
    placements(entries, turn).lastOption.map(_.by)

  val tests = Tests {
    test("sure of the topic: no tool, no note") {
      val provider = new RecordingProvider
      val (_, _, _) = third("more ~0.9", provider)
      provider.requests.lastOption.map(r => (r.tools, r.messages.lastOption)) ==>
        Some((Vector.empty, Some(Message.User("more ~0.9"))))
    }

    test("unsure: the tool offered and the note on the message, in the request only") {
      val provider = new RecordingProvider
      val (turn, entries, _) = third("hm ~0.5", provider)
      val first = provider.requests.lift(provider.requests.size - 1)
      first.map(_.tools.map(_.name)) ==> Some(Vector("topic"))
      first.map(_.use) ==> Some(ToolUse.Auto)
      val note = first.flatMap(_.messages.lastOption) match {
        case Some(Message.User(text)) => text
        case other => other.toString
      }
      assert(
        note.startsWith("hm ~0.5\n\n[grit: the topic may have changed."),
        note.contains("Current topic: new topic."),
        note.contains("Earlier topics: new topic (2).")
      )
      texts(entries).filter(_.startsWith("user: hm")) ==> Vector("user: hm ~0.5")
    }

    test("the verdict: noted, the model called again, and the second reply is the reply") {
      val provider = new RecordingProvider
      val durable = before
      val (turn, entries, ledger) =
        third("""hm ~0.5 #call:{"about":"new","name":"Sailing"}""", provider, durable)
      provider.requests.size ==> 2 // both rounds of this turn
      val again = provider.requests.lastOption.getOrElse(sys.error("no request"))
      again.use ==> ToolUse.Off
      again.tools.map(_.name) ==> Vector("topic")
      again.messages.takeRight(2).collect { case r: Message.ToolResult => r.content } ==>
        Vector("Noted. Answer the message now.")
      durable.recordedSteps(turn.workflowId).filter(Turn.Step.optional.contains) ==>
        Vector("call-model-again", "record-verdict")
      // Round one's text ("stub calls topic") is nowhere: the reply is round two's.
      val replies = texts(entries).filter(_.startsWith("assistant:"))
      // The stub quotes the message as round two saw it: with the note.
      assert(
        replies.lastOption.exists(
          _.startsWith(
            s"assistant: stub reply to: ${"""hm ~0.5 #call:{"about":"new","name":"Sailing"}"""}"
          )
        )
      )
      assert(!texts(entries).exists(_.contains("stub calls")))
      heard(durable, turn)._2.text ==> replies.lastOption.getOrElse("").stripPrefix("assistant: ")
      // Placed by the classifier, then by the verdict: a new topic, which the last wins.
      placements(entries, turn).map(_.by) ==> Vector(
        Placement.Classified(0.5, Placement.Outcome.Uncertain),
        Placement.Asked(Verdict.New(Some("Sailing")), None)
      )
      topics(entries).placed(turn.turnSeq) ==> Some(TopicId.openedBy(turn))
      ledger.rows.map(_._1) ==> ledger.rows.map(_._1).distinct
      assert(ledger.rows.exists(_._1 == TurnVerdict.verdictId(turn)))
    }

    test("the verdict names an earlier topic: placed back in it") {
      val (turn, entries, _) =
        third(
          """back ~0.5 #call:{"about":"earlier","earlier":"new topic (2)"}""",
          new RecordingProvider
        )
      val t = topics(entries)
      t.placed(turn.turnSeq) ==> Some(
        TopicId.openedBy(TurnRef(conversation, grit.core.id.TurnSeq(0)))
      )
      t.topics.size ==> 2
    }

    test("reading a call: the earlier key an enum, and each refusal's message") {
      val none = TurnTopics.Classification(Vector.empty, None, Vector.empty, None, None)
      val two = none.copy(
        earlier = Vector(
          TurnTopics.Shown(TopicId("a"), "Knots"),
          TurnTopics.Shown(TopicId("b"), "Knots (2)")
        )
      )
      TurnVerdict.read(two, ujson.Obj("about" -> "earlier", "earlier" -> "Knots (2)")) ==>
        Right(Verdict.Earlier("Knots (2)"))
      TurnVerdict.read(two, ujson.Obj("about" -> "new", "name" -> "  Sailing ")) ==>
        Right(Verdict.New(Some("Sailing")))
      TurnVerdict.read(two, ujson.Obj("about" -> "new", "name" -> " ")) ==> Right(Verdict.New(None))
      TurnVerdict.read(two, ujson.Obj("about" -> "current", "earlier" -> "Knots")) ==>
        Right(Verdict.Current)
      TurnVerdict.read(two, ujson.Obj("about" -> "earlier")).left.map(_.message) ==>
        Left("`earlier` is missing: it takes one of `Knots`, `Knots (2)`.")
      TurnVerdict
        .read(two, ujson.Obj("about" -> "earlier", "earlier" -> "knots"))
        .left
        .map(_.message) ==>
        Left("`earlier` takes one of `Knots`, `Knots (2)`, not \"knots\".")
      // No earlier topics: no `earlier` field to send, and no `earlier` to say.
      TurnVerdict.topic(none).schema(strict = false).parameters("properties").obj.keys.toVector ==>
        Vector("about", "name")
      TurnVerdict.read(none, ujson.Obj("about" -> "earlier")).left.map(_.message) ==>
        Left("`about` takes one of `current`, `new`, not \"earlier\".")
      TurnVerdict.read(none, ujson.Obj("about" -> "new", "earlier" -> "x")).left.map(_.message) ==>
        Left("There is no argument `earlier`; the arguments there are `about`, `name`.")
    }

    test("the verdict says current: it stays") {
      val (turn, entries, _) =
        third("""hm ~0.5 #call:{"about":"current"}""", new RecordingProvider)
      val t = topics(entries)
      t.placed(turn.turnSeq) ==> t.current.map(_.id)
      lastBy(entries, turn) ==> Some(Placement.Asked(Verdict.Current, None))
    }

    test("unreadable arguments: an error result, the classifier's placement stands") {
      val provider = new RecordingProvider
      val (turn, entries, _) = third("""hm ~0.5 #call:{"about":"sideways"}""", provider)
      val refused = "`about` takes one of `current`, `earlier`, `new`, not \"sideways\"."
      provider.requests.lastOption.toVector
        .flatMap(_.messages)
        .collect { case r: Message.ToolResult => (r.content, r.isError) } ==>
        Vector((s"$refused Answer the message now.", true))
      val ps = placements(entries, turn)
      ps.map(_.weights).distinct.size ==> 1
      lastBy(entries, turn) ==> Some(
        Placement.Asked(Verdict.Unreadable(s"""$refused Sent: {"about":"sideways"}"""), None)
      )
    }

    test("the earlier topics are an enum in the tool's schema, offered per call") {
      val provider = new RecordingProvider
      val (_, _, _) = third("hm ~0.5", provider)
      val shown = provider.requests.lift(provider.requests.size - 1).flatMap(_.tools.headOption)
      shown.map(_.parameters("properties")("earlier")("enum")) ==>
        Some(ujson.Arr("new topic (2)"))
      shown.map(_.parameters("required")) ==> Some(ujson.Arr("about"))
    }

    test("the model answers without calling: its answer is the reply, the verdict unreadable") {
      val provider =
        new Scripted((r, _) => new StubProvider().complete(r.copy(tools = Vector.empty)))
      val durable = before
      val (turn, entries, _) = third("hm ~0.5", provider, durable)
      durable.recordedSteps(turn.workflowId).filter(Turn.Step.optional.contains) ==>
        Vector("record-verdict")
      lastBy(entries, turn) ==>
        Some(Placement.Asked(Verdict.Unreadable("answered without calling topic"), None))
      // Round one's own answer, to the message with its note.
      assert(
        texts(entries)
          .filter(_.startsWith("assistant:"))
          .lastOption
          .exists(_.startsWith("assistant: stub reply to: hm ~0.5\n\n[grit:"))
      )
    }

    test("round two calls again beside text: the text is the reply, the call dropped, noted") {
      val provider = new Scripted((_, n) =>
        if (n == 0) Right(said("thinking", topicCall(ujson.Obj("about" -> "current"))))
        else Right(said("the answer", topicCall(ujson.Obj("about" -> "new"))))
      )
      val (turn, entries, _) = third("hm ~0.5", provider)
      texts(entries).filter(_.startsWith("assistant:")).lastOption ==> Some("assistant: the answer")
      lastBy(entries, turn) ==> Some(
        Placement.Asked(
          Verdict.Current,
          Some("the second call called a tool again; its calls were dropped")
        )
      )
    }

    test("round two calls again and says nothing: a plain call answers; the turn never fails") {
      val provider = new Scripted((r, _) =>
        if (r.tools.isEmpty) Right(said("plainly"))
        else Right(said("", topicCall(ujson.Obj("about" -> "current"))))
      )
      val durable = before
      val (turn, entries, ledger) = third("hm ~0.5", provider, durable)
      durable.recordedSteps(turn.workflowId).filter(Turn.Step.optional.contains) ==>
        Vector("call-model-again", "call-model-plain", "record-verdict")
      val plain = provider.requests.lastOption.getOrElse(sys.error("no request"))
      plain.tools ==> Vector.empty
      plain.messages.lastOption ==> Some(Message.User("hm ~0.5"))
      texts(entries).filter(_.startsWith("assistant:")).lastOption ==> Some("assistant: plainly")
      lastBy(entries, turn) ==> Some(
        Placement.Asked(
          Verdict.Current,
          Some("the second call called a tool again and said nothing; a plain call answered")
        )
      )
      // Both calls spent on the verdict are billed to it, once.
      ledger.rows.collect { case r if r._1 == TurnVerdict.verdictId(turn) => r._4.input } ==>
        Vector(Tokens(20))
    }

    test("round two fails: a plain call answers") {
      val provider = new Scripted((r, n) =>
        if (n == 0) new StubProvider().complete(r)
        else if (r.tools.isEmpty) Right(said("plainly"))
        else Left(ProviderError.Unavailable("HTTP 529"))
      )
      val (turn, entries, _) = third("""hm ~0.5 #call:{"about":"current"}""", provider)
      texts(entries).filter(_.startsWith("assistant:")).lastOption ==> Some("assistant: plainly")
      lastBy(entries, turn).collect { case Placement.Asked(_, a) => a } ==>
        Some(
          Some("the second call failed (Model(HTTP 529 (after 3 tries))); a plain call answered")
        )
    }

    test("the verdict not recorded: the turn replies, and its log says so") {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        val t = say(entries, text)
        runTurn(new InMemoryDurable, entries, new RecordingProvider, t, classifier = classifier)
      }
      val turn = say(entries, """hm ~0.5 #call:{"about":"current"}""")
      val refusing = new Refusing(entries, TurnVerdict.verdictId(turn))
      val log = runTurn(
        before,
        refusing,
        new RecordingProvider,
        turn,
        classifier = classifier
      )
      assert(
        log.startsWith("replied: "),
        log.endsWith("; verdict not recorded: Store(no room for a verdict)")
      )
    }

    test("a crash in round two resumes there, without calling round one again") {
      // Its calls count from 0 on the third turn: round one, round two (dies), round two.
      val provider = new Scripted((r, n) =>
        if (n == 1) throw new InMemoryDurable.Crash else new StubProvider().complete(r)
      )
      val durable = before
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        val t = say(entries, text)
        runTurn(new InMemoryDurable, entries, new RecordingProvider, t, classifier = classifier)
      }
      val turn = say(entries, """hm ~0.5 #call:{"about":"current"}""")
      assertThrows[InMemoryDurable.Crash](
        runTurn(durable, entries, provider, turn, classifier = classifier)
      )
      runTurn(durable, entries, provider, turn, classifier = classifier)
      provider.requests.map(_.use) ==> Vector(ToolUse.Auto, ToolUse.Off, ToolUse.Off)
      assert(
        texts(entries)
          .filter(_.startsWith("assistant:"))
          .lastOption
          .exists(_.startsWith("""assistant: stub reply to: hm ~0.5 #call:{"about":"current"}"""))
      )
    }
  }
}
