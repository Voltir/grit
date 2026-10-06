package grit.turn

import grit.assembly.estimate.CharEstimate
import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ProviderError, ToolUse}
import grit.core.store.{Entry, InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, ToolName, ToolSpec, Toolbox}
import grit.core.topic.{Placement, Verdict}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*
import TurnLoop.Round
import TurnFixtures.Scripted

/** The tool loop inside the durable turn ([[Turn.Patches.Tools]]), over core's in-memory
  * durability and store: which steps it records, what each model call is shown, what the
  * store keeps, and what a crash in a tool's step runs again.
  */
object TurnLoopTurnTests extends TestSuite {
  import TurnFixtures.*

  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001")))

  private def calling(text: String, calls: (String, String, String)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls.map((id, name, p) =>
        AssistantBlock.ToolCall(ToolCallId(id), name, ujson.Obj("path" -> p))
      ),
      if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
      usage,
      "m"
    )

  private val files = Map("a.txt" -> "alpha", "b.txt" -> "beta", "c.txt" -> "gamma")

  /** `said` answered in one turn with the loop offered `peek` and `poke` over `ws`,
    * `provider` answering, entries kept in `entries` (`store` behind it).
    */
  private def looped(
      durable: InMemoryDurable,
      store: InMemoryEntryStore,
      entries: grit.core.store.EntryStore,
      turn: TurnRef,
      provider: grit.core.provider.Provider^,
      ws: Files^,
      ledger: InMemoryUsageLedger = new InMemoryUsageLedger,
      summarizer: grit.core.provider.Provider^ = new StubProvider(),
      calls: Int = 5
  ): String = {
    val _ = store
    durable.run(turn.workflowId)(
      tooledBody(entries, provider, ledger, summarizer, NoClassifier, ws, tools(ws), calls)
    )
  }

  private def own(entries: InMemoryEntryStore, turn: TurnRef): Vector[Entry] =
    entries
      .list(conversation)(using grit.dbos.sql.TestTx.fake)
      .getOrElse(Vector.empty)
      .filter(_.turnSeq == turn.turnSeq)

  private def exchange(entries: InMemoryEntryStore, turn: TurnRef): Vector[Message] =
    own(entries, turn).map(_.payload).collect {
      case Payload.Exchange(reply) => reply: Message
      case Payload.Result(result, _) => result: Message
    }

  /** Three rounds: `peek` a, then `peek` b and c, then the answer. */
  private def threeRounds: Scripted = new Scripted((_, n) =>
    Right(n match {
      case 0 => calling("looking", ("t1", "peek", "a.txt"))
      case 1 => calling("", ("t2", "peek", "b.txt"), ("t3", "peek", "c.txt"))
      case _ => calling("alpha, beta and gamma")
    })
  )

  val tests = Tests {
    test("three rounds: each call and each tool its own step, the exchange kept in order") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val ledger = new InMemoryUsageLedger
      val summarizer = new RecordingProvider
      val ws = new Files(files)
      val provider = threeRounds
      val turn = say(entries, "read them")
      looped(durable, entries, entries, turn, provider, ws, ledger, summarizer) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "DBOS.patch-tools") ==> Vector(
        "DBOS.patch-tools",
        "call-model",
        "record-call:0",
        "tool:0:0",
        "call-model:1",
        "record-call:1",
        "tool:1:0",
        "tool:1:1",
        "call-model:2",
        "append",
        "summarise",
        "append-summary"
      )
      ws.reads ==> 3
      val kept = exchange(entries, turn)
      kept ==> Vector(
        calling("looking", ("t1", "peek", "a.txt")),
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        calling("", ("t2", "peek", "b.txt"), ("t3", "peek", "c.txt")),
        Message.ToolResult(ToolCallId("t2"), "beta", isError = false),
        Message.ToolResult(ToolCallId("t3"), "gamma", isError = false)
      )
      // Each call is shown its window, then the turn's own messages so far.
      provider.requests.map(_.messages) ==> Vector(
        Vector(Message.User("read them")),
        Message.User("read them") +: kept.take(2),
        Message.User("read them") +: kept
      )
      provider.requests.map(r => (r.tools.map(_.name), r.use)) ==>
        Vector.fill(3)((Vector("peek", "poke"), ToolUse.Auto))
      // The answer is the reply entry; no window or later turn sees the exchange.
      texts(entries).filter(_.startsWith("assistant:")) ==> Vector(
        "assistant: alpha, beta and gamma"
      )
      own(entries, turn).map(_.id).filter(EntryId.value(_).startsWith("result:")) ==>
        Vector("result:c1:0:0:0", "result:c1:0:1:0", "result:c1:0:1:1").map(EntryId(_))
      // Each call billed once, beside the estimate of exactly what it was sent.
      ledger.rows.map(r => EntryId.value(r._1)) ==>
        Vector("call:c1:0:0", "call:c1:0:1", "reply:c1:0", "summary:c1:0")
      ledger.rows.take(3).map(_._5) ==> provider.requests.map(CharEstimate.request)
      // The summary is written knowing what was read.
      val summarised = summarizer.requests.flatMap(_.messages).collect { case Message.User(t) => t }
      assert(
        summarised.exists(t =>
          t.contains("[called peek (t1)") && t.contains("Tool result (t1): alpha")
        )
      )
      // A later turn's window holds the reply, not the exchange.
      val next = new RecordingProvider
      runTurn(new InMemoryDurable, entries, next, say(entries, "and?"))
      next.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User("read them"),
          calling("alpha, beta and gamma"),
          Message.User("and?")
        )
      )
    }

    test("a tool made with calling is told the tool:n:j call it runs, free or approved") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val path = Args.of((path = Field.text("A path."))).map(_.path)
      def where(name: ToolName, gate: Gate[String]) =
        new Hosted(ToolSpec(name, "Says which call it is.", path), gate, p => p)
          .calling((_, at) => Outcome.Done(at.key))
      val toolbox = Toolbox
        .of[caps.CapSet^{ws}](
          peek(ws),
          where(ToolName("where"), Gate.Free),
          where(ToolName("where_asking"), Gate.Ask(p => p))
        )
        .fold(d => throw new java.lang.AssertionError(d), identity)
      val provider = new Scripted((_, n) =>
        Right(n match {
          case 0 => calling("", ("t1", "peek", "a.txt"), ("t2", "where", "x"))
          case 1 => calling("", ("t3", "where_asking", "y"))
          case _ => calling("told")
        })
      )
      val turn = say(entries, "where are you")
      val durable = new InMemoryDurable
      def run(through: grit.core.store.EntryStore): String =
        durable.run(turn.workflowId)(
          tooledBody(
            through,
            provider,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            toolbox,
            5
          )
        )
      assertThrows[InMemoryDurable.Crash](run(crashingAtAsk(entries)))
      durable.send(
        turn.workflowId,
        Approval.topic(ToolCallId("t3")),
        Approval.encode(Approval.Approved)
      )
      run(entries) ==> "replied: reply:c1:0; summarised: summary:c1:0"
      exchange(entries, turn).collect { case r: Message.ToolResult => r } ==> Vector(
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        Message.ToolResult(ToolCallId("t2"), "tool:c1:0:0:1", isError = false),
        Message.ToolResult(ToolCallId("t3"), "tool:c1:0:1:0", isError = false)
      )
    }

    test("a tool turn's rounds are built and recorded without listing the conversation") {
      val store = new InMemoryEntryStore
      val entries = new UnlistedWhileAnswering(store)
      val provider = threeRounds
      val turn = say(store, "read them")
      looped(new InMemoryDurable, store, entries, turn, provider, new Files(files)) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      entries.windows ==> 1
      val kept = exchange(store, turn)
      provider.requests.map(_.messages) ==> Vector(
        Vector(Message.User("read them")),
        Message.User("read them") +: kept.take(2),
        Message.User("read them") +: kept
      )
    }

    test("the turn replayed calls nothing and runs nothing") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val ws = new Files(files)
      val turn = say(entries, "read them")
      looped(durable, entries, entries, turn, threeRounds, ws)
      val again = new RecordingProvider
      val history = durable.history(turn.workflowId)
      new InMemoryDurable()
        .replay(turn.workflowId, history)(
          tooledBody(
            new InMemoryEntryStore,
            again,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            tools(ws),
            5
          )
        )
        .isRight ==> true
      (again.requests.size, ws.reads) ==> (0, 3)
    }

    test("a crash between a free tool's run and its result runs it again, keeping one result") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, e => EntryId.value(e.id).startsWith("result:"))
      val durable = new InMemoryDurable
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(if (n == 0) calling("", ("t1", "peek", "a.txt")) else calling("alpha"))
      )
      val turn = say(store, "read a")
      assertThrows[InMemoryDurable.Crash](looped(durable, store, entries, turn, provider, ws))
      durable.recordedSteps(turn.workflowId).lastOption ==> Some("record-call:0")
      ws.reads ==> 1
      looped(durable, store, entries, turn, provider, ws) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      ws.reads ==> 2
      exchange(store, turn).count(_.isInstanceOf[Message.ToolResult]) ==> 1
      provider.requests.size ==> 2
    }

    test("a gated call nobody answers in time is denied, and not run") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(if (n == 0) calling("", ("t1", "poke", "a.txt")) else calling("could not"))
      )
      val turn = say(entries, "poke a")
      val durable = new InMemoryDurable
      looped(durable, entries, entries, turn, provider, ws)
      ws.reads ==> 0
      exchange(entries, turn).collect { case r: Message.ToolResult => r } ==> Vector(
        Outcome.Unanswered.result(ToolCallId("t1"))
      )
      durable.recordedSteps(turn.workflowId).filter(_.contains(":0:0")) ==>
        Vector("ask:0:0", "tool:0:0")
    }

    test("a gated call is asked about, and runs once approved") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(if (n == 0) calling("", ("t1", "poke", "a.txt")) else calling("poked"))
      )
      val turn = say(entries, "poke a")
      val durable = new InMemoryDurable
      val atAsk = crashingAtAsk(entries)
      assertThrows[InMemoryDurable.Crash](looped(durable, entries, atAsk, turn, provider, ws))
      durable.send(
        turn.workflowId,
        Approval.topic(ToolCallId("t1")),
        Approval.encode(Approval.Approved)
      )
      looped(durable, entries, entries, turn, provider, ws) ==>
        "replied: reply:c1:0; summarised: summary:c1:0"
      ws.reads ==> 1
      val slot = TurnTools.Slot(turn, Round.First, 0)
      entries.get(slot.askId)(using TestTx.fake).toOption.flatten.map(_.payload) ==>
        Some(Payload.Ask(ToolCallId("t1"), "poke a.txt"))
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "record-call:0").take(5) ==>
        Vector("record-call:0", "ask:0:0", "DBOS.recv", "DBOS.sleep", "tool:0:0")
      // A watcher is shown the answer's arrival as the wait for it.
      Turn.Step.named(durable.recordedSteps(turn.workflowId)).filter(_.contains(":0:0")) ==>
        Vector("ask:0:0", "wait:0:0", "tool:0:0")
    }

    test("a gated call declined is denied with the reason, and the model reads it") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(if (n == 0) calling("", ("t1", "poke", "a.txt")) else calling("understood"))
      )
      val turn = say(entries, "poke a")
      val durable = new InMemoryDurable
      val no = Approval.Declined(Some("not that file"))
      val atAsk = crashingAtAsk(entries)
      assertThrows[InMemoryDurable.Crash](looped(durable, entries, atAsk, turn, provider, ws))
      durable.send(turn.workflowId, Approval.topic(ToolCallId("t1")), Approval.encode(no))
      looped(durable, entries, entries, turn, provider, ws)
      ws.reads ==> 0
      val denied = Outcome.Declined(Some("not that file")).result(ToolCallId("t1"))
      exchange(entries, turn).collect { case r: Message.ToolResult => r } ==> Vector(denied)
      provider.requests.lastOption.flatMap(_.messages.lastOption) ==> Some(denied)
    }

    test("an unknown tool and unreadable arguments are results the model reads") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(n match {
          case 0 =>
            calling("", ("t1", "grep", "x")).copy(blocks =
              Vector(
                AssistantBlock.ToolCall(ToolCallId("t1"), "grep", ujson.Obj()),
                AssistantBlock.ToolCall(ToolCallId("t2"), "peek", ujson.Obj("file" -> "a"))
              )
            )
          case _ => calling("sorry")
        })
      )
      val turn = say(entries, "go")
      looped(new InMemoryDurable, entries, entries, turn, provider, ws)
      exchange(entries, turn).collect { case r: Message.ToolResult => (r.content, r.isError) } ==>
        Vector(
          ("There is no tool named `grep`; the tools are `peek`, `poke`.", true),
          (
            "The call to `peek` was not run: There is no argument `file`; the arguments there " +
              "are `path`. You sent: {\"file\":\"a\"}",
            true
          )
        )
    }

    test("a reply cut off at max tokens has its calls answered, not run") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(
          if (n == 0) calling("", ("t1", "peek", "a.txt")).copy(stop = StopReason.MaxTokens)
          else calling("shorter")
        )
      )
      val turn = say(entries, "go")
      looped(new InMemoryDurable, entries, entries, turn, provider, ws)
      ws.reads ==> 0
      exchange(entries, turn).collect { case r: Message.ToolResult => r } ==>
        Vector(TurnLoop.CutOff.result(ToolCallId("t1")))
    }

    test("the budget's last call is made with tools off, and its calls dropped") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, _) => Right(calling("more", ("t", "peek", "a.txt"))))
      val turn = say(entries, "go")
      val ledger = new InMemoryUsageLedger
      looped(new InMemoryDurable, entries, entries, turn, provider, ws, ledger, calls = 2)
      provider.requests.map(_.use) ==> Vector(ToolUse.Auto, ToolUse.Off)
      // Only the last call is told it is the last, after all it is sent; nothing keeps it.
      provider.requests.map(_.messages.lastOption) ==> Vector(
        Some(Message.User("go")),
        Some(Message.User(TurnLoop.LastCall))
      )
      provider.requests.lastOption.map(_.messages.dropRight(1)) ==>
        provider.requests.headOption.map(_.messages ++ exchange(entries, turn))
      assert(!texts(entries).exists(_.contains(TurnLoop.LastCall)))
      // Each call billed beside the estimate of what it was sent, the note included.
      ledger.rows.take(2).map(_._5) ==> provider.requests.map(CharEstimate.request)
      texts(entries).filter(_.startsWith("assistant:")) ==> Vector("assistant: more")
    }

    test("a silent reply fails the turn") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        Right(if (n == 0) calling("", ("t1", "peek", "a.txt")) else calling(""))
      )
      val turn = say(entries, "go")
      looped(new InMemoryDurable, entries, entries, turn, provider, ws) ==>
        "failed: Model(the reply to call-model:1 said nothing and called no tool)"
    }

    test("a failed call after a tool round ends the turn with no reply") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val provider = new Scripted((_, n) =>
        if (n == 0) Right(calling("", ("t1", "peek", "a.txt")))
        else Left(ProviderError.Unavailable("HTTP 529"))
      )
      val turn = say(entries, "go")
      looped(
        new InMemoryDurable,
        entries,
        entries,
        turn,
        provider,
        ws
      ) ==> "failed: Model(HTTP 529 (after 3 tries))"
      texts(entries).filter(_.startsWith("assistant:")) ==> Vector.empty
    }

    test(
      "unsure of the topic: topic offered first, asked for on the first call, its verdict recorded"
    ) {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        runTurn(
          new InMemoryDurable,
          entries,
          new RecordingProvider,
          say(entries, text),
          classifier = classifier
        )
      }
      val ws = new Files(files)
      val provider = new Scripted((r, n) =>
        if (n == 0)
          Right(
            calling("", ("t1", "peek", "a.txt")).copy(blocks =
              Vector(
                AssistantBlock.ToolCall(ToolCallId("v"), "topic", ujson.Obj("about" -> "current")),
                AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj("path" -> "a.txt"))
              )
            )
          )
        else new StubProvider().complete(r)
      )
      val turn = say(entries, "hm ~0.5")
      val durable = new InMemoryDurable
      durable.run(turn.workflowId)(
        tooledBody(
          entries,
          provider,
          new InMemoryUsageLedger,
          new StubProvider(),
          classifier,
          ws,
          tools(ws),
          5
        )
      )
      provider.requests.map(_.tools.map(_.name)) ==>
        Vector.fill(2)(Vector("topic", "peek", "poke"))
      def lastUser(r: ModelRequest): String =
        r.messages.collect { case Message.User(t) => t }.lastOption.getOrElse("")
      assert(lastUser(provider.requests(0)).contains("[grit: the topic may have changed."))
      lastUser(provider.requests(1)) ==> "hm ~0.5"
      exchange(entries, turn).collect { case r: Message.ToolResult => r.content } ==>
        Vector(TurnVerdict.Noted, "alpha")
      durable.recordedSteps(turn.workflowId).dropWhile(_ != "call-model:1") ==>
        Vector("call-model:1", "record-verdict", "append", "summarise", "append-summary")
      topics(entries).placements.getOrElse(turn.turnSeq, Vector.empty).map(_.by).lastOption ==>
        Some(Placement.Asked(Verdict.Current, None))
    }

    test("unsure of the topic, and topic called only in a later round: that is on the record") {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach { text =>
        runTurn(
          new InMemoryDurable,
          entries,
          new RecordingProvider,
          say(entries, text),
          classifier = classifier
        )
      }
      val ws = new Files(files)
      val provider = new Scripted((r, n) =>
        if (n == 0) Right(calling("", ("t1", "peek", "a.txt")))
        else if (n == 1)
          Right(
            calling("", ("t2", "peek", "a.txt")).copy(blocks =
              Vector(
                AssistantBlock.ToolCall(ToolCallId("v"), "topic", ujson.Obj("about" -> "new"))
              )
            )
          )
        else new StubProvider().complete(r)
      )
      val turn = say(entries, "hm ~0.5")
      new InMemoryDurable().run(turn.workflowId)(
        tooledBody(
          entries,
          provider,
          new InMemoryUsageLedger,
          new StubProvider(),
          classifier,
          ws,
          tools(ws),
          5
        )
      )
      topics(entries).placements.getOrElse(turn.turnSeq, Vector.empty).map(_.by).lastOption ==>
        Some(
          Placement.Asked(
            Verdict.Unreadable("answered without calling topic"),
            Some(
              "no topic call in the first reply; topic called only in a later round (1); not read"
            )
          )
        )
    }
  }
}
