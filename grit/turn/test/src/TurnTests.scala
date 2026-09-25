package grit.turn

import grit.assembly.estimate.CharEstimate
import grit.core.context.AssemblyNote
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*

object TurnTests extends TestSuite {

  import TurnFixtures.*

  private val Done = "replied: reply:c1:0; summarised: summary:c1:0"

  private val AllSteps: Vector[String] =
    Vector(
      "classify",
      "record-topic",
      "assemble",
      "record-window",
      "call-model",
      "call-model-again",
      "call-model-plain",
      "record-verdict",
      "append",
      "summarise",
      "append-summary"
    )

  /** The steps every turn takes. */
  private val Always: Vector[String] = AllSteps.filterNot(Turn.Step.optional.contains)

  /** What a turn records: its steps, each patch's marker before the steps it brought. */
  private val Recorded: Vector[String] =
    ("DBOS.patch-topics" +: Always)
      .patch(4, Vector("DBOS.patch-record-window"), 0)
      .patch(6, Vector("DBOS.patch-tools"), 0)

  /** How many of [[Recorded]] come before `assemble`. */
  private val Placing = 3

  val tests = Tests {
    test("the step names are the recorded ones, and a running turn is in the next") {
      Turn.Step.all ==> AllSteps
      Turn.running(Vector.empty) ==> "classify"
      Turn.running(Vector("DBOS.patch-topics", "classify")) ==> "record-topic"
      Turn.running(Vector("DBOS.patch-topics", "classify", "record-topic")) ==> "assemble"
      Turn.running(Vector("assemble")) ==> "record-window"
      Turn.running(Vector("assemble", "DBOS.patch-record-window", "record-window")) ==>
        "call-model"
      Turn.running(Vector("assemble", "DBOS.patch", "call-model", "append")) ==> "summarise"
      Turn.running(AllSteps) ==> "append-summary"
      // A step only some turns take is never named before it is recorded.
      Turn.running(Vector("assemble", "record-window", "call-model")) ==> "append"
      Turn.running(Vector("call-model", "call-model-again")) ==> "append"
    }

    test("a tool loop's steps are named from its rounds, and a running loop is in the next") {
      val round1 = TurnLoop.Round.First.next
      (round1.step, Turn.Step.recordCall(round1), Turn.Step.tool(round1, 2)) ==>
        ("call-model:1", "record-call:1", "tool:1:2")
      Vector("call-model:3", "record-call:0", "tool:1:2", "DBOS.patch-tools", "tool:x:1")
        .map(Turn.Step.family) ==>
        Vector(Some("call-model"), Some("record-call"), Some("tool"), None, None)
      val loop = Vector("record-window", "DBOS.patch-tools", "call-model")
      Turn.running(loop :+ "record-call:0") ==> "tool:0:0"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:0")) ==> "call-model:1"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:1", "call-model:1")) ==> "append"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:0", "call-model:1", "append")) ==>
        "summarise"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:0", "record-verdict")) ==> "append"
    }

    test("a turn records the model's reply as its entry") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(new InMemoryDurable, entries, provider, turn) ==> Done
      texts(entries) ==> Vector(
        "user: hello",
        "assistant: stub reply to: hello",
        // The stub quotes its request; it gives no labelled lines, so all of it is the summary.
        "summary: stub reply to: This exchange starts a topic that has no name yet: give it one.\n\nUser: hello\n\n" +
          "Assistant: stub reply to: hello"
      )
      provider.requests.map(_.system) ==> Vector(system)
    }

    test("each call's usage is recorded once, under the entry it produced") {
      val entries = new InMemoryEntryStore
      val ledger = new InMemoryUsageLedger
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      val provider = new RecordingProvider
      val summarizer = new RecordingProvider
      runTurn(durable, entries, provider, turn, ledger, summarizer)
      runTurn(durable, entries, new RecordingProvider, turn, ledger, new RecordingProvider)
      ledger.rows.map(r => (r._1, r._2, r._3)) ==>
        Vector(Turn.replyId(turn), TurnSummary.id(turn))
          .map(id => (id, turn.workflowId, StubProvider.Model))
      // Beside each, the estimate of exactly the request that was sent.
      ledger.rows.map(_._5) ==>
        (provider.requests ++ summarizer.requests).map(CharEstimate.request)
    }

    test("M0 gate: the same workflow id twice calls the provider once") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn) ==> Done
      runTurn(durable, entries, provider, turn) ==> Done
      provider.requests.size ==> 1
      texts(entries).size ==> 3
    }

    test("a crash after the model call resumes without calling it again") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, isReply)
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(store, "hello")
      assertThrows[InMemoryDurable.Crash](runTurn(durable, entries, provider, turn))
      durable.recordedSteps(turn.workflowId) ==> Recorded.take(Placing + 5)
      runTurn(durable, entries, provider, turn) ==> Done
      provider.requests.size ==> 1
      durable.recordedSteps(turn.workflowId) ==> Recorded
    }

    test("the reply is told to edges as it streams, and the pieces join back to it") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello there")
      runTurn(durable, entries, new RecordingProvider, turn)
      val (pieces, h) = heard(durable, turn)
      assert(pieces.nonEmpty)
      h.text ==> "stub reply to: hello there"
    }

    test("a crash mid-stream: the rerun's pieces follow, and a reader hears only them") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      // Long enough that the crashed run fills and writes a piece before it dies.
      val asked = Vector.tabulate(80)(i => s"word$i").mkString(" ")
      val provider = new DiesMidStream(before = 60)
      val turn = say(entries, asked)
      assertThrows[InMemoryDurable.Crash](runTurn(durable, entries, provider, turn))
      runTurn(durable, entries, provider, turn)
      provider.calls ==> 2
      val (pieces, h) = heard(durable, turn)
      pieces.map(_.attempt).distinct.size ==> 2
      h.text ==> s"stub reply to: $asked"
      texts(entries).lastOption.exists(_.startsWith("summary")) ==> true
    }

    test("the window is recorded before the model is called") {
      val entries = new InMemoryEntryStore
      val provider = new Peeking(entries)
      runTurn(new InMemoryDurable, entries, provider, say(entries, "hello")) ==> Done
      provider.sawWindow ==> Vector(true)
    }

    test("a crash recording the window resumes there, and the model is called once") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Window])
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(store, "hello")
      assertThrows[InMemoryDurable.Crash](runTurn(durable, entries, provider, turn))
      provider.requests.size ==> 0
      runTurn(durable, entries, provider, turn) ==> Done
      provider.requests.size ==> 1
      windows(store).size ==> 1
    }

    test("the summary is written from the turn's own messages, after its reply") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val summarizer = new RecordingProvider
      runTurn(durable, entries, new RecordingProvider, say(entries, "one"), summarizer = summarizer)
      val turn = say(entries, "two")
      runTurn(durable, entries, new RecordingProvider, turn, summarizer = summarizer)
      summarizer.requests.lastOption ==> Some(
        ModelRequest(
          TurnSummary.TopicalSystem,
          Vector(
            Message.User(
              "This exchange starts a topic that has no name yet: give it one.\n\nUser: two\n\nAssistant: stub reply to: two"
            )
          )
        )
      )
      val all = entries.list(conversation)(using TestTx.fake).getOrElse(Vector.empty)
      val summary = all.find(_.id == TurnSummary.id(turn))
      summary.map(_.parentId) ==> Some(Some(Turn.replyId(turn)))
      summary.map(_.turnSeq) ==> Some(turn.turnSeq)
      all.lastOption.map(_.id) ==> Some(TurnSummary.id(turn))
    }

    test("a failed summary leaves the reply standing") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      val out = runTurn(
        durable,
        entries,
        new RecordingProvider,
        turn,
        summarizer = new RecordingProvider(fail = true)
      )
      out ==> "replied: reply:c1:0; no summary: Model(down)"
      durable.recordedSteps(turn.workflowId) ==> Recorded.take(Placing + 7)
      texts(entries) ==> Vector("user: hello", "assistant: stub reply to: hello")
    }

    test("a summary with no text is a failed summary") {
      val silent = new Provider {
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
          Right(
            Message.Assistant(
              Vector(AssistantBlock.Reasoning("thinking", None), AssistantBlock.Text("  ")),
              StopReason.MaxTokens,
              Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
              "m"
            )
          )
      }
      val entries = new InMemoryEntryStore
      val turn = say(entries, "hello")
      runTurn(new InMemoryDurable, entries, new RecordingProvider, turn, summarizer = silent) ==>
        "replied: reply:c1:0; no summary: Model(the summary has no text (stop: MaxTokens))"
      texts(entries).size ==> 2
    }

    test("a crash while recording the summary resumes without summarising again") {
      val store = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val summarizer = new RecordingProvider
      val turn = say(store, "hello")
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Summary])
      assertThrows[InMemoryDurable.Crash](
        runTurn(durable, entries, new RecordingProvider, turn, summarizer = summarizer)
      )
      durable.recordedSteps(turn.workflowId) ==> Recorded.take(Placing + 7)
      runTurn(durable, entries, new RecordingProvider, turn, summarizer = summarizer) ==> Done
      summarizer.requests.size ==> 1
      durable.recordedSteps(turn.workflowId) ==> Recorded
    }

    test("a query assembly wrote is recorded before the reply, with its cost, and never sent") {
      val entries = new InMemoryEntryStore
      val ledger = new InMemoryUsageLedger
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(
        turnBodyWith(entries, provider, new Noting(entries, queried), ledger)
      ) ==> Done
      texts(entries).take(3) ==>
        Vector("user: hello", "query: hello greeting", "assistant: stub reply to: hello")
      ledger.rows.map(r => (r._1, r._4, r._5)).headOption ==>
        Some((Turn.queryId(turn, 0), queried.usage, queried.estimate))
      provider.requests.map(_.messages) ==> Vector(Vector(Message.User("hello")))
    }

    test("a later turn sees earlier turns, then its own message") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      runTurn(durable, entries, provider, say(entries, "one"))
      runTurn(durable, entries, provider, say(entries, "two"))
      provider.requests.map(_.messages.size) ==> Vector(1, 3)
      provider.requests.lastOption.flatMap(_.messages.headOption) ==> Some(Message.User("one"))
      provider.requests.lastOption.flatMap(_.messages.lastOption) ==> Some(Message.User("two"))
    }

    test("each reply's window is recorded beside it, with the turns search recalled") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val one = say(entries, "one")
      runTurn(durable, entries, provider, one)
      val two = say(entries, "two")
      val recalling = new Noting(entries, AssemblyNote.Recalled(Vector(one.turnSeq)))
      val _ = durable.run(two.workflowId)(
        turnBodyWith(entries, provider, recalling, new InMemoryUsageLedger)
      )
      windows(entries) ==> Vector(
        Turn.windowId(one) -> Payload.Window(Vector.empty, Vector.empty),
        Turn.windowId(two) ->
          Payload.Window(Vector(EntryId("in:one"), Turn.replyId(one)), Vector(one.turnSeq))
      )
      texts(entries).drop(3).take(2) ==> Vector("user: two", "assistant: stub reply to: two")
    }

    test("a failed model call ends the turn with no reply") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider(fail = true)
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn) ==> "failed: Model(down)"
      durable.recordedSteps(turn.workflowId) ==> Recorded.take(Placing + 5)
      texts(entries) ==> Vector("user: hello")
    }

    test("not a turn id") {
      val entries = new InMemoryEntryStore
      new InMemoryDurable().run(WorkflowId("proof"))(
        turnBody(entries, new RecordingProvider)
      ) ==> "not a turn: proof"
    }
  }
}
