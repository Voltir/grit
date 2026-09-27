package grit.turn

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Db, Entry, InMemoryEntryStore, InMemoryUsageLedger, Nearby, Payload}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*

object TurnTests extends TestSuite {

  import TurnFixtures.*

  private val Done = "replied: reply:c1:0; summarised: summary:c1:0"

  private val AllSteps: Vector[String] =
    Vector(
      "pin-models",
      "offer",
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
    Always
      .patch(2, Vector("DBOS.patch-topics"), 0)
      .patch(6, Vector("DBOS.patch-record-window"), 0)
      .patch(8, Vector("DBOS.patch-tools"), 0)

  /** How many of [[Recorded]] come before `assemble`. */
  private val Placing = 5

  val tests = Tests {
    test("the step names are the recorded ones, and a running turn is in the next") {
      Turn.Step.all ==> AllSteps
      Turn.running(Vector.empty) ==> "pin-models"
      Turn.running(Vector("pin-models")) ==> "offer"
      Turn.running(Vector("pin-models", "offer")) ==> "classify"
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
      // Waiting for a person: DBOS records the wait's end as it begins, which is no step, so
      // the turn is waiting on them; once the wait ends, answered or not, the tool runs.
      val asked = loop ++ Vector("record-call:0", "ask:0:0", "DBOS.sleep")
      Turn.running(asked) ==> "wait:0:0"
      Turn.running(asked :+ "DBOS.recv") ==> "tool:0:0"
      Turn.running(asked ++ Vector("DBOS.recv", "tool:0:0")) ==> "call-model:1"
      Turn.Step.family("wait:0:1") ==> Some("wait")
      Turn.Step.family("ask:0:1") ==> Some("ask")
      Turn.running(loop ++ Vector("record-call:0", "tool:0:1", "call-model:1")) ==> "append"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:0", "call-model:1", "append")) ==>
        "summarise"
      Turn.running(loop ++ Vector("record-call:0", "tool:0:0", "record-verdict")) ==> "append"
    }

    test("a wait for a person is named for the call it waits on, once it has ended") {
      Turn.Step.named(
        Vector("ask:1:0", "DBOS.recv", "DBOS.sleep", "tool:1:0", "ask:1:1", "DBOS.sleep")
      ) ==> Vector("ask:1:0", "wait:1:0", "DBOS.sleep", "tool:1:0", "ask:1:1", "DBOS.sleep")
      // A recv that ends no ask's wait keeps DBOS's name.
      Turn.Step.named(Vector("tool:0:0", "DBOS.recv", "ask:0:1", "tool:0:1", "DBOS.recv")) ==>
        Vector("tool:0:0", "DBOS.recv", "ask:0:1", "tool:0:1", "DBOS.recv")
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
      val store = new InMemoryEntryStore
      val ledger = new InMemoryUsageLedger
      val durable = new InMemoryDurable
      val turn = say(store, "hello")
      val provider = new RecordingProvider
      val summarizer = new RecordingProvider
      // Dies recording the summary, after the reply's row: the rerun must not record it again.
      val entries = new CrashOnInsert(store, _.payload.isInstanceOf[Payload.Summary])
      assertThrows[InMemoryDurable.Crash](
        runTurn(durable, entries, provider, turn, ledger, summarizer)
      )
      runTurn(durable, entries, provider, turn, ledger, summarizer) ==> Done
      ledger.rows.map(r => (r._1, r._2, r._3)) ==>
        Vector(Turn.replyId(turn), TurnSummary.id(turn))
          .map(id => (id, turn.workflowId, StubProvider.Model))
      // Beside each, the estimate of exactly the request that was sent.
      ledger.rows.map(_._5) ==>
        (provider.requests ++ summarizer.requests).map(CharEstimate.request)
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

    test(
      "nearby sections come first, one message each; a gone entry is left out, an empty section dropped"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val closing = TestClosings.prose("We talked about one.", Some("one"))
      val closed = EntryId("closing:c1:1")
      say(entries, "one")
      entries.insert(
        Entry(
          closed,
          conversation,
          TurnSeq(0),
          None,
          1,
          Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, closing),
          Instant.EPOCH
        )
      )(using TestTx.fake)
      val api = ConversationId("api")
      def elsewhere(id: String, seq: Long, message: Message) =
        entries.insert(
          Entry(EntryId(id), api, TurnSeq(0), None, seq, Payload.Message(message), Instant.EPOCH)
        )(using TestTx.fake)
      elsewhere("api:u", 0, Message.User("the invoice test is flaky"))
      elsewhere(
        "api:r",
        1,
        Message.Assistant(
          Vector(AssistantBlock.Text("Pin TZ=UTC in the test JVM.")),
          StopReason.EndTurn,
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
          "m"
        )
      )
      val web = ConversationId("web")
      entries.insert(
        Entry(
          EntryId("web:u"),
          web,
          TurnSeq(0),
          None,
          0,
          Payload.Message(Message.User("the login page is blank")),
          Instant.EPOCH
        )
      )(using TestTx.fake)
      val turn = say(entries, "two")
      val at = (p: String) => Place.read(p).fold(e => sys.error(e), identity)
      val nearby = Vector(
        Nearby(
          api,
          at("fs:/home/nick/api"),
          Vector(EntryId("api:u"), EntryId("api:gone"), EntryId("api:r"))
        ),
        Nearby(ConversationId("docs"), at("fs:/home/nick/docs"), Vector(EntryId("docs:gone"))),
        Nearby(web, at("fs:/home/nick/web"), Vector(EntryId("web:u")))
      )
      val opening = new ContextAssembler {
        def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
          Right(Window(Vector(closed), Vector.empty, nearby))
      }
      new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(entries, provider, opening, new InMemoryUsageLedger)
      )
      provider.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User(
            "[afar] another conversation of yours, shown by grit, still open, at fs:/home/nick/api:\n" +
              "User: the invoice test is flaky\nAssistant: Pin TZ=UTC in the test JVM."
          ),
          Message.User(
            "[afar] another conversation of yours, shown by grit, still open, at fs:/home/nick/web:\n" +
              "User: the login page is blank"
          ),
          Message.User(
            "[record] this conversation so far, written by grit (closed 1970-01-01): " +
              "We talked about one.\nOutcome: one"
          ),
          Message.User("two")
        )
      )
      windows(entries).lastOption.map(_._2) ==> Some(
        Payload.Window(Vector(closed), Vector.empty, nearby)
      )
    }

    test("a window that leaves turns out is sent with a gap line where they were") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      say(entries, "one")
      say(entries, "two")
      say(entries, "three")
      val turn = say(entries, "four")
      val recalled = new ContextAssembler {
        def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
          Right(Window(Vector(EntryId("in:one"), EntryId("in:three"))))
      }
      new InMemoryDurable().run(turn.workflowId)(
        turnBodyWith(entries, provider, recalled, new InMemoryUsageLedger)
      )
      provider.requests.headOption.map(_.messages) ==> Some(
        Vector(
          Message.User("one"),
          Message.User("[gap] earlier turns not shown"),
          Message.User("three"),
          Message.User("four")
        )
      )
    }

    test("each turn's window is recorded as its own entry, with the turns search recalled") {
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
    }

    test("a failed model call ends the turn with no reply") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider(fail = true)
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn) ==> "failed: Model(down (after 3 tries))"
      durable.recordedSteps(turn.workflowId) ==> Recorded.take(Placing + 5)
      texts(entries) ==> Vector("user: hello")
    }

    test("a provider unavailable twice is asked again after each wait, as a fresh attempt") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new Flaky(2, ProviderError.Unavailable("HTTP 504: error code: 504"))
      val clock = new NoWait
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, provider, clock = clock)) ==> Done
      provider.calls ==> 3
      clock.waited ==> Turn.Retries.toVector
      val (pieces, told) = heard(durable, turn)
      pieces.map(_.attempt).distinct.size ==> 3
      told.text ==> "stub reply to: hello"
      durable.recordedSteps(turn.workflowId) ==> Recorded
    }

    test("a provider unavailable on every try fails the call after the last wait") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new Flaky(3, ProviderError.Unavailable("HTTP 504"))
      val clock = new NoWait
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, provider, clock = clock)) ==>
        "failed: Model(HTTP 504 (after 3 tries))"
      provider.calls ==> 3
      clock.waited ==> Turn.Retries.toVector
    }

    test("a provider that refused is not asked again") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new Flaky(1, ProviderError.Refused("HTTP 400: too long"))
      val clock = new NoWait
      val turn = say(entries, "hello")
      durable.run(turn.workflowId)(turnBody(entries, provider, clock = clock)) ==>
        "failed: Model(HTTP 400: too long)"
      provider.calls ==> 1
      clock.waited ==> Vector.empty
    }

    test("not a turn id") {
      val entries = new InMemoryEntryStore
      new InMemoryDurable().run(WorkflowId("proof"))(
        turnBody(entries, new RecordingProvider)
      ) ==> "not a turn: proof"
    }
  }
}
