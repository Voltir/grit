package grit.turn

import grit.assembly.estimate.CharEstimate
import grit.core.durable.InMemoryDurable
import grit.core.id.WorkflowId
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
    Vector("assemble", "call-model", "append", "summarise", "append-summary")

  val tests = Tests {
    test("a turn records the model's reply as its entry") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(new InMemoryDurable, entries, provider, turn) ==> Done
      texts(entries) ==> Vector(
        "user: hello",
        "assistant: stub reply to: hello",
        "summary: stub reply to: User: hello\n\nAssistant: stub reply to: hello"
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
      val entries = new CrashOnInsert(store)
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(store, "hello")
      assertThrows[InMemoryDurable.Crash](runTurn(durable, entries, provider, turn))
      durable.recordedSteps(turn.workflowId) ==> Vector("assemble", "call-model")
      runTurn(durable, entries, provider, turn) ==> Done
      provider.requests.size ==> 1
      durable.recordedSteps(turn.workflowId) ==> AllSteps
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
          TurnSummary.System,
          Vector(Message.User("User: two\n\nAssistant: stub reply to: two"))
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
      durable.recordedSteps(turn.workflowId) ==> AllSteps.take(4)
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
      durable.recordedSteps(turn.workflowId) ==> AllSteps.take(4)
      runTurn(durable, entries, new RecordingProvider, turn, summarizer = summarizer) ==> Done
      summarizer.requests.size ==> 1
      durable.recordedSteps(turn.workflowId) ==> AllSteps
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

    test("a failed model call ends the turn with no reply") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider(fail = true)
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn) ==> "failed: Model(down)"
      durable.recordedSteps(turn.workflowId) ==> Vector("assemble", "call-model")
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
