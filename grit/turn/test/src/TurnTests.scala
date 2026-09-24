package grit.turn

import grit.core.*
import grit.models.StubProvider
import utest.*

object TurnTests extends TestSuite {

  import TurnFixtures.*

  val tests = Tests {
    test("a turn records the model's reply as its entry") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(new InMemoryDurable, entries, provider, turn) ==> "replied: reply:c1:0"
      texts(entries) ==> Vector("user: hello", "assistant: stub reply to: hello")
      provider.requests.map(_.system) ==> Vector(system)
    }

    test("the reply's usage is recorded once, under the reply's entry") {
      val entries = new InMemoryEntryStore
      val ledger = new InMemoryUsageLedger
      val durable = new InMemoryDurable
      val turn = say(entries, "hello")
      runTurn(durable, entries, new RecordingProvider, turn, ledger)
      runTurn(durable, entries, new RecordingProvider, turn, ledger)
      ledger.rows.map(r => (r._1, r._2, r._3)) ==>
        Vector((Turn.replyId(turn), turn.workflowId, StubProvider.Model))
    }

    test("M0 gate: the same workflow id twice calls the provider once") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(durable, entries, provider, turn) ==> "replied: reply:c1:0"
      runTurn(durable, entries, provider, turn) ==> "replied: reply:c1:0"
      provider.requests.size ==> 1
      texts(entries).size ==> 2
    }

    test("a crash after the model call resumes without calling it again") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store)
      val durable = new InMemoryDurable
      val provider = new RecordingProvider
      val turn = say(store, "hello")
      assertThrows[InMemoryDurable.Crash](runTurn(durable, entries, provider, turn))
      durable.recordedSteps(turn.workflowId) ==> Vector("assemble", "call-model")
      runTurn(durable, entries, provider, turn) ==> "replied: reply:c1:0"
      provider.requests.size ==> 1
      durable.recordedSteps(turn.workflowId) ==> Vector("assemble", "call-model", "append")
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
