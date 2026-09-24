package grit.turn

import grit.assembly.LinearAssembler
import grit.core.*
import grit.dbos.TestTx
import grit.models.StubProvider
import java.time.Instant
import utest.*

object TurnTests extends TestSuite {

  private val conversation = ConversationId("c1")

  private val system = "you are a test"

  /** The stub provider, keeping every request it was sent. */
  final class RecordingProvider(fail: Boolean = false) extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      requests = requests :+ request
      if (fail) Left(ProviderError.Unavailable("down")) else new StubProvider().complete(request)
    }
  }

  /** Reads straight through to the in-memory store. */
  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** An entry store that dies, once, when a reply is appended. */
  final class CrashOnInsert(underlying: EntryStore) extends EntryStore {
    @caps.unsafe.untrackedCaptures
    var armed = true

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (armed) { armed = false; throw new InMemoryDurable.Crash }
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  /** Records `text` as the user message that starts the conversation's next turn. */
  private def say(entries: EntryStore, text: String): TurnRef = {
    given Tx = TestTx.fake
    val next = entries.lockNext(conversation).getOrElse(sys.error("in-memory store"))
    entries.insert(
      Entry(
        EntryId(s"in:$text"),
        conversation,
        next.turnSeq,
        None,
        next.seq,
        Payload.Message(Message.User(text)),
        Instant.EPOCH
      )
    )
    TurnRef(conversation, next.turnSeq)
  }

  private def texts(entries: EntryStore): Vector[String] =
    entries.list(conversation)(using TestTx.fake).getOrElse(Vector.empty).map {
      _.payload match {
        case Payload.Message(Message.User(text)) => s"user: $text"
        case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => s"assistant: $t" }.mkString
        case Payload.Message(other) => other.toString
      }
    }

  private def runTurn(
      durable: InMemoryDurable,
      entries: EntryStore,
      provider: Provider^,
      turn: TurnRef
  ): String =
    durable.run(turn.workflowId)(
      Turn.body(system, entries, new LinearAssembler(entries), provider, FakeDb)
    )

  val tests = Tests {
    test("a turn records the model's reply as its entry") {
      val entries = new InMemoryEntryStore
      val provider = new RecordingProvider
      val turn = say(entries, "hello")
      runTurn(new InMemoryDurable, entries, provider, turn) ==> "replied: reply:c1:0"
      texts(entries) ==> Vector("user: hello", "assistant: stub reply to: hello")
      provider.requests.map(_.system) ==> Vector(system)
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
        Turn.body(system, entries, new LinearAssembler(entries), new RecordingProvider, FakeDb)
      ) ==> "not a turn: proof"
    }
  }
}
