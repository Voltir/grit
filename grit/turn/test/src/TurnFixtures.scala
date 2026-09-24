package grit.turn

import java.time.Instant

import grit.assembly.{CharEstimate, LinearAssembler}
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{ConversationId, EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{
  Db,
  Entry,
  EntryStore,
  InMemoryUsageLedger,
  Payload,
  StoreError,
  Tx,
  UsageLedger
}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

/** The turn's test world: the stub provider, the linear assembler, and core's in-memory
  * store and durability fakes. Shared by the turn tests, the replay gate and the recorder.
  */
object TurnFixtures {

  val conversation = ConversationId("c1")

  val system = "you are a test"

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

  /** An entry store that dies, once, on the first insert of an entry `when` picks: by
    * default the reply, the first entry a turn inserts.
    */
  final class CrashOnInsert(underlying: EntryStore, when: Entry -> Boolean = _ => true)
      extends EntryStore {
    @caps.unsafe.untrackedCaptures
    var armed = true

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (armed && when(entry)) { armed = false; throw new InMemoryDurable.Crash }
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  /** Records `text` as the user message that starts the conversation's next turn. */
  def say(entries: EntryStore, text: String): TurnRef = {
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

  def texts(entries: EntryStore): Vector[String] =
    entries.list(conversation)(using TestTx.fake).getOrElse(Vector.empty).map {
      _.payload match {
        case Payload.Message(Message.User(text)) => s"user: $text"
        case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => s"assistant: $t" }.mkString
        case Payload.Message(other) => other.toString
        case Payload.Summary(text) => s"summary: $text"
      }
    }

  /** The turn's workflow body over `entries` and `provider`, summarised by `summarizer`. */
  def turnBody(
      entries: EntryStore,
      provider: Provider^,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider()
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      system,
      entries,
      ledger,
      new LinearAssembler(entries, CharEstimate, LinearAssembler.DefaultBudget),
      CharEstimate,
      provider,
      summarizer,
      FakeDb
    )(id)

  def runTurn(
      durable: InMemoryDurable,
      entries: EntryStore,
      provider: Provider^,
      turn: TurnRef,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider()
  ): String =
    durable.run(turn.workflowId)(turnBody(entries, provider, ledger, summarizer))
}
