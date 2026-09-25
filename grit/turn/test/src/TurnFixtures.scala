package grit.turn

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{ConversationId, EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
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

  /** Entries a crash is aimed at: the reply's insert. */
  val isReply: Entry -> Boolean = _.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _)) => true
    case _ => false
  }

  /** The stub, noting as it is called whether `entries` already hold a window record. */
  final class Peeking(entries: EntryStore) extends Provider {
    @caps.unsafe.untrackedCaptures
    var sawWindow: Vector[Boolean] = Vector.empty

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      sawWindow = sawWindow :+ windows(entries).nonEmpty
      new StubProvider().complete(request)
    }
  }

  /** The stub, streaming; its first call tells `before` pieces of its reply and then dies,
    * as a crashed process would, mid-stream.
    */
  final class DiesMidStream(before: Int) extends Provider {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      new StubProvider().complete(request)

    override def stream(
        request: ModelRequest,
        onDelta: Delta => Unit
    ): Either[ProviderError, Message.Assistant] = {
      calls += 1
      var told = 0
      new StubProvider().stream(
        request,
        d => {
          if (calls == 1 && told == before) throw new InMemoryDurable.Crash
          told += 1
          onDelta(d)
        }
      )
    }
  }

  /** What a reader hears of `turn`'s reply stream in `durable`. */
  def heard(
      durable: InMemoryDurable,
      turn: TurnRef
  ): (Vector[TurnStream.Piece], TurnStream.Heard) = {
    val pieces =
      durable.streamed(turn.workflowId, TurnStream.Key).flatMap(TurnStream.decode(_).toOption)
    (pieces, pieces.foldLeft(TurnStream.Heard.nothing)(_ + _))
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

  /** The conversation as a transcript: every entry but the windows' records. */
  def texts(entries: EntryStore): Vector[String] =
    all(entries).flatMap {
      _.payload match {
        case Payload.Message(Message.User(text)) => Some(s"user: $text")
        case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
          Some(blocks.collect { case AssistantBlock.Text(t) => s"assistant: $t" }.mkString)
        case Payload.Message(other) => Some(other.toString)
        case Payload.Summary(text) => Some(s"summary: $text")
        case Payload.Query(text) => Some(s"query: $text")
        case Payload.Window(_, _) | Payload.Topic(_) => None
      }
    }

  /** The records of the turns' windows, with their ids. */
  def windows(entries: EntryStore): Vector[(EntryId, Payload.Window)] =
    all(entries).collect { case Entry(id, _, _, _, _, w: Payload.Window, _) => id -> w }

  private def all(entries: EntryStore): Vector[Entry] =
    entries.list(conversation)(using TestTx.fake).getOrElse(Vector.empty)

  /** The linear window, with `note` added: an assembler that says it wrote a query. */
  final class Noting(entries: EntryStore, note: AssemblyNote) extends ContextAssembler {
    def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
      new LinearAssembler(entries, CharEstimate, LinearAssembler.DefaultBudget)
        .assemble(request)
        .map(_.copy(notes = Vector(note)))
  }

  /** A query note, as retrieval would write it. */
  val queried: AssemblyNote.Queried =
    AssemblyNote.Queried(
      "hello greeting",
      "writer",
      Usage(Tokens(30), Tokens(4), Tokens.Zero, Some(BigDecimal("0.00001"))),
      Tokens(28)
    )

  /** The turn's workflow body over `entries` and `provider`, windowed by `assembler`. */
  def turnBodyWith(
      entries: EntryStore,
      provider: Provider^,
      assembler: ContextAssembler^,
      ledger: UsageLedger
  )(
      id: WorkflowId
  )(using Durable^): String =
    Turn.body(
      TurnEnv(
        system,
        TurnRecords(entries, ledger, CharEstimate),
        assembler,
        NoClassifier,
        provider,
        new StubProvider(),
        FakeDb,
        Clock.system(),
        Fresh.random()
      )
    )(id)

  /** The stub classifier, counting the questions it was asked, call by call. */
  final class CountingClassifier(fail: Boolean = false) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    /** Every state it was asked about, as sent. */
    @caps.unsafe.untrackedCaptures
    var states = Vector.empty[ujson.Value]

    protected def answer(
        state: ujson.Value,
        questions: Vector[grit.core.classify.Question]
    ): Either[grit.core.classify.ClassifierError, grit.core.classify.Answers] = {
      calls += 1
      states = states :+ state
      if (fail) Left(grit.core.classify.ClassifierError.Unavailable("HTTP 529: overloaded"))
      else Right(grit.models.StubClassifier.answers(state, questions))
    }
  }

  /** The conversation's topics as its entries leave them. */
  def topics(entries: EntryStore): grit.core.topic.Topics =
    grit.core.topic.Topics.fold(all(entries).flatMap(e => TurnTopics.events(e.payload)))

  /** No classifier: every message after a conversation's first is unclassified. */
  def NoClassifier: Classifier^ = Classifier.none("no classifier")

  /** The turn's workflow body over `entries` and `provider`, summarised by `summarizer`,
    * its messages placed by `classifier`.
    */
  def turnBody(
      entries: EntryStore,
      provider: Provider^,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider(),
      classifier: Classifier^ = NoClassifier
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      TurnEnv(
        system,
        TurnRecords(entries, ledger, CharEstimate),
        new LinearAssembler(entries, CharEstimate, LinearAssembler.DefaultBudget),
        classifier,
        provider,
        summarizer,
        FakeDb,
        Clock.system(),
        Fresh.random()
      )
    )(id)

  def runTurn(
      durable: InMemoryDurable,
      entries: EntryStore,
      provider: Provider^,
      turn: TurnRef,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider(),
      classifier: Classifier^ = NoClassifier
  ): String =
    durable.run(turn.workflowId)(turnBody(entries, provider, ledger, summarizer, classifier))
}
