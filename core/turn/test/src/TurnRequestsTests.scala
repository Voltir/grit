package grit.turn

import java.time.LocalDate

import grit.core.document.InMemoryDocuments
import grit.core.durable.{InMemoryDurable, StepRecord}
import grit.core.id.{ConversationId, EntryId, EntrySeq, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{
  AfterToolResult,
  Catalog,
  Known,
  Profile,
  Source,
  StrictSchemas,
  ToolGuidance
}
import grit.core.provider.ModelRequest
import grit.core.speech.Speaking
import grit.core.store.{
  Entry,
  EntryStore,
  InMemoryEntryStore,
  InMemoryModelProfileStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  ModelProfileStore,
  StoreError,
  Tx
}
import grit.core.triage.InMemoryTriageStore
import grit.dbos.sql.TestTx

import utest.*

/** A turn's model requests rebuilt after the fact from what it recorded
  * ([[TurnRecord.requests]]), against what its provider was sent: a turn run over core's
  * in-memory durability and stores, then read back from its steps.
  */
object TurnRequestsTests extends TestSuite {
  import TurnFixtures.*

  private given Tx = TestTx.fake

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

  private val files = Map("a.txt" -> "alpha", "b.txt" -> "beta")

  /** `id`'s steps as a reader of DBOS sees them: a marker or a throw records no output. */
  private def steps(durable: InMemoryDurable, id: WorkflowId): Vector[StepRecord] =
    durable
      .history(id)
      .map(s =>
        StepRecord(
          s.name,
          s.outcome match {
            case InMemoryDurable.Outcome.Output(value) => Some(value)
            case InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker => None
          },
          None
        )
      )

  /** The fixture turn's stores as a reader sees them, its entries `entries`. */
  private def reads(
      entries: EntryStore,
      profiles: ModelProfileStore = profilesKept()
  ): TurnRecord.Reads =
    TurnRecord.Reads(
      entries,
      new InMemoryPrincipals,
      new InMemoryDocuments,
      Prompts,
      ToolSets,
      profiles
    )

  /** `turn`, answered by `provider` with `peek` and `poke` offered, in at most `calls` calls,
    * its message's placement asked of `classifier`; its steps.
    */
  private def answered(
      entries: InMemoryEntryStore,
      turn: TurnRef,
      provider: grit.core.provider.Provider^,
      calls: Int = 5,
      classifier: grit.core.classify.Classifier^ = NoClassifier
  ): Vector[StepRecord] = {
    val durable = new InMemoryDurable
    val ws = new Files(files)
    durable.run(turn.workflowId)(
      tooledBody(
        entries,
        provider,
        new InMemoryUsageLedger,
        new grit.models.StubProvider(),
        classifier,
        ws,
        tools(ws),
        calls
      )
    )
    steps(durable, turn.workflowId)
  }

  /** A turn said "go", its models `catalog`'s, answered by `provider` with `peek` and `poke`
    * in at most two calls: the turn, its entries, the profiles it was pinned in, its steps.
    */
  private def pinnedRun(
      provider: grit.core.provider.Provider^,
      catalog: Catalog
  ): (TurnRef, InMemoryEntryStore, InMemoryModelProfileStore, Vector[StepRecord]) = {
    val entries = new InMemoryEntryStore
    val profiles = new InMemoryModelProfileStore
    val ws = new Files(files)
    val durable = new InMemoryDurable
    val turn = say(entries, "go")
    durable.run(turn.workflowId)(
      modelsBody(
        entries,
        new TurnModelsTests.Switching(provider, Right(catalog)),
        profiles,
        ws,
        tools(ws),
        calls = 2
      )
    )
    (turn, entries, profiles, steps(durable, turn.workflowId))
  }

  /** Each request rebuilt, as sent if the answer's `use` was `off` or not. */
  private def sent(calls: TurnRecord.Calls, off: Boolean): Vector[ModelRequest] =
    calls.looped.map(_.request) ++
      calls.answered.map(a => if (off) a.off else a.on).toVector

  /** An entry store that no longer holds the entries at `gone`. */
  private final class Purged(underlying: EntryStore, gone: Set[EntrySeq]) extends EntryStore {
    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
      underlying.get(id).map(_.filterNot(e => gone.contains(e.seq)))
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.list(c).map(_.filterNot(e => gone.contains(e.seq)))
    def at(c: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] =
      underlying.at(c, seqs).map(_.filterNot(e => gone.contains(e.seq)))
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.ofTurn(turn).map(_.filterNot(e => gone.contains(e.seq)))
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  val tests = Tests {
    test(
      "the requests rebuilt from a turn's record are those its provider was sent, call by call"
    ) {
      val entries = new InMemoryEntryStore
      val provider = new Scripted((_, n) =>
        Right(n match {
          case 0 => calling("looking", ("t1", "peek", "a.txt"))
          case 1 => calling("", ("t2", "peek", "b.txt"))
          case _ => calling("alpha and beta")
        })
      )
      val turn = say(entries, "read them")
      val recorded = answered(entries, turn, provider)
      val calls = TurnRecord.requests(turn, recorded, reads(entries))
      calls.map(c =>
        (c.looped.map(l => (l.round, l.reply)), c.answered.map(a => (a.round, a.reply)))
      ) ==>
        Right(
          (
            Vector((0, EntryId("call:c1:0:0")), (1, EntryId("call:c1:0:1"))),
            Some((2, turn.replyId))
          )
        )
      calls.map(sent(_, off = false)) ==> Right(provider.requests)
      calls.map(_.schemas) ==> Right(TurnRecord.Schemas.Sent)
    }

    test("a turn's budget's last call is rebuilt as sent with tools off") {
      val entries = new InMemoryEntryStore
      val provider = new Scripted((_, _) => Right(calling("more", ("t", "peek", "a.txt"))))
      val turn = say(entries, "go")
      val recorded = answered(entries, turn, provider, calls = 2)
      val calls = TurnRecord.requests(turn, recorded, reads(entries))
      calls.map(sent(_, off = true)) ==> Right(provider.requests)
      calls.map(_.answered.map(_.on) == provider.requests.lastOption) ==> Right(false)
    }

    test("a turn asked about its topic is rebuilt with its first call tagged and topic offered") {
      val entries = new InMemoryEntryStore
      val classifier = new CountingClassifier
      Vector("hello", "knots? ~0.1").foreach(text =>
        runTurn(
          new InMemoryDurable,
          entries,
          new RecordingProvider,
          say(entries, text),
          classifier = classifier
        )
      )
      val provider = new Scripted((r, n) =>
        if (n == 0)
          Right(
            calling("").copy(
              blocks = Vector(
                AssistantBlock.ToolCall(ToolCallId("v"), "topic", ujson.Obj("about" -> "current")),
                AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj("path" -> "a.txt"))
              ),
              stop = StopReason.ToolUse
            )
          )
        else new grit.models.StubProvider().complete(r)
      )
      val turn = say(entries, "hm ~0.5")
      val recorded = answered(entries, turn, provider, classifier = classifier)
      // The setting: the first call was tagged and offered topic.
      provider.requests.headOption.map(_.tools.map(_.name)) ==> Some(
        Vector("topic", "peek", "poke")
      )
      val calls = TurnRecord.requests(turn, recorded, reads(entries))
      calls.map(sent(_, off = false)) ==> Right(provider.requests)
    }

    test("a heard turn's answer is its draft, rebuilt as its provider was sent it") {
      val w = speechWorld()
      val provider = new Scripted((_, _) => Right(said("It moved to Thursday.")))
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          provider,
          new Before(w.entries),
          w.ledger,
          speech = TurnSpeech(Speaking.Within(speechLimits), w.store, w.deliveries),
          weighing = TurnWeighing(
            new InMemoryTriageStore(w.entries, NoPeriods),
            new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
          )
        )
      )
      val calls = TurnRecord.requests(w.turn, steps(durable, w.turn.workflowId), reads(w.entries))
      calls.map(_.answered.map(a => (a.reply, a.on))) ==>
        Right(Some((w.turn.draftId, provider.requests.head)))
    }

    test("a turn whose window names an entry since deleted is refused, naming its seq") {
      val w = speechWorld()
      val durable = new InMemoryDurable
      durable.run(w.turn.workflowId)(
        turnBodyWith(
          w.entries,
          new Scripted((_, _) => Right(said("It moved to Thursday."))),
          new Before(w.entries),
          w.ledger,
          speech = TurnSpeech(Speaking.Within(speechLimits), w.store, w.deliveries)
        )
      )
      val purged = new Purged(w.entries, Set(EntrySeq(0)))
      TurnRecord.requests(w.turn, steps(durable, w.turn.workflowId), reads(purged)) ==>
        Left("window names unknown entries at seqs: 0")
    }

    test("a turn guided in its system prompt and told in its last result is rebuilt as sent") {
      val nick = Source.Declared("nick", LocalDate.of(2026, 9, 25))
      val pair = Profile(
        TestCatalog.policy.turn.ref,
        afterResult = Known.Of(AfterToolResult.InLastResult, nick),
        guidance = Known.Of(ToolGuidance.SystemLines, nick)
      )
      val provider = new Scripted((_, _) => Right(calling("more", ("t", "peek", "a.txt"))))
      val (turn, entries, profiles, recorded) =
        pinnedRun(provider, TestCatalog.overlaid(Vector(pair)))
      val calls = TurnRecord.requests(turn, recorded, reads(entries, profiles))
      // These models send the summary to the same provider: its third request.
      calls.map(sent(_, off = true)) ==> Right(provider.requests.take(2))
    }

    test("a turn under a pair that enforces strict schemas says its schemas are recorded") {
      val pair = Profile(
        TestCatalog.policy.turn.ref,
        strict = Known.Of(
          StrictSchemas.Enforced,
          Source.Declared("nick", LocalDate.of(2026, 9, 25))
        )
      )
      val provider = new Scripted((_, _) => Right(calling("done")))
      val (turn, entries, profiles, recorded) =
        pinnedRun(provider, TestCatalog.overlaid(Vector(pair)))
      TurnRecord.requests(turn, recorded, reads(entries, profiles)).map(_.schemas) ==>
        Right(TurnRecord.Schemas.Recorded)
    }
  }
}
