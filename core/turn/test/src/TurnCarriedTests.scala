package grit.turn

import java.time.Instant

import grit.core.durable.{FlowFixtures, InMemoryDurable}
import grit.core.edge.InMemoryEdges
import grit.core.id.{EntryId, EntrySeq, PeriodSeq, ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.model.{Assignment, Catalog, Grade, Ladder, Rung}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.provider.{ModelRequest, ProviderError}
import grit.core.store.{Entry, EntryStore, InMemoryEntryStore, InMemoryUsageLedger, Payload, Tx}
import grit.core.tool.{Believed, Outcome, Toolbox}
import grit.core.visibility.Integrity
import grit.dbos.sql.TestTx
import grit.models.StubProvider

import utest.*
import TurnFixtures.*
import TurnBasis.Why

/** Why a turn drew its window afresh, or the record it carried, as its `carried` step records
  * it and [[TurnRecord.carried]] reads it back: one test per reason.
  */
object TurnCarriedTests extends TestSuite {

  /** What `turn`'s `carried` step recorded, read back from its history. */
  private def basis(durable: InMemoryDurable, turn: TurnRef): Either[String, Option[TurnBasis]] =
    TurnRecord.carried(durable.history(turn.workflowId).map(FlowFixtures.recorded))

  /** `text` said and answered by the stub, on `durable`. */
  private def answered(
      entries: EntryStore,
      durable: InMemoryDurable,
      text: String,
      fail: Boolean = false
  ): TurnRef = {
    val turn = say(entries, text)
    durable.run(turn.workflowId)(turnBody(entries, new RecordingProvider(fail)))
    turn
  }

  /** `payload` kept as `of`'s, newer than every entry so far. */
  private def keep(entries: EntryStore, of: TurnRef, id: String, payload: Payload): Unit = {
    given Tx = TestTx.fake
    val next = entries.lockNext(conversation).getOrElse(sys.error("in-memory store"))
    val _ = entries.insert(
      Entry(
        EntryId(id),
        conversation,
        of.turnSeq,
        None,
        next.seq,
        payload,
        Instant.EPOCH,
        Integrity.Member
      )
    )
  }

  private def boundary(entries: Vector[EntrySeq]): Payload.Window =
    Payload.Window(entries, Vector.empty, carry = Some(Payload.Carry(None, Tokens(1), None, None)))

  val tests = Tests {
    test("a conversation's first turn draws afresh: no earlier turn kept a window") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      basis(durable, first) ==> Right(Some(TurnBasis.Afresh(Why.NoWindow)))
    }

    test("a turn after one answered under the same tools carries that turn's window record") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Against(Turn.windowId(first))))
    }

    test("a turn after one whose call failed draws afresh: that turn kept no prefix") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      answered(entries, durable, "one", fail = true)
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.NoPrefix)))
    }

    test(
      "a turn offered other tools than the last draws afresh: its prefix differs; one offered the same carries it"
    ) {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val ws = new Files(Map("a.txt" -> "alpha"))
      def run(text: String, gated: Boolean) = {
        val t = say(entries, text)
        durable.run(t.workflowId)(
          tooledBody(
            entries,
            new StubProvider(),
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            if (gated) gatedTools(ws) else tools(ws),
            3
          )
        )
        t
      }
      run("one", gated = false)
      val second = run("two", gated = true)
      val third = run("three", gated = true)
      (basis(durable, second), basis(durable, third)) ==> (
        Right(Some(TurnBasis.Afresh(Why.PrefixDiffers))),
        Right(Some(TurnBasis.Against(Turn.windowId(second))))
      )
    }

    test("a turn sure of its topic after one asked about it carries that turn's window record") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val classifier = new CountingClassifier
      def run(text: String) = {
        val t = say(entries, text)
        durable.run(t.workflowId)(
          turnBody(entries, new RecordingProvider, classifier = classifier)
        )
        t
      }
      val _ = Vector("hello", "knots? ~0.1").map(run)
      val asked = run("hm ~0.5")
      basis(durable, run("more ~0.9")) ==> Right(Some(TurnBasis.Against(Turn.windowId(asked))))
    }

    test("a turn after a period closed since the last turn's window draws afresh: a boundary") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      keep(
        entries,
        first,
        "closed",
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("Said one."))
      )
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.Boundary)))
    }

    test("a turn routed to another pair than the last draws afresh: the pair differs") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Now(Outcome.Done("")))
      // TestCatalog with its turn role at another model: the same bytes go to another pair.
      val regraded = Catalog.of(
        TestCatalog.policy.copy(turn =
          Ladder
            .of(Vector(Rung(Grade.Standard, Assignment(pair("test/other", None), 4096, None))))
            .getOrElse(throw new java.lang.AssertionError())
        ),
        Vector.empty
      )
      def run(text: String, catalog: Catalog) = {
        val t = say(entries, text)
        durable.run(t.workflowId)(
          hostedBody(entries, new RecordingProvider, edge, hosted = Vector.empty, catalog = catalog)
        )
        t
      }
      val _ = run("one", TestCatalog)
      basis(durable, run("two", regraded)) ==> Right(Some(TurnBasis.Afresh(Why.PairDiffers)))
    }

    test("a turn whose first gate would hide a result the window carries draws afresh") {
      // fetch's result is believed at untrusted; post requires outside, so a later turn's
      // first gate hides the carried result to keep post offered.
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val box = Toolbox
        .of[caps.CapSet^{NoCheckout}](
          fetchBelieved(Believed.At(Integrity.Untrusted)).over(_ => Outcome.Done("alpha")),
          hostedPost.requiring(Integrity.Outside).over(_ => Outcome.Done("posted"))
        )
        .fold(d => throw new java.lang.AssertionError(d), identity)
      def run(
          text: String,
          script: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant]
      ) = {
        val t = say(entries, text)
        durable.run(t.workflowId)(
          tooledBody(
            entries,
            new Scripted(script),
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            NoCheckout,
            box,
            3
          )
        )
        t
      }
      run(
        "fetch a.txt",
        (r, n) =>
          if (n == 0)
            Right(
              said(
                "",
                AssistantBlock.ToolCall(ToolCallId("t1"), "fetch", ujson.Obj("path" -> "a.txt"))
              )
            )
          else new StubProvider().complete(r)
      )
      val second = run("post it", (r, _) => new StubProvider().complete(r))
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.Hidden)))
    }

    test("a turn after a window record that kept no carry draws afresh: no carry") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      keep(
        entries,
        first,
        "old-window",
        Payload.Window(Vector(EntrySeq(0)), Vector.empty, carry = None)
      )
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.NoCarry)))
    }

    test("a turn after a window record appended after one not read draws afresh: unchained") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      keep(
        entries,
        first,
        "orphan-window",
        Payload.Window(
          Vector(EntrySeq(0)),
          Vector.empty,
          carry = Some(Payload.Carry(Some(EntryId("gone")), Tokens(1), None, None))
        )
      )
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.Unchained)))
    }

    test("a turn after a window record naming an entry since gone draws afresh: unrendered") {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable
      val first = answered(entries, durable, "one")
      keep(entries, first, "gappy-window", boundary(Vector(EntrySeq(0), EntrySeq(999))))
      val second = answered(entries, durable, "two")
      basis(durable, second) ==> Right(Some(TurnBasis.Afresh(Why.Unrendered)))
    }
  }
}
