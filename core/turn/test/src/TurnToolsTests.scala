package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.id.{
  ClaimHolder,
  ConversationId,
  EntryId,
  EntrySeq,
  Harness,
  PrincipalId,
  SessionId,
  TestCallSlots,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.identity.Principal
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.{Place, Service}
import grit.core.session.{Event, Logged, Session}
import grit.core.store.{Askers, Entry, EntryStore, InMemoryEntryStore, Jot, Payload, StoreError, Tx}
import grit.core.tool.{
  Args,
  Believed,
  Bound,
  Field,
  Gate,
  Hosted,
  Outcome,
  Repairs,
  Tool,
  ToolName,
  ToolSpec,
  Toolbox
}
import grit.core.visibility.{
  Accounted,
  Cap,
  Compartments,
  Integrity,
  Label,
  Provenance,
  Subject,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*
import TurnLoop.Pending

/** [[TurnTools.Settling]]: what a call's step runs, and what it runs again after a crash. Free
  * tools may run twice; a gated one, approved, never does; a kept result is never run again.
  */
object TurnToolsTests extends TestSuite {
  import TurnFixtures.*

  private val files = Map("a.txt" -> "alpha")

  private def call(name: String): Pending =
    Pending.Run(AssistantBlock.ToolCall(ToolCallId("c"), name, ujson.Obj("path" -> "a.txt")))

  /** `pending` settled as the turn settles it, at the first call of the first round: a gated
    * call decided by `approval`.
    */
  private def settle(
      entries: EntryStore,
      ws: Files^,
      turn: TurnRef,
      pending: Pending,
      approval: Approval = Approval.TimedOut
  ): Either[TurnFailure, TurnTools.Settled] = {
    val slot = TurnTools.Slot(TestCallSlots.at(turn))
    val id = pending.call.id
    val settling = settlingIn(new FakeJot, entries)
    TurnTools.read(tools(ws), pending, Repairs.All, PublicReads, Integrity.Authority) match {
      case Left(outcome) =>
        val unrun = Left(Integrity.Untrusted)
        settling.answer(slot, id, pending.call.name, outcome, unrun, Instant.EPOCH)
      case Right(free: Bound.Free) => settling.run(slot, id, free, Integrity.Member, Instant.EPOCH)
      case Right(gated: Bound.Gated) =>
        settling.decide(slot, id, gated, approval, Integrity.Member, Instant.EPOCH)
      case Right(_: Bound.Hosted) => Left(TurnFailure.Model("no hosted tools here"))
      case Right(_: Bound.Keeping) => Left(TurnFailure.Model("no keeping tools here"))
    }
  }

  /** A jot whose writes through [[stage]] stand only when the write they are made in commits:
    * on a `Right`, as [[grit.core.store.Jot]] says; a write that throws or fails keeps none.
    */
  private final class Staged extends Jot {
    // Each holds an immutable value, written and read on the test's one thread.
    @caps.unsafe.untrackedCaptures
    var kept: Vector[String] = Vector.empty
    @caps.unsafe.untrackedCaptures
    private var staged: Vector[String] = Vector.empty
    def stage(name: String): Unit = staged = staged :+ name
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
      staged = Vector.empty
      try {
        val result = body(using TestTx.fake)
        if (result.isRight) kept = kept ++ staged
        result
      } finally staged = Vector.empty
    }
  }

  /** `start`, a keeping tool that keeps the name it is given through `jot`, refusing a name
    * already kept, as `project_new` refuses a key a project has, after writing it again.
    */
  private def start(jot: Staged): Tool[String]^{jot} =
    Hosted.keeping(
      ToolSpec(ToolName("start"), "Starts.", Args.of((path = Field.text("What."))).map(_.path)),
      (p: String) => p,
      Integrity.Untrusted,
      Believed.Own
    ) { (p, _, _) =>
      val exists = jot.kept.contains(p)
      jot.stage(p)
      Right(if (exists) Outcome.Failed(s"$p exists already") else Outcome.Done(s"started $p"))
    }

  /** A call of [[start]] with `p`, settled as the turn settles it through `jot`, at the first
    * call of the first round.
    */
  private def keep(entries: EntryStore, jot: Staged, turn: TurnRef, p: String) = {
    val slot = TurnTools.Slot(TestCallSlots.at(turn))
    val sent =
      Pending.Run(AssistantBlock.ToolCall(ToolCallId("c"), "start", ujson.Obj("path" -> p)))
    val box = Toolbox
      .of[caps.CapSet^{jot}](start(jot))
      .fold(d => throw new java.lang.AssertionError(d), identity)
    TurnTools.read(box, sent, Repairs.All, PublicReads, Integrity.Authority) match {
      case Right(keeping: Bound.Keeping) =>
        settlingIn(jot, entries)
          .keepFrom(
            slot,
            ToolCallId("c"),
            keeping,
            jot.as(Subject.Turn(turn)),
            Integrity.Member,
            Instant.EPOCH
          )
      case other => throw new java.lang.AssertionError(s"not keeping: $other")
    }
  }

  private def kept(entries: InMemoryEntryStore, id: EntryId): Option[Payload] =
    entries.get(id)(using TestTx.fake).toOption.flatten.map(_.payload)

  private def isResult(e: Entry): Boolean = EntryId.value(e.id).startsWith("result:")

  /** What a call of `tool`, free and pure, is kept believed at, settled as the turn settles it. */
  private def ranAt(tool: Tool[String]): Either[TurnFailure, Option[Integrity]] = {
    val entries = new InMemoryEntryStore
    val turn = say(entries, "go")
    val slot = TurnTools.Slot(TestCallSlots.at(turn))
    val settling = settlingIn(new FakeJot, entries)
    val box = Toolbox.of[{}](tool).fold(d => throw new java.lang.AssertionError(d), identity)
    val sent = Pending.Run(AssistantBlock.ToolCall(ToolCallId("c"), "said", ujson.Obj("p" -> "x")))
    TurnTools.read(box, sent, Repairs.All, PublicReads, Integrity.Authority) match {
      case Right(free: Bound.Free) =>
        settling
          .run(slot, ToolCallId("c"), free, Integrity.Member, Instant.EPOCH)
          .map(_ => believed(entries, slot.resultId))
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }
  }

  /** `said`, free, answering `outcome`, its results believed at [[Integrity.Outside]], below
    * the turn's [[Integrity.Member]] that [[ranAt]] runs it at.
    */
  private def said(outcome: Outcome): Tool[String] =
    new Hosted(
      ToolSpec(ToolName("said"), "Says.", Args.of((p = Field.text("What."))).map(_.p)),
      Gate.Free,
      (p: String) => p,
      Believed.At(Integrity.Outside)
    ).over(_ => outcome)

  /** What a kept entry is believed at. */
  /** `said` as the engine offers it, logging nothing. */
  private val unlogged: Hosted[String] =
    new Hosted(
      ToolSpec(ToolName("said"), "Says.", Args.of((p = Field.text("What."))).map(_.p)),
      Gate.Free,
      (p: String) => p,
      Believed.Default
    )

  /** `said` as the engine offers it, logged as a read of the path it is given. */
  private val logged: Hosted[String] =
    unlogged.logging((p, _, _) => Some(Logged.Read(Logged.shaped(Event.Shape.Path, p))))

  /** The place every call [[logging]] answers was sent to. */
  private val sentTo: Place = Place.under(grit.core.place.Namespace.Fs, Vector("/work/grit"))

  private val holder = ClaimHolder.of("coder/abc").fold(sys.error, identity)

  /** `tool` called once with `x`, sent to [[sentTo]] and answered `outcome` by its edge, settled
    * as the turn settles it, twice when `again`, its session recorded into `spy` working as
    * [[holder]], its asker as `askers` reads it: what each settling came to, and the entries
    * kept. Unless `said`, the turn's call is the conversation's first entry, at seq 0, which no
    * event's ordinal may be.
    */
  private def logging(
      tool: Hosted[String],
      outcome: Outcome,
      spy: RecordedSessions,
      askers: Askers = LocalAsker,
      again: Boolean = false,
      said: Boolean = true
  ): (Vector[Either[TurnFailure, TurnTools.Settled]], InMemoryEntryStore) = {
    val entries = new InMemoryEntryStore
    val turn = if (said) say(entries, "go") else TurnRef(conversation, TurnSeq.First)
    val slot = TurnTools.Slot(TestCallSlots.at(turn))
    val sessions = TurnSessions(spy, _ => Some(holder))
    val settling = settlingIn(new FakeJot, entries, sessions, hosting().copy(askers = askers))
    val box = Toolbox.of[{}](tool).fold(d => throw new java.lang.AssertionError(d), identity)
    val sent = Pending.Run(AssistantBlock.ToolCall(ToolCallId("c"), "said", ujson.Obj("p" -> "x")))
    val runs = Vector.fill(if (again) 2 else 1)(
      TurnTools.read(box, sent, Repairs.All, PublicReads, Integrity.Authority) match {
        case Right(hosted: Bound.Hosted) =>
          settling.answer(
            slot,
            ToolCallId("c"),
            hosted.shown,
            outcome,
            Left(Integrity.Member),
            Instant.EPOCH,
            text => hosted.logs(sentTo, text)
          )
        case other => throw new java.lang.AssertionError(s"not hosted: $other")
      }
    )
    (runs, entries)
  }

  private def believed(entries: InMemoryEntryStore, id: EntryId): Option[Integrity] =
    entries.get(id)(using TestTx.fake).toOption.flatten.map(_.integrity)

  val tests = Tests {
    test(
      "a hosted call's result is believed at its service's declared output, else at the integrity given"
    ) {
      val github = Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)
      val declaring = Visibility
        .of(
          Compartments.Shipped,
          Visibility.Shipped.rooms,
          Vector.empty,
          Vector.empty,
          caps = Vector(Cap(github.place, Integrity.Outside))
        )
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      val entries = new InMemoryEntryStore
      val turn = say(entries, "go")
      val settling = settlingIn(new FakeJot(visibility = declaring), entries)
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
      val hosted =
        settling.answer(
          slot,
          ToolCallId("c"),
          "issue_read",
          Outcome.Done("an issue", Some(Integrity.Authority)),
          Right(github),
          Instant.EPOCH
        )
      hosted.map(_ => believed(entries, slot.resultId)) ==> Right(Some(Integrity.Outside))
      val other = new InMemoryEntryStore
      val otherTurn = say(other, "go")
      val otherSlot = TurnTools.Slot(TestCallSlots.at(otherTurn))
      settlingIn(new FakeJot, other)
        .answer(
          otherSlot,
          ToolCallId("c"),
          "read",
          Outcome.Done("a file"),
          Left(Integrity.Untrusted),
          Instant.EPOCH
        )
        .map(_ => believed(other, otherSlot.resultId)) ==> Right(Some(Integrity.Untrusted))
    }

    test(
      "a hosted call's result is believed at the meet of its items as its places' caps cap them"
    ) {
      val github = Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)
      val grit = Place.read("service:github/octo/grit").fold(sys.error, identity)
      val docs = Place.read("service:github/octo/docs").fold(sys.error, identity)
      val declaring = Visibility
        .of(
          Compartments.Shipped,
          Visibility.Shipped.rooms,
          Vector.empty,
          Vector.empty,
          caps = Vector(
            Cap(github.place, Integrity.Untrusted),
            Cap(grit, Integrity.Member),
            Cap(docs, Integrity.Outside)
          )
        )
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      val entries = new InMemoryEntryStore
      val turn = say(entries, "go")
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
      settlingIn(new FakeJot(visibility = declaring), entries)
        .answer(
          slot,
          ToolCallId("c"),
          "search_issues",
          Outcome.Done(
            "two issues",
            None,
            Accounted.of(
              Vector(Provenance(grit, Integrity.Authority), Provenance(docs, Integrity.Member))
            )
          ),
          Right(github),
          Instant.EPOCH
        )
        .map(_ => believed(entries, slot.resultId)) ==> Right(Some(Integrity.Outside))
    }

    test("an own tool's result is believed at what its outcome says, else at its tool's") {
      ranAt(said(Outcome.Done("x"))) ==> Right(Some(Integrity.Outside))
      ranAt(said(Outcome.Done("x", Some(Integrity.Untrusted)))) ==> Right(Some(Integrity.Untrusted))
      ranAt(said(Outcome.Failed("no"))) ==> Right(Some(Integrity.Outside))
    }

    test(
      "a call's kind says which steps settle it, a hosted one's its tool and that tool's place"
    ) {
      val ws = new Files(files)
      val jot = new Staged
      val box = Toolbox
        .of[caps.CapSet^{ws, jot}](peek(ws), poke(ws), hostedFetch, hostedProd, start(jot))
        .fold(d => throw new java.lang.AssertionError(d), identity)
      def kind(name: String): TurnTools.Kind =
        TurnTools.Kind.of(
          TurnTools.read(box, call(name), Repairs.All, PublicReads, Integrity.Authority),
          tool => Option.when(ToolName.value(tool) == "fetch")(Place.Everywhere)
        )
      // A keeping call is Free: its steps and their names are a free call's (ADR 0004).
      Vector("nothing", "peek", "poke", "fetch", "prod", "start").map(kind) ==> Vector(
        TurnTools.Kind.Refused,
        TurnTools.Kind.Free,
        TurnTools.Kind.Gated,
        TurnTools.Kind.Hosted(ToolName("fetch"), asks = false, Some(Place.Everywhere)),
        TurnTools.Kind.Hosted(ToolName("prod"), asks = true, None),
        TurnTools.Kind.Free
      )
    }

    test("a free call runs, and its result is kept under its fixed id") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val id = TurnTools.Slot(TestCallSlots.at(turn)).resultId
      settle(entries, ws, turn, call("peek")) ==> Right(TurnTools.Settled(id, failed = false))
      kept(entries, id) ==>
        Some(
          Payload.Result(
            Message.ToolResult(ToolCallId("c"), "alpha", isError = false),
            "peek a.txt"
          )
        )
    }

    test(
      "a logged call that ran is its session's event at its result's seq, as its asker, at its conversation's label and place, working as its holder"
    ) {
      val spy = new RecordedSessions
      val (runs, entries) = logging(logged, Outcome.Done("héllo"), spy)
      val result = entries.get(runs.head.map(_.result).getOrElse(EntryId("none")))(using
        TestTx.fake
      )
      val seq = result.toOption.flatten.map(e => EntrySeq.value(e.seq))
      val expected = for {
        id <- SessionId.of(Harness.Grit, ConversationId.value(conversation))
        n <- seq.toRight("no result")
        event <- Logged.Read(Logged.shaped(Event.Shape.Path, "x")).event(n, Instant.EPOCH, "héllo")
      } yield (
        Session.Header(
          id,
          None,
          Principal.Person(PrincipalId.Local, Set.empty),
          Label.Public,
          origin.place,
          Some(holder)
        ),
        Vector(event)
      )
      assert(runs.forall(_.isRight), expected.map(Vector(_)) == Right(spy.recorded))
    }

    test("a call that did not run to done, or whose asker is no person, logs nothing") {
      val failed = new RecordedSessions
      val unasked = new RecordedSessions
      val silent = new RecordedSessions
      logging(logged, Outcome.Failed("no"), failed)
      logging(logged, Outcome.Done("ok"), unasked, askedBy(None))
      logging(unlogged, Outcome.Done("ok"), silent)
      assert(failed.recorded.isEmpty, unasked.recorded.isEmpty, silent.recorded.isEmpty)
    }

    test("a session kept where the turn does not read costs the call nothing") {
      val hidden = new RecordedSessions(Left(StoreError.Invalid("kept where this does not read")))
      val broken = new RecordedSessions(Left(StoreError.DatabaseError("down")))
      val (settled, keptIn) = logging(logged, Outcome.Done("ok"), hidden)
      val (failed, _) = logging(logged, Outcome.Done("ok"), broken)
      assert(
        settled.map(_.map(_.failed)) == Vector(Right(false)),
        settled.head.toOption.flatMap(s => kept(keptIn, s.result)).isDefined,
        failed.forall(_.isLeft)
      )
    }

    test("a logged call whose event cannot be made keeps its result and records nothing") {
      val spy = new RecordedSessions
      val (runs, entries) = logging(logged, Outcome.Done("ok"), spy, said = false)
      assert(
        runs.map(_.map(_.failed)) == Vector(Right(false)),
        runs.head.toOption.flatMap(s => kept(entries, s.result)).isDefined,
        spy.recorded.isEmpty
      )
    }

    test("a logged call settled again records nothing more") {
      val spy = new RecordedSessions
      val (runs, _) = logging(logged, Outcome.Done("ok"), spy, again = true)
      assert(runs.distinct.size == 1, spy.recorded.size == 1)
    }

    test("a kept result is returned as it is, and nothing runs again") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val first = settle(entries, ws, turn, call("peek"))
      settle(entries, ws, turn, call("peek")) ==> first
      settle(entries, ws, turn, call("poke"), Approval.Approved) ==> first
      ws.reads ==> 1
    }

    test(
      "a crash before a keeping call's result is kept keeps nothing, so its rerun keeps afresh"
    ) {
      val store = new InMemoryEntryStore
      val jot = new Staged
      val turn = say(store, "go")
      val entries = new CrashOnInsert(store, isResult)
      assertThrows[InMemoryDurable.Crash](keep(entries, jot, turn, "p"))
      val rerun = keep(entries, jot, turn, "p")
      (
        rerun.map(_.failed),
        jot.kept,
        kept(store, TurnTools.Slot(TestCallSlots.at(turn)).resultId)
      ) ==> (
        Right(false),
        Vector("p"),
        Some(Payload.Result(Outcome.Done("started p").result(ToolCallId("c")), "start p"))
      )
    }

    test("a keeping call that does not answer done keeps nothing it wrote, and its outcome") {
      val store = new InMemoryEntryStore
      val jot = new Staged
      val turn = say(store, "go")
      jot.kept = Vector("p")
      val refused = keep(store, jot, turn, "p")
      (
        refused.map(_.failed),
        jot.kept,
        kept(store, TurnTools.Slot(TestCallSlots.at(turn)).resultId)
      ) ==> (
        Right(true),
        Vector("p"),
        Some(Payload.Result(Outcome.Failed("p exists already").result(ToolCallId("c")), "start p"))
      )
    }

    test("a crash between a free call's run and its result runs it again") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, isResult)
      val ws = new Files(files)
      val turn = say(store, "go")
      assertThrows[InMemoryDurable.Crash](settle(entries, ws, turn, call("peek")))
      settle(entries, ws, turn, call("peek")).map(_.failed) ==> Right(false)
      ws.reads ==> 2
    }

    test("a crash between an approved gated call's run and its result: interrupted, not rerun") {
      val store = new InMemoryEntryStore
      val entries = new CrashOnInsert(store, isResult)
      val ws = new Files(files)
      val turn = say(store, "go")
      val approved = Approval.Approved
      assertThrows[InMemoryDurable.Crash](settle(entries, ws, turn, call("poke"), approved))
      ws.reads ==> 1
      kept(store, TurnTools.Slot(TestCallSlots.at(turn)).attemptId) ==>
        Some(Payload.Attempt(ToolCallId("c")))
      settle(entries, ws, turn, call("poke"), approved).map(_.failed) ==> Right(true)
      ws.reads ==> 1
      kept(store, TurnTools.Slot(TestCallSlots.at(turn)).resultId) ==>
        Some(Payload.Result(Outcome.Interrupted.result(ToolCallId("c")), "poke a.txt"))
    }

    test("a gated call declined or timed out does not run") {
      val ws = new Files(files)
      val answers = Vector(
        Approval.Declined(Some("not now")) -> Outcome.Declined(Some("not now")),
        Approval.TimedOut -> Outcome.Unanswered
      )
      answers.foreach { (approval, expected) =>
        val entries = new InMemoryEntryStore
        val turn = say(entries, "go")
        settle(entries, ws, turn, call("poke"), approval)
        kept(entries, TurnTools.Slot(TestCallSlots.at(turn)).resultId) ==>
          Some(Payload.Result(expected.result(ToolCallId("c")), "poke a.txt"))
        kept(entries, TurnTools.Slot(TestCallSlots.at(turn)).attemptId) ==> None
      }
      ws.reads ==> 0
    }

    test(
      "a call declined with a reason keeps a result saying only that it was declined, and the reason as its own entry after it"
    ) {
      val ws = new Files(files)
      val entries = new InMemoryEntryStore
      val turn = say(entries, "go")
      val slot = TurnTools.Slot(TestCallSlots.at(turn))
      settle(entries, ws, turn, call("poke"), Approval.Declined(Some("not\nthat file")))
      kept(entries, slot.resultId) ==> Some(
        Payload.Result(
          Message.ToolResult(
            ToolCallId("c"),
            "Declined: the person you are answering declined this call; it did not run.",
            isError = true
          ),
          "poke a.txt"
        )
      )
      kept(entries, slot.declinedId) ==> Some(Payload.Declined(ToolCallId("c"), "not\nthat file"))
      def seq(id: EntryId): Option[Long] = entries
        .list(conversation)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .find(_.id == id)
        .map(e => grit.core.id.EntrySeq.value(e.seq))
      seq(slot.declinedId) ==> seq(slot.resultId).map(_ + 1)
      // Declined with no reason, nothing more is kept.
      val quiet = new InMemoryEntryStore
      val again = say(quiet, "go")
      settle(quiet, ws, again, call("poke"), Approval.Declined(None))
      kept(quiet, TurnTools.Slot(TestCallSlots.at(again)).declinedId) ==> None
      ws.reads ==> 0
    }

    test("a refused call is answered with its outcome, and nothing runs") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val refused = Pending.Refused(
        AssistantBlock.ToolCall(ToolCallId("c"), "peek", ujson.Obj("path" -> "a.txt")),
        TurnLoop.CutOff
      )
      settle(entries, ws, turn, refused).map(_.failed) ==> Right(true)
      ws.reads ==> 0
    }

    test("a call that does not read is shown by the tool's name as it was sent") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val id = TurnTools.Slot(TestCallSlots.at(turn)).resultId
      val unknown: AssistantBlock.ToolCall =
        AssistantBlock.ToolCall(ToolCallId("c"), "peek<|channel|>x", ujson.Obj())
      settle(entries, ws, turn, Pending.Run(unknown)).map(_.failed) ==> Right(true)
      kept(entries, id).collect { case Payload.Result(_, shown) => shown } ==>
        Some("peek<|channel|>x")
    }
  }
}
