package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, TestCallSlots, ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Service
import grit.core.store.{Entry, EntryStore, InMemoryEntryStore, Payload}
import grit.core.tool.{
  Args,
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
import grit.core.visibility.{Compartments, Integrity, Output, Visibility}
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
    val settling = new TurnTools.Settling(new FakeJot, entries)
    TurnTools.read(tools(ws), pending, Repairs.All, PublicReads, Integrity.Authority) match {
      case Left(outcome) =>
        val unrun = Left(Integrity.Untrusted)
        settling.answer(slot, id, pending.call.name, outcome, unrun, Instant.EPOCH)
      case Right(free: Bound.Free) => settling.run(slot, id, free, Instant.EPOCH)
      case Right(gated: Bound.Gated) =>
        settling.decide(slot, id, gated, approval, Instant.EPOCH)
      case Right(_: Bound.Hosted) => Left(TurnFailure.Model("no hosted tools here"))
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
    val settling = new TurnTools.Settling(new FakeJot, entries)
    val box = Toolbox.of[{}](tool).fold(d => throw new java.lang.AssertionError(d), identity)
    val sent = Pending.Run(AssistantBlock.ToolCall(ToolCallId("c"), "said", ujson.Obj("p" -> "x")))
    TurnTools.read(box, sent, Repairs.All, PublicReads, Integrity.Authority) match {
      case Right(free: Bound.Free) =>
        settling
          .run(slot, ToolCallId("c"), free, Instant.EPOCH)
          .map(_ => believed(entries, slot.resultId))
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }
  }

  /** `said`, free, answering `outcome`, its results believed at [[Integrity.Member]]. */
  private def said(outcome: Outcome): Tool[String] =
    new Hosted(
      ToolSpec(ToolName("said"), "Says.", Args.of((p = Field.text("What."))).map(_.p)),
      Gate.Free,
      (p: String) => p
    ).believed(Integrity.Member).over(_ => outcome)

  /** What a kept entry is believed at. */
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
          outputs = Vector(Output(github, Integrity.Member))
        )
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      val entries = new InMemoryEntryStore
      val turn = say(entries, "go")
      val settling = new TurnTools.Settling(new FakeJot(visibility = declaring), entries)
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
      hosted.map(_ => believed(entries, slot.resultId)) ==> Right(Some(Integrity.Member))
      val other = new InMemoryEntryStore
      val otherTurn = say(other, "go")
      val otherSlot = TurnTools.Slot(TestCallSlots.at(otherTurn))
      new TurnTools.Settling(new FakeJot, other)
        .answer(
          otherSlot,
          ToolCallId("c"),
          "read",
          Outcome.Done("a file"),
          Left(Integrity.Member),
          Instant.EPOCH
        )
        .map(_ => believed(other, otherSlot.resultId)) ==> Right(Some(Integrity.Member))
    }

    test("an own tool's result is believed at what its outcome says, else at its tool's") {
      ranAt(said(Outcome.Done("x"))) ==> Right(Some(Integrity.Member))
      ranAt(said(Outcome.Done("x", Some(Integrity.Outside)))) ==> Right(Some(Integrity.Outside))
      ranAt(said(Outcome.Failed("no"))) ==> Right(Some(Integrity.Member))
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

    test("a kept result is returned as it is, and nothing runs again") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val first = settle(entries, ws, turn, call("peek"))
      settle(entries, ws, turn, call("peek")) ==> first
      settle(entries, ws, turn, call("poke"), Approval.Approved) ==> first
      ws.reads ==> 1
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
