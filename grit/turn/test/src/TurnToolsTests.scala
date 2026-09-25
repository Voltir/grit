package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.durable.InMemoryDurable
import grit.core.id.{EntryId, ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Entry, EntryStore, InMemoryEntryStore, Payload}
import grit.core.tool.{Bound, Outcome}
import grit.dbos.sql.TestTx

import utest.*
import TurnLoop.{Pending, Round}

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
    val slot = TurnTools.Slot(turn, Round.First, 0)
    val id = pending.call.id
    val settling = new TurnTools.Settling(new FakeJot, entries)
    TurnTools.read(tools(ws), pending) match {
      case Left(outcome) => settling.answer(slot, id, outcome, Instant.EPOCH)
      case Right(free: Bound.Free) => settling.run(slot, id, free, Instant.EPOCH)
      case Right(gated: Bound.Gated) =>
        settling.decide(slot, id, gated, approval, Instant.EPOCH)
    }
  }

  private def kept(entries: InMemoryEntryStore, id: EntryId): Option[Payload] =
    entries.get(id)(using TestTx.fake).toOption.flatten.map(_.payload)

  private def isResult(e: Entry): Boolean = EntryId.value(e.id).startsWith("result:")

  val tests = Tests {
    test("a free call runs, and its result is kept under its fixed id") {
      val entries = new InMemoryEntryStore
      val ws = new Files(files)
      val turn = say(entries, "go")
      val id = TurnTools.Slot(turn, Round.First, 0).resultId
      settle(entries, ws, turn, call("peek")) ==> Right(TurnTools.Settled(id, failed = false))
      kept(entries, id) ==>
        Some(Payload.Exchange(Message.ToolResult(ToolCallId("c"), "alpha", isError = false)))
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
      kept(store, TurnTools.Slot(turn, Round.First, 0).attemptId) ==>
        Some(Payload.Attempt(ToolCallId("c")))
      settle(entries, ws, turn, call("poke"), approved).map(_.failed) ==> Right(true)
      ws.reads ==> 1
      kept(store, TurnTools.Slot(turn, Round.First, 0).resultId) ==>
        Some(Payload.Exchange(Outcome.Interrupted.result(ToolCallId("c"))))
    }

    test("a gated call declined or timed out does not run") {
      val ws = new Files(files)
      val answers = Vector(
        Approval.Declined(Some("not now")) -> Outcome.Denied(Some("not now")),
        Approval.TimedOut -> Outcome.Denied(Some(grit.core.tool.Bound.Unanswered))
      )
      answers.foreach { (approval, expected) =>
        val entries = new InMemoryEntryStore
        val turn = say(entries, "go")
        settle(entries, ws, turn, call("poke"), approval)
        kept(entries, TurnTools.Slot(turn, Round.First, 0).resultId) ==>
          Some(Payload.Exchange(expected.result(ToolCallId("c"))))
        kept(entries, TurnTools.Slot(turn, Round.First, 0).attemptId) ==> None
      }
      ws.reads ==> 0
    }

    test("an answer is its approval; none is timed out; one unreadable declines, saying why") {
      TurnTools.approval(Some(Approval.encode(Approval.Approved))) ==> Approval.Approved
      TurnTools.approval(Some(Approval.encode(Approval.Declined(Some("no"))))) ==>
        Approval.Declined(Some("no"))
      TurnTools.approval(None) ==> Approval.TimedOut
      TurnTools.approval(Some("yes please")) ==> Approval.Declined(
        Some("The answer could not be read (not a JSON object), so it did not run.")
      )
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
  }
}
