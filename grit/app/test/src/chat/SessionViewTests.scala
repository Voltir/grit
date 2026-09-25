package grit.app.chat

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Entry, Payload, UsageLedger}

import utest.*

/** The conversation as the session tab describes it, from entries and ledger rows alone. */
object SessionViewTests extends TestSuite {

  private val c = ConversationId("c")

  private def entry(seq: Long, turn: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$seq"), c, TurnSeq(turn), None, seq, payload, Instant.EPOCH)

  private def user(seq: Long, turn: Long) =
    entry(seq, turn, Payload.Message(Message.User(s"question $turn")))

  private def reply(seq: Long, turn: Long) = entry(
    seq,
    turn,
    Payload.Message(
      Message.Assistant(
        Vector(AssistantBlock.Text(s"answer $turn")),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
    )
  )

  private def cost(seq: Long, model: String, in: Long, out: Long, usd: Option[String]) =
    UsageLedger.Row(
      EntryId(s"e$seq"),
      model,
      Usage(Tokens(in), Tokens(out), Tokens.Zero, usd.map(BigDecimal(_))),
      Tokens(in)
    )

  /** Three turns: the first two answered and summarised, the third recalling the first
    * and still running; the second's window recalled nothing.
    */
  private val entries: Vector[Entry] = Vector(
    user(0, 0),
    entry(1, 0, Payload.Window(Vector.empty, Vector.empty)),
    reply(2, 0),
    entry(3, 0, Payload.Summary("asked about runes")),
    user(4, 1),
    entry(5, 1, Payload.Window(Vector(EntryId("e0")), Vector.empty)),
    reply(6, 1),
    entry(7, 1, Payload.Summary("asked again")),
    user(8, 2),
    entry(9, 2, Payload.Query("runes")),
    entry(10, 2, Payload.Window(Vector(EntryId("e0"), EntryId("e2")), Vector(TurnSeq(0))))
  )

  private val costs: Vector[UsageLedger.Row] = Vector(
    cost(2, "big", 100, 20, Some("0.001")),
    cost(3, "small", 50, 5, Some("0.0001")),
    cost(6, "big", 200, 30, Some("0.002")),
    cost(7, "small", 60, 5, None),
    cost(9, "writer", 40, 4, Some("0.00005")),
    // A row whose entry this read of the conversation has not seen yet.
    cost(11, "big", 10, 1, Some("0.0001"))
  )

  val tests = Tests {
    test("an empty conversation is empty") {
      SessionView.of(Vector.empty, Vector.empty) ==> SessionView.empty
    }

    test("its length: the turns asked, and the messages said, replies included") {
      val v = SessionView.of(entries, costs)
      v.turns ==> 3
      v.messages ==> 5
    }

    test("what it was billed and cost: every row, whatever it priced") {
      val v = SessionView.of(entries, costs)
      v.input ==> Tokens(460)
      v.output ==> Tokens(65)
      v.spent ==> Some(BigDecimal("0.00325"))
      SessionView
        .of(entries, costs.map(r => r.copy(usage = r.usage.copy(costUsd = None))))
        .spent ==>
        None
    }

    test("what search recalled: which earlier turns, and in how many windows") {
      val v = SessionView.of(entries, costs)
      v.recalls ==> 1
      v.recalled ==> Vector(TurnSeq(0))
    }

    test("a turn's ledger is read until it has been read with its summary, then not again") {
      val asked = entries.take(2)
      // Turn 0 asked, not yet summarised: read, and not settled.
      SessionView.unread(asked, Set.empty) ==> (Vector(TurnSeq(0)), Set.empty[TurnSeq])
      // Its summary written since: read again, and settled by that read.
      SessionView.unread(entries, Set.empty) ==>
        (Vector(TurnSeq(0), TurnSeq(1), TurnSeq(2)), Set(TurnSeq(0), TurnSeq(1)))
      // Settled turns are not read; the running one still is.
      SessionView.unread(entries, Set(TurnSeq(0), TurnSeq(1))) ==>
        (Vector(TurnSeq(2)), Set(TurnSeq(0), TurnSeq(1)))
    }

    test("each role that called a model, with its models, calls and cost, in turn order") {
      SessionView.of(entries, costs).roles ==> Vector(
        SessionView.Role(SessionView.Turn, Vector("big"), 2, Some(BigDecimal("0.003"))),
        SessionView.Role(SessionView.Query, Vector("writer"), 1, Some(BigDecimal("0.00005"))),
        SessionView.Role(SessionView.Summary, Vector("small"), 2, Some(BigDecimal("0.0001")))
      )
      // A role no model answered is not listed.
      SessionView.of(entries, costs.filter(_.model != "writer")).roles.map(_.name) ==>
        Vector(SessionView.Turn, SessionView.Summary)
    }
  }
}
