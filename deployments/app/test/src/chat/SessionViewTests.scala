package grit.app.chat

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.spend.Spend
import grit.core.store.{Entry, Payload, UsageLedger}

import utest.*

/** The conversation as the session tab describes it, from entries and ledger rows alone. */
object SessionViewTests extends TestSuite {

  private val c = ConversationId("c")

  private def entry(seq: Long, turn: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$seq"), c, TurnSeq(turn), None, EntrySeq(seq), payload, Instant.EPOCH)

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
    entry(5, 1, Payload.Window(Vector(EntrySeq(0)), Vector.empty)),
    reply(6, 1),
    entry(7, 1, Payload.Summary("asked again")),
    user(8, 2),
    entry(9, 2, Payload.Query("runes")),
    entry(10, 2, Payload.Window(Vector(EntrySeq(0), EntrySeq(2)), Vector(TurnSeq(0))))
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
      SessionView.of(Vector.empty, Vector.empty, Spend.Zero, None) ==> SessionView.empty
    }

    test("its length: the turns asked, and the messages said, replies included") {
      val v = SessionView.of(entries, costs, Spend.Zero, None)
      v.turns ==> 3
      v.messages ==> 5
    }

    test(
      "what it was billed: its turns' rows; what it cost: every call recorded for it, its closes' included"
    ) {
      // The rows' own sum is at least 0.00325; the conversation's, with a close, is more.
      val recorded = Spend(7, Cost.AtLeast(BigDecimal("0.0045")))
      val v = SessionView.of(entries, costs, recorded, None)
      v.input ==> Tokens(460)
      v.output ==> Tokens(65)
      v.spent ==> Some(Cost.AtLeast(BigDecimal("0.0045")))
      SessionView.of(entries, Vector.empty, Spend.Zero, None).spent ==> None
    }

    test("what search recalled: which earlier turns, and in how many windows") {
      def window(seq: Long, turn: Long, recalled: Long*) =
        entry(seq, turn, Payload.Window(Vector.empty, recalled.toVector.map(TurnSeq(_))))
      // Four windows, one recalling nothing; turn 1 recalled first, turn 0 twice. Three
      // windows recalled something, two turns were recalled, four recalls in all.
      val recalling = Vector(
        user(0, 0),
        user(1, 1),
        window(2, 1),
        user(3, 2),
        window(4, 2, 1),
        user(5, 3),
        window(6, 3, 1, 0),
        user(7, 4),
        window(8, 4, 0)
      )
      val v = SessionView.of(recalling, Vector.empty, Spend.Zero, None)
      v.recalls ==> 3
      v.recalled ==> Vector(TurnSeq(0), TurnSeq(1))
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
      SessionView.of(entries, costs, Spend.Zero, None).roles ==> Vector(
        SessionView.Role(SessionView.Turn, Vector("big"), 2, Some(Cost.Exact(BigDecimal("0.003")))),
        SessionView
          .Role(SessionView.Query, Vector("writer"), 1, Some(Cost.Exact(BigDecimal("0.00005")))),
        SessionView
          .Role(SessionView.Summary, Vector("small"), 2, Some(Cost.AtLeast(BigDecimal("0.0001"))))
      )
      // A role no model answered is not listed.
      SessionView
        .of(entries, costs.filter(_.model != "writer"), Spend.Zero, None)
        .roles
        .map(_.name) ==>
        Vector(SessionView.Turn, SessionView.Summary)
    }
  }
}
