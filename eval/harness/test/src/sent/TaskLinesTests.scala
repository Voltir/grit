package grit.eval.harness.sent

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.provider.ModelRequest
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Integrity
import grit.eval.harness.capture.Spent

import utest.*

/** A task's turns read for what they held in the cache ([[TaskLines]]): each boundary's read
  * share and carry-over, a dropped turn flagged, and the longest gap between replies.
  */
object TaskLinesTests extends TestSuite {

  private val At = Instant.parse("2026-11-05T12:00:00Z")

  /** A call whose ledger row read `read` and wrote `write` of `input`, unpriced. */
  private def call(n: Int, input: Long, read: Long, write: Long): MeasuredCall = {
    val usage = Usage(Tokens(input), Tokens(10), Tokens(read), None, Tokens(write))
    MeasuredCall(
      EntryId(s"r$n"),
      ModelRequest("sys", Vector.empty),
      Some(Spent(None, "m", usage, Tokens(0))),
      None,
      None,
      None,
      None
    )
  }

  /** Entry `n`, of turn `turn`, kept `n` seconds after [[At]]. */
  private def entry(n: Long, turn: Long, id: String = ""): Entry = Entry(
    EntryId(if (id.isEmpty) s"e$n" else id),
    ConversationId("c"),
    TurnSeq(turn),
    None,
    EntrySeq(n),
    Payload.Message(Message.User(s"m$n")),
    At.plusSeconds(n),
    Integrity.Member
  )

  val tests: Tests = Tests {
    test(
      "a turn's window drawn at a boundary drops the earlier turns it shows none of, recalled ones kept; a carried window drops none; each call kept when its reply was"
    ) {
      // Turns 0 to 3 each say one message; turn 3's two calls kept their replies as r1 and r2.
      val entries =
        Vector(entry(1, 0), entry(2, 1), entry(3, 2), entry(4, 3), entry(5, 3, "r1"), entry(6, 3, "r2"))
      def window(seqs: Vector[Long], recalled: Vector[Long], after: Option[EntryId]) =
        Payload.Window(
          seqs.map(EntrySeq(_)),
          recalled.map(TurnSeq(_)),
          carry = Some(Payload.Carry(after, Tokens(0), None, None))
        )
      def read(w: Option[Payload.Window]) = {
        val t = TaskTurn.of(TurnSeq(3), w, Vector(call(1, 1, 0, 0), call(2, 1, 0, 0)), entries)
        (t.boundary, t.dropped.map(TurnSeq.value), t.kept)
      }
      (
        read(Some(window(Vector(3), Vector.empty, None))),
        read(Some(window(Vector(3), Vector(1), None))),
        read(Some(window(Vector(3), Vector.empty, Some(EntryId("w"))))),
        read(None)
      ) ==> (
        (true, Vector(0L, 1L), Vector(At.plusSeconds(5), At.plusSeconds(6))),
        (true, Vector(0L), Vector(At.plusSeconds(5), At.plusSeconds(6))),
        (false, Vector.empty, Vector(At.plusSeconds(5), At.plusSeconds(6))),
        (false, Vector.empty, Vector(At.plusSeconds(5), At.plusSeconds(6)))
      )
    }

    test(
      "each turn says its first call's read share and the carry-over the turn before left unread; a boundary that dropped a turn is flagged; the longest gap names its turn"
    ) {
      val turns = Vector(
        TaskTurn(
          TurnSeq(0),
          Vector(call(1, 10_000, 0, 9_000), call(2, 11_000, 9_000, 2_000)),
          boundary = true,
          Vector.empty,
          Vector(At, At.plusSeconds(10))
        ),
        TaskTurn(
          TurnSeq(1),
          Vector(call(3, 8_000, 5_000, 3_000)),
          boundary = true,
          Vector.empty,
          Vector(At.plusSeconds(30))
        ),
        TaskTurn(
          TurnSeq(2),
          Vector(call(4, 9_000, 6_000, 0)),
          boundary = true,
          Vector(TurnSeq(0)),
          Vector(At.plusSeconds(390))
        )
      )
      val cache = SentMarkdown.cacheOf(turns.flatMap(_.calls)).getOrElse("none")
      TaskLines.lines(turns) ==> Vector(
        s"task: 3 turns, 4 calls; cache $cache; longest gap between replies 6m 0s (into turn 2)",
        "turn 0 (boundary): 2 calls; first call read 0.0% of 10000, misses unpriced",
        "turn 1 (boundary): 1 calls; first call read 62.5% of 8000, misses unpriced; " +
          "carry-over: 6000 of 11000 the turn before left in the cache not read",
        "turn 2 (boundary, DROPPED turns 0): 1 calls; first call read 66.7% of 9000, misses " +
          "unpriced; carry-over: 2000 of 8000 the turn before left in the cache not read"
      )
    }
  }
}
