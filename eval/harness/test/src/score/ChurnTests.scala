package grit.eval.harness.score

import java.time.Instant

import grit.core.id.{ConversationId, EntrySeq, TurnSeq, WorkflowId}
import grit.core.message.Tokens
import grit.core.store.Focus
import grit.core.tool.ToolName
import grit.dbos.engine.Build
import grit.eval.harness.capture.{
  Call,
  Called,
  Ended,
  Part,
  Parts,
  Reading,
  Round,
  Said,
  Settled,
  Support,
  TurnCase
}
import grit.eval.harness.label.{ChurnPair, ChurnPairs}
import grit.eval.harness.stats.{Estimate, Proportion}
import grit.turn.{TurnOffer, TurnRecord}

import utest.*

/** Churn over four questions, each asked before and after, every turn's reads made by hand and
  * every figure worked in the comments. Every id here is made up.
  */
object ChurnTests extends TestSuite {

  import Churn.Reads

  private val read = ToolName("read")
  private val other = ToolName("other")

  private def indexed(followed: Boolean, support: Option[Double]) =
    Call(Called.Tool(read), Settled.Ok(9), Reading.Indexed(followed, support.flatMap(Support.read)))
  private def plain(tool: ToolName) = Call(Called.Tool(tool), Settled.Ok(9), Reading.Other)
  private val topic = Call(Called.Topic, Settled.Ok(5), Reading.Other)

  /** A Slack turn in thread `thread`, answering `said`, its calls one round each, its window
    * holding a part of each of `from`.
    */
  private def turn(thread: String, said: String, calls: Vector[Call], from: String*) =
    TurnCase(
      WorkflowId(s"$thread:0"),
      ConversationId(thread),
      TurnSeq(0),
      Said.Slack(Fixtures.id(said)),
      TurnOffer.Root.Addressed,
      Focus.Open,
      Instant.parse("2026-10-12T09:00:00Z"),
      Build.Unknown,
      None,
      None,
      TurnRecord.Weigh.Unrecorded,
      Some(
        Parts(
          (thread +: from).toVector.map(c =>
            Part(Part.Kind.Recent, ConversationId(c), Vector(EntrySeq(1)), Tokens(10), None)
          ),
          Vector.empty,
          Tokens.Zero,
          Tokens.Zero
        )
      ),
      calls.map(c => Round(Vector(c))),
      Ended.Replied(10, false),
      Vector.empty,
      None
    )

  // q1: before b1 reads after other and read (the topic call not counted): first indexed at 2,
  // first useful (0.5) at 3; after a1 reads at once, followed, useful; its window holds a part of
  // b2's thread.
  private val b1 = turn(
    "b1",
    "CB/1.1",
    Vector(
      plain(other),
      topic,
      plain(read),
      indexed(false, Some(0.1)),
      indexed(false, Some(0.5))
    )
  )
  private val a1 = turn("a1", "CA/1.1", Vector(indexed(true, Some(0.6))), "b2")
  // q2: b2 at 1, useful at 1; a2 at 0 not followed, useful at 1.
  private val b2 = turn("b2", "CB/2.1", Vector(plain(other), indexed(false, Some(0.4))))
  private val a2 =
    turn("a2", "CA/2.1", Vector(indexed(false, Some(0.0)), indexed(true, Some(0.9))))
  // q3: b3 at 0, never useful; a3 at 1, followed, useful at 1.
  private val b3 = turn("b3", "CB/3.1", Vector(indexed(false, None)))
  private val a3 = turn("a3", "CA/3.1", Vector(plain(other), indexed(true, Some(0.3))))
  // q4: b4 never reads; a4 at 0, followed, useful.
  private val b4 = turn("b4", "CB/4.1", Vector.empty)
  private val a4 = turn("a4", "CA/4.1", Vector(indexed(true, Some(0.8))))

  private val turns: Vector[TurnCase] = Vector(b1, a1, b2, a2, b3, a3, b4, a4)

  private def pairs(qs: (String, String, String)*) =
    ChurnPairs
      .of(qs.toVector.map((q, b, a) => q -> ChurnPair(Fixtures.id(b), Fixtures.id(a))))
      .fold(sys.error, identity)

  private val four = pairs(
    ("q1", "CB/1.1", "CA/1.1"),
    ("q2", "CB/2.1", "CA/2.1"),
    ("q3", "CB/3.1", "CA/3.1"),
    ("q4", "CB/4.1", "CA/4.1")
  )

  private def right[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private def r(x: Double): Double = math.round(x * 1e6) / 1e6
  private def est(e: Estimate) = (r(e.mean), r(e.se), e.n, e.g)

  val tests = Tests {
    test("each turn's reads count the calls before its first indexed and first useful read") {
      turns.map(Reads.of) ==> Vector(
        Reads.Read(2, followed = false, Some(3)),
        Reads.Read(0, followed = true, Some(0)),
        Reads.Read(1, followed = false, Some(1)),
        Reads.Read(0, followed = false, Some(1)),
        Reads.Read(0, followed = false, None),
        Reads.Read(1, followed = true, Some(1)),
        Reads.Unread,
        Reads.Read(0, followed = true, Some(0))
      )
    }

    test("churn is each question's before less after, clustered by question, with known values") {
      // Indexed: q1 2−0 = 2, q2 1−0 = 1, q3 0−1 = −1; mean 2/3, residuals 4/3, 1/3, −5/3,
      // Σe² = 42/9, V = 3/2 · (42/9)/9, se = 0.881917. Useful: q1 3−0 = 3, q2 1−1 = 0 (q3's
      // before was never useful); mean 1.5, V = 2 · 4.5/4, se = 1.5.
      val churn = right(Churn.of(turns, four))
      (churn.indexed.map(est), churn.useful.map(est)) ==>
        (Some((r(2.0 / 3), 0.881917, 3, 3)), Some((1.5, 1.5, 2, 2)))
      // The after-turns that read: a1, a3 and a4 followed a card, a2 did not.
      churn.followed ==> Proportion(3, 4, 4, Proportion.Interval.TooFewClusters)
    }

    test("a turn that never reads is counted apart, its pair in no difference, never imputed") {
      val churn = right(Churn.of(turns, four))
      churn.pairs.map(p => (p.question, p.before.reads == Reads.Unread)) ==>
        Vector("q1" -> false, "q2" -> false, "q3" -> false, "q4" -> true)
      // Without q4 the estimates are the same: it never entered them.
      right(
        Churn.of(
          turns,
          pairs(
            four.questions.toVector.take(3).map((q, p) => (q, p.before.written, p.after.written))*
          )
        )
      ).indexed ==> churn.indexed
    }

    test("a turn whose window held a part of another thread of the pairs is confounded") {
      // a1's window holds a part of b2's thread; its own thread, and threads outside the pairs,
      // confound nothing.
      val churn = right(Churn.of(turns, four))
      churn.pairs.flatMap(p => Vector(p.before, p.after)).filter(_.confounded).map(_.workflow) ==>
        Vector(WorkflowId("a1:0"))
      // Apart: q2 1, q3 −1; mean 0, se 1. Useful apart: q2 alone, one cluster, none.
      (churn.indexedApart.map(est), churn.usefulApart) ==> (Some((0.0, 1.0, 2, 2)), None)
    }

    test("a pair missing a side is refused by its question and message") {
      Churn.of(turns.filterNot(_ == a3), four) ==>
        Left("churn: question q3: no turn answers its after message CA/3.1")
      Churn.of(turns.filterNot(_ == b1), four) ==>
        Left("churn: question q1: no turn answers its before message CB/1.1")
    }
  }
}
