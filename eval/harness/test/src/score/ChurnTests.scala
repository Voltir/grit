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
import grit.eval.harness.label.{ChurnQuestions, Gold}
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

  /** Questions graded right when a reply holds `right`, each asked in its rounds by message. */
  private def questions(qs: (String, Vector[(String, String)])*): ChurnQuestions = {
    def entry(q: String, asked: Vector[(String, String)]): String = {
      val rounds = asked.map((round, at) => s""""$round": "$at"""").mkString("{", ", ", "}")
      s""""$q": {"gold": {"facts": [["right"]], "never": []}, "asked": $rounds}"""
    }
    right(ChurnQuestions.read(qs.toVector.map(entry).mkString("{", ", ", "}")))
  }

  private def four(more: (String, Vector[(String, String)])*) = questions(
    (Vector(
      "q1" -> Vector("b" -> "CB/1.1", "a" -> "CA/1.1"),
      "q2" -> Vector("b" -> "CB/2.1", "a" -> "CA/2.1"),
      "q3" -> Vector("b" -> "CB/3.1", "a" -> "CA/3.1"),
      "q4" -> Vector("b" -> "CB/4.1", "a" -> "CA/4.1")
    ) ++ more)*
  )

  private def churn(ts: Vector[TurnCase], qs: ChurnQuestions = four()) =
    Churn.of(ts, qs, "b", "a", None)

  private val gold = Gold(::(Gold.Fact(::("right", Nil)), Nil), Vector.empty)

  private def right[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private def r(x: Double): Double = math.round(x * 1e6) / 1e6
  private def est(e: Estimate) = (r(e.mean), r(e.se), e.n, e.g)

  // Graded: b1 right after 4 calls, a1 after 1; b2 and a2 right after 2; b3 wrong having read,
  // a3 right after 2; b4 no reply (never read), a4 right after 1.
  private val graded = Map(
    b1 -> "right",
    a1 -> "right",
    b2 -> "right",
    a2 -> "right",
    b3 -> "wrong",
    a3 -> "right",
    a4 -> "right"
  ).map((t, reply) => t.workflow -> gold.matched(reply))

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
      val c = right(churn(turns))
      (c.indexed.map(est), c.useful.map(est)) ==>
        (Some((r(2.0 / 3), 0.881917, 3, 3)), Some((1.5, 1.5, 2, 2)))
      // The after-turns that read: a1, a3 and a4 followed a card, a2 did not.
      c.followed ==> Proportion(3, 4, 4, Proportion.Interval.TooFewClusters)
    }

    test("a turn that never reads is counted apart, its pair in no difference, never imputed") {
      val c = right(churn(turns))
      c.pairs.map(p => (p.question, p.before.reads == Reads.Unread)) ==>
        Vector("q1" -> false, "q2" -> false, "q3" -> false, "q4" -> true)
      // Without q4 the estimates are the same: it never entered them.
      right(
        churn(
          turns,
          questions(
            "q1" -> Vector("b" -> "CB/1.1", "a" -> "CA/1.1"),
            "q2" -> Vector("b" -> "CB/2.1", "a" -> "CA/2.1"),
            "q3" -> Vector("b" -> "CB/3.1", "a" -> "CA/3.1")
          )
        )
      ).indexed ==> c.indexed
    }

    test("a turn whose window held a part of another thread of the pairs is confounded") {
      // a1's window holds a part of b2's thread; its own thread, and threads outside the pairs,
      // confound nothing.
      val c = right(churn(turns))
      c.pairs.flatMap(p => Vector(p.before, p.after)).filter(_.confounded).map(_.workflow) ==>
        Vector(WorkflowId("a1:0"))
      // Clean: q2 1, q3 −1; mean 0, se 1. Useful clean: q2 alone, one cluster, none.
      (c.indexedClean.map(est), c.usefulClean) ==> (Some((0.0, 1.0, 2, 2)), None)
    }

    test(
      "a turn whose window held the thread of another asking of its own question is echoed, its pair not clean"
    ) {
      // q2 was asked once more, in round c, answered in thread c2; a3's window holds c2's thread,
      // another question's asking, which echoes nothing; a2's holds it and is echoed. An asking
      // no turn answers (q3's round d) is left out.
      val c2 = turn("c2", "CC/2.1", Vector.empty)
      val ts = Vector(
        b1,
        a1,
        b2,
        turn("a2", "CA/2.1", a2.rounds.flatMap(_.calls), "c2"),
        b3,
        turn("a3", "CA/3.1", a3.rounds.flatMap(_.calls), "c2"),
        b4,
        a4,
        c2
      )
      val qs = questions(
        "q1" -> Vector("b" -> "CB/1.1", "a" -> "CA/1.1"),
        "q2" -> Vector("b" -> "CB/2.1", "a" -> "CA/2.1", "c" -> "CC/2.1"),
        "q3" -> Vector("b" -> "CB/3.1", "a" -> "CA/3.1", "d" -> "CD/3.1"),
        "q4" -> Vector("b" -> "CB/4.1", "a" -> "CA/4.1")
      )
      val c = right(churn(ts, qs))
      c.pairs.flatMap(p => Vector(p.before, p.after)).filter(_.echoed).map(_.workflow) ==>
        Vector(WorkflowId("a2:0"))
      // Clean: q3 alone (q1 confounded, q2 echoed, q4 never read): one cluster, none.
      (c.indexedClean, c.indexed.map(_.n)) ==> (None, Some(3))
    }

    test(
      "a correct turn after 0 calls ranks above one after 2, and a wrong one that read above one that never read"
    ) {
      import Churn.Rank
      Vector(
        Rank.Correct(2),
        Rank.Wrong(read = false),
        Rank.Correct(0),
        Rank.Wrong(read = true)
      ).sorted ==>
        Vector(Rank.Wrong(read = false), Rank.Wrong(read = true), Rank.Correct(2), Rank.Correct(0))
    }

    test(
      "calls to correct and better are before against after a pair, clustered by question, with known values"
    ) {
      val c = right(Churn.of(turns, four(), "b", "a", Some(graded)))
      val k = c.correctness.getOrElse(sys.error("graded"))
      (k.before, k.after) ==> (
        Proportion(2, 4, 4, Proportion.Interval.TooFewClusters),
        Proportion(4, 4, 4, Proportion.Interval.TooFewClusters)
      )
      // To correct: q1 4−1 = 3, q2 2−2 = 0; mean 1.5, V = 2 · 4.5/4, se 1.5. Clean (q1 is
      // confounded): q2 alone, none.
      (k.toCorrect.map(est), k.toCorrectClean) ==> (Some((1.5, 1.5, 2, 2)), None)
      // Better: q1 +1, q2 0, q3 +1, q4 +1; mean 0.75, residuals 0.25, −0.75, 0.25, 0.25,
      // Σe² = 0.75, V = 4/3 · 0.75/16, se 0.25. Clean: q2 0, q3 1, q4 1; mean 2/3,
      // Σe² = 2/3, V = 3/2 · (2/3)/9, se 1/3.
      (k.better.map(est), k.betterClean.map(est)) ==>
        (Some((0.75, 0.25, 4, 4)), Some((r(2.0 / 3), r(1.0 / 3), 3, 3)))
    }

    test(
      "a paired turn graded is unreplied when graded lacks it, and every turn ungraded when graded is None"
    ) {
      import Churn.Grade
      right(Churn.of(turns, four(), "b", "a", Some(graded))).pairs.map(_.before.grade) ==>
        Vector(
          Grade.Graded(gold.matched("right")),
          Grade.Graded(gold.matched("right")),
          Grade.Graded(gold.matched("wrong")),
          Grade.Unreplied
        )
      val c = right(churn(turns))
      (c.correctness, c.pairs.flatMap(p => Vector(p.before.grade, p.after.grade)).distinct) ==>
        (None, Vector(Grade.Ungraded))
    }

    test("a pair missing a side is refused by its question and message") {
      churn(turns.filterNot(_ == a3)) ==>
        Left("churn: question q3: no turn answers its after message CA/3.1")
      churn(turns.filterNot(_ == b1)) ==>
        Left("churn: question q1: no turn answers its before message CB/1.1")
    }
  }
}
