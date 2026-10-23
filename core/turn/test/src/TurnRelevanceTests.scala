package grit.turn

import grit.core.act.MoveName
import grit.core.classify.{Answer, Answered, Ask, ClassifierError, Question}
import grit.core.context.{Briefs, Results, Retrieved}
import grit.core.id.TurnSeq
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability

import utest.*

object TurnRelevanceTests extends TestSuite {
  import TurnRelevance.Verdict

  private type Asked = Ask[TurnRelevance.State, Vector[Probability]]

  private def found(n: Int): Retrieved = Retrieved.Own(TurnSeq(n.toLong))

  private def ranked(count: Int): Results =
    Results((0 until count).toVector.map(n => found(n) -> (count - n).toDouble))

  private def briefed(batch: Vector[Retrieved]): Briefs =
    Briefs("the message", batch.map(r => r -> s"brief of $r"))

  private def answered(ps: Double*): Either[ClassifierError, Answered[Vector[Probability]]] =
    Right(
      Answered(
        ps.toVector.map(Probability.clamped),
        Usage(Tokens(40), Tokens(6), Tokens.Zero, None),
        "judge"
      )
    )

  val tests = Tests {
    test("a candidate at the pass mark passes and one under it is rejected") {
      val batch = Vector(found(0), found(1))
      TurnRelevance.verdicts(batch, briefed(batch), answered(0.5, 0.49)) ==> Vector(
        found(0) -> Verdict.Passed(Probability.clamped(0.5)),
        found(1) -> Verdict.Rejected(Probability.clamped(0.49))
      )
    }

    test("a candidate gone before it was briefed is unjudged, the others read in order") {
      val batch = Vector(found(0), found(1), found(2))
      val briefs = briefed(Vector(found(0), found(2)))
      TurnRelevance.verdicts(batch, briefs, answered(0.1, 0.9)) ==> Vector(
        found(0) -> Verdict.Rejected(Probability.clamped(0.1)),
        found(1) -> Verdict.Unjudged(TurnRelevance.Gone),
        found(2) -> Verdict.Passed(Probability.clamped(0.9))
      )
    }

    test("an unavailable classifier leaves each candidate unjudged, and unjudged is admitted") {
      val results = ranked(3)
      val batch = results.ranked.map(_._1)
      val verdicts =
        TurnRelevance.verdicts(batch, briefed(batch), Left(ClassifierError.Unavailable("down")))
      (verdicts, TurnRelevance.admitted(results, verdicts)) ==> (
        batch.map(_ -> Verdict.Unjudged("unavailable: down")),
        results
      )
    }

    test("the ten questions of a batch name candidates[0] to candidates[9] in order") {
      val state = TurnRelevance.State("the message", (0 until 10).toVector.map(n => s"c$n"))
      val ask = TurnRelevance.ask(state)
      (
        ask.map((a: Asked) => a.questions.collect { case q: Question.YesNo => q.instructions }),
        ask.map((a: Asked) => a.read((0 until 10).toVector.map(n => Answer.YesNo(n / 10.0))))
      ) ==> (
        Some(
          (0 until 10).toVector.map(n =>
            s"Would candidates[$n] help answer the message? Yes only when it holds facts, " +
              "decisions or context a reply to the message needs; no when it is only on a " +
              "related subject."
          )
        ),
        Some(Right((0 until 10).toVector.map(n => Probability.clamped(n / 10.0))))
      )
    }

    test("the judge reads 1,200 characters of each candidate: the head of a 400-token piece") {
      // Pinned as measured: 1,200 characters under 400-token pieces judged more answering
      // pieces relevant than 800 under 200, the quote still within what was read.
      (TurnRelevance.Chars, grit.core.files.Pieces.Target) ==>
        (1200, grit.core.message.Tokens(400))
    }

    test("only the first thirty are admitted, judged or not") {
      val results = ranked(35)
      val verdicts = Vector(
        found(2) -> Verdict.Rejected(Probability.clamped(0.1)),
        found(3) -> Verdict.Passed(Probability.clamped(0.9)),
        found(31) -> Verdict.Passed(Probability.clamped(0.9))
      )
      TurnRelevance.admitted(results, verdicts) ==>
        Results(results.ranked.take(30).filterNot(_._1 == found(2)))
    }

    test("batches are Judged / PerCall, named relevance-0, relevance-1, relevance-2") {
      val results = ranked(35)
      val split = TurnRelevance.batches(results)
      val empty = TurnRelevance.batches(Results.empty)
      (
        split.map((name, batch) => MoveName.value(name) -> batch),
        empty.map(_._2.size)
      ) ==> (
        Vector(
          "relevance-0" -> results.ranked.slice(0, 10).map(_._1),
          "relevance-1" -> results.ranked.slice(10, 20).map(_._1),
          "relevance-2" -> results.ranked.slice(20, 30).map(_._1)
        ),
        Vector(0, 0, 0)
      )
    }

    test("grounds: a passed candidate at GroundsAt or more, never an unjudged one") {
      val at = Probability.value(TurnRelevance.GroundsAt)
      def one(v: Verdict): Boolean = TurnRelevance.grounds(Vector(found(0) -> v))
      (
        one(Verdict.Passed(TurnRelevance.GroundsAt)),
        one(Verdict.Passed(Probability.clamped(at - 0.01))),
        one(Verdict.Unjudged("down")),
        one(Verdict.Rejected(Probability.clamped(0.1))),
        TurnRelevance.grounds(Vector.empty),
        TurnRelevance.grounds(
          Vector(
            found(0) -> Verdict.Unjudged("down"),
            found(1) -> Verdict.Passed(Probability.clamped(0.9))
          )
        )
      ) ==> (true, false, false, false, false, true)
    }
  }
}
