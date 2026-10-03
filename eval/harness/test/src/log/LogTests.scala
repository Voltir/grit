package grit.eval.harness.log

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.Usage
import grit.core.triage.{Kind, KnowledgeSources}
import grit.eval.harness.corpus.{CaseId, Digest, Failure}
import grit.eval.harness.score.Fixtures
import grit.lifecycle.triage.TriageQuestions

import utest.*

/** A log by position read under its questions' names. Every row is synthetic. */
object LogTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)
  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)
  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)

  private def row[A](c: CaseId, outcome: Outcome[A]): Row[A] =
    Row(
      Suite.Triage,
      c,
      0,
      Digest.text("r"),
      key,
      "m",
      None,
      outcome,
      Usage.Zero,
      1.milli,
      false,
      None
    )

  val tests = Tests {
    test(
      "each answered row is named by the questions in order, a choice's keys from its question; a row of another shape fails unreadable, and the footer counts again"
    ) {
      val v1 = TriageQuestions.V1.questions(KnowledgeSources.Empty)
      val ps = Vector(0.1, 0.6, 0.1, 0.1, 0.1)
      val (a, b, c) = (id("C1/1"), id("C1/2"), id("C1/3"))
      val positional = Log(
        Fixtures.header("kept"),
        Vector(
          row(
            a,
            Outcome.Answered(
              Vector(
                Weights.Choice(1, ps, 0.4),
                Weights.YesNo(0.2),
                Weights.YesNo(0.9),
                Weights.YesNo(0.1)
              )
            )
          ),
          row(b, Outcome.Answered(Vector(Weights.YesNo(0.2)))),
          row[Vector[Weights]](c, Outcome.Failed(Failure.Unavailable))
        ),
        Some(Footer(BigDecimal("0.002"), 3, 0, 0, 0))
      )
      val named = Log.named(positional, v1)
      (
        named.header.questions,
        named.rows.map(r => (r.id, r.outcome)),
        named.footer
      ) ==> (
        Some(Vector("kind", "waiting", "durable", "helps").map(name)),
        Vector(
          a -> Outcome.Answered(
            VectorMap(
              name("kind") -> Answer.Choice(
                Kind.written(Kind.values(1)),
                Kind.values.toVector.map(Kind.written).zip(ps).map(Answer.Weight(_, _)),
                0.4
              ),
              name("waiting") -> Answer.YesNo(0.2),
              name("durable") -> Answer.YesNo(0.9),
              name("helps") -> Answer.YesNo(0.1)
            )
          ),
          b -> Outcome.Failed(Failure.Unreadable),
          c -> Outcome.Failed(Failure.Unavailable)
        ),
        Some(Footer(BigDecimal("0.002"), 1, 0, 2, 0))
      )
    }
  }
}
