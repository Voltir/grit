package grit.eval.harness.corpus

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.triage.{Kind, Tags}

import utest.*

/** What a case keeps of live triage's tags, from each era's. Every answer is synthetic. */
object LiveTests extends TestSuite {

  private def p(d: Double): Probability = Probability.clamped(d)
  private def name(n: String): QuestionName = QuestionName.read(n).fold(sys.error, identity)

  private val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.00004")))
  private val v1 = Tags.V1.answers(Kind.Decision, p(0.7), p(0.2), p(0.9), p(0.1))
  private val v2: VectorMap[QuestionName, Answer] = VectorMap(
    name("open") -> Answer.YesNo(0.8),
    name("durable") -> Answer.YesNo(0.3),
    name("source:github") -> Answer.YesNo(0.6)
  )

  val tests = Tests {
    test(
      "tags answering v1's four names alone are kept as v1's, any others under their names with nothing dropped, and no answer as its failure's kind"
    ) {
      (
        Live.of(Tags.Weighed(v1, "jev", usage)),
        Live.of(Tags.Weighed(v2, "jev", usage)),
        Live.of(Tags.Weighed(v1 + (name("source:github") -> Answer.YesNo(0.5)), "jev", usage)),
        Live.of(Tags.Unanswered("unavailable: refused"))
      ) ==> (
        Live.Weighed(Kind.Decision, p(0.7), p(0.2), p(0.9), p(0.1), "jev", usage.costUsd),
        Live.Named(v2, "jev", usage.costUsd),
        Live.Named(v1 + (name("source:github") -> Answer.YesNo(0.5)), "jev", usage.costUsd),
        Live.Unanswered(Failure.Unavailable)
      )
    }

    test("v1's names with a kind choice of no kind are kept under their names") {
      val odd = v1.updated(
        Tags.V1.kind,
        Answer.Choice("musing", Vector(Answer.Weight("musing", 0.7)), 0.0)
      )
      Live.of(Tags.Weighed(odd, "jev", usage)) ==> Live.Named(odd, "jev", usage.costUsd)
    }
  }
}
