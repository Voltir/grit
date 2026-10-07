package grit.eval.harness.score

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, WorkflowId}
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.stitch.Offered
import grit.core.triage.Kind
import grit.dbos.engine.Build
import grit.eval.harness.capture.{
  Case,
  CaseId,
  Clusters,
  Digest,
  Live,
  Offering,
  Placement,
  SeenCheck,
  Stitched,
  Triaged
}
import grit.eval.harness.log.{CacheKey, Outcome, Row, Suite, Weights}

import utest.*

/** How far apart repeats, and a run and live, answered. Every id here is synthetic. */
object SpreadTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)
  private def p(d: Double): Probability = Probability.clamped(d)
  private val a = id("C1/1727000000.000100")
  private val b = id("C1/1727000000.000200")
  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)

  private def row(
      suite: Suite,
      c: CaseId,
      repeat: Int,
      outcome: Outcome[Vector[Weights]]
  ): Row[Vector[Weights]] =
    Row(
      suite,
      c,
      repeat,
      Digest.text("r"),
      key,
      "m",
      None,
      outcome,
      Usage.Zero,
      1.millis,
      false,
      None
    )

  /** Triage's four answers: kinds weighed `kinds` (not yet summing to 1), then three yes. */
  private def triage(kinds: Vector[Double], w: Double, d: Double, h: Double) =
    Outcome.Answered(
      Vector(Weights.Choice(0, kinds, 0.5), Weights.YesNo(w), Weights.YesNo(d), Weights.YesNo(h))
    )

  private def live(c: CaseId, kind: Kind, kindP: Double, w: Double, d: Double, h: Double) =
    Case(
      c,
      EntryId(s"in:conv:${c.written}"),
      ConversationId("conv"),
      Instant.EPOCH,
      Triaged(WorkflowId("triage:conv:1:0"), None, Build.Unknown),
      Live.Weighed(kind, p(kindP), p(w), p(d), p(h), "m", None),
      None,
      None,
      Clusters(c, c),
      None,
      None
    )

  val tests = Tests {
    test("repeats spread by each probability's max minus min, choices normalised first") {
      val rows = Vector(
        row(Suite.Triage, a, 0, triage(Vector(2.0, 2.0, 0, 0, 0), 0.25, 0.5, 0.75)),
        // The same choice weighed at twice the scale: no gap once normalised.
        row(Suite.Triage, a, 1, triage(Vector(4.0, 4.0, 0, 0, 0), 0.25, 0.5, 0.75)),
        row(Suite.Triage, b, 0, triage(Vector(1.0, 0, 0, 0, 0), 0.25, 0.5, 0.75)),
        row(Suite.Triage, b, 1, triage(Vector(1.0, 0, 0, 0, 0), 0.25, 0.5, 0.875)),
        row(Suite.Triage, b, 2, triage(Vector(1.0, 0, 0, 0, 0), 0.25, 0.5, 0.75)),
        row(Suite.Triage, b, 3, Outcome.Skipped)
      )
      Spread.repeats(rows) ==> Spread(0.125, Vector((Suite.Triage, b)), 2)
    }

    test("a run is compared to the live kind's probability and the three yes/no") {
      val cases = Vector(
        live(a, Kind.Answer, 0.5, 0.25, 0.5, 0.75),
        live(b, Kind.Question, 0.75, 0.25, 0.5, 0.75)
      )
      val rows = Vector(
        // a: the live kind (answer, second) at 0.5 once normalised: no gap.
        row(Suite.Triage, a, 0, triage(Vector(1.0, 1.0, 0, 0, 0), 0.25, 0.5, 0.75)),
        // b: the live kind (question, first) at 0.5, live 0.75.
        row(Suite.Triage, b, 0, triage(Vector(1.0, 1.0, 0, 0, 0), 0.25, 0.5, 0.75))
      )
      Spread.live(rows, cases) ==> Spread(0.25, Vector((Suite.Triage, b)), 2)
    }

    test(
      "a stitch row is compared to each exchange offered live, only when the seen check matched"
    ) {
      def stitched(seen: SeenCheck) =
        live(a, Kind.Question, 1.0, 0, 0, 0).copy(stitch =
          Some(
            Stitched(
              Placement.Begins(p(0.25)),
              Vector(
                Offering(b, Offered.Recent(1), Some(p(0.25))),
                Offering(a, Offered.Lexical(2.0), Some(p(0.125)))
              ),
              None,
              Vector.empty,
              seen,
              0
            )
          )
        )
      val asked = row(
        Suite.Stitch,
        a,
        0,
        Outcome.Answered(Vector(Weights.Choice(2, Vector(0.25, 0.25, 0.5), 0.2)))
      )
      Spread.live(Vector(asked), Vector(stitched(SeenCheck.Match))) ==>
        Spread(0.125, Vector((Suite.Stitch, a)), 1)
      Spread.live(Vector(asked), Vector(stitched(SeenCheck.Unbuilt))) ==>
        Spread(0.0, Vector.empty, 0)
    }
  }
}
