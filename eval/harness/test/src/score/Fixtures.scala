package grit.eval.harness.score

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{ConversationId, EntryId, WorkflowId}
import grit.core.message.Usage
import grit.core.period.Probability
import grit.core.stitch.Offered
import grit.core.triage.Kind
import grit.dbos.engine.Build
import grit.eval.harness.corpus.{
  Case,
  CaseId,
  Clusters,
  Digest,
  Live,
  Placement,
  SeenCheck,
  Slot,
  Stitched,
  Triaged
}
import grit.eval.harness.log.{CacheKey, Header, Outcome, Row, Suite, Weights}

/** Synthetic cases and rows for the scorers' tests: every id here is made up. */
object Fixtures {

  def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  /** The case `n` (`C1/<n>`), in the exchange `exchange` began, said by `author` when given,
    * its live tags `live`, and offered `slots` exchanges by stitching when given.
    */
  def caseOf(
      n: Int,
      exchange: Int,
      author: Option[String] = None,
      live: Live = Live.Unanswered(grit.eval.harness.corpus.Failure.Other),
      slots: Option[Vector[CaseId]] = None
  ): Case = {
    val c = id(s"C1/$n")
    Case(
      c,
      EntryId(s"in:conv:$n"),
      ConversationId("conv"),
      Instant.EPOCH,
      Triaged(WorkflowId(s"triage:conv:1:$n"), None, Build.Unknown),
      live,
      None,
      author.map(Digest.text),
      Clusters(id(s"C1/$exchange"), id(s"C1/$exchange")),
      slots.map(s =>
        Stitched(
          Placement.Begins(Probability.Zero),
          Vector.empty,
          None,
          s.zipWithIndex.map((root, i) => Slot(root, Offered.Recent(i + 1))),
          SeenCheck.Match,
          0
        )
      ),
      None
    )
  }

  /** Live tags: `kind` at `kindP`, and the three yes/no. */
  def weighed(kind: Kind, kindP: Double, w: Double, d: Double, h: Double): Live =
    Live.Weighed(
      kind,
      Probability.clamped(kindP),
      Probability.clamped(w),
      Probability.clamped(d),
      Probability.clamped(h),
      "m",
      None
    )

  /** A run's header: variant `variant`, `repeats` repeats, started under `rule` when given. */
  def header(variant: String = "live", repeats: Int = 1, rule: Option[Digest] = None): Header =
    Header(
      "c",
      Digest.text("c"),
      None,
      variant,
      Some(Digest.text("w")),
      "m",
      None,
      Build.Unknown,
      repeats,
      true,
      BigDecimal(1),
      rule,
      Instant.EPOCH
    )

  private val key = CacheKey.read("ab" * 32).fold(sys.error, identity)

  def row(
      suite: Suite,
      c: CaseId,
      repeat: Int,
      outcome: Outcome[Vector[Weights]],
      latencyMs: Int = 1
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
      latencyMs.millis,
      false
    )

  /** Triage's four answers: kinds weighed `kinds` (in Kind's order), then the three yes/no. */
  def triage(kinds: Vector[Double], w: Double, d: Double, h: Double): Outcome[Vector[Weights]] =
    Outcome.Answered(
      Vector(Weights.Choice(0, kinds, 0.5), Weights.YesNo(w), Weights.YesNo(d), Weights.YesNo(h))
    )

  /** Stitching's one answer, weighing each offered exchange and then beginning anew. */
  def stitch(ps: Vector[Double]): Outcome[Vector[Weights]] =
    Outcome.Answered(Vector(Weights.Choice(0, ps, 0.5)))
}
