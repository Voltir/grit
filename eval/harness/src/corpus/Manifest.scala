package grit.eval.harness.corpus

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Probability}
import grit.core.place.Weight
import grit.core.stitch.{Stitching, Tuning}
import grit.dbos.engine.Build
import grit.lifecycle.triage.TriageQuestion

/** What a corpus is, text-free: the `source` database it was dumped from and the `restored`
  * one its cases point into, the `dump`, the lifecycle `settings` the restored database holds,
  * the stitch `tuning` its cases are rebuilt under, the builders' `constants`, the build that
  * captured it, and how many cases it has and how many of them were `stitched`.
  *
  * @param tuning
  *   the one most placements were made under, [[Tuning.Default]] when none were; a case made
  *   under another keeps its own ([[Case.tuning]])
  * @param tunings
  *   how many tunings its placements were made under: more than 1 flags the corpus
  */
final case class Manifest(
    source: String,
    restored: String,
    dump: Dump,
    settings: Settings,
    tuning: Tuning,
    tunings: Int,
    constants: Constants,
    capture: Build,
    cases: Int,
    stitched: Int
)

/** A `pg_dump` archive of a source database: its `sha256`, and when it finished (`at`); every
  * row in it was written before then.
  */
final case class Dump(sha256: Digest, at: Instant)

/** The lifecycle settings a restored database holds ([[LifecycleSettings]]), as numbers and the
  * scope's written form: the dump holds only the latest, so a corpus assumes they held over its
  * window.
  */
final case class Settings(
    idle: FiniteDuration,
    retention: FiniteDuration,
    ledger: FiniteDuration,
    balance: Int,
    settle: FiniteDuration,
    resolveAt: Probability,
    asks: Int,
    scope: String,
    weight: Double
)

object Settings {
  def of(s: LifecycleSettings): Settings =
    Settings(
      s.windows.idle,
      s.windows.retention,
      s.windows.ledger,
      s.balance,
      s.settle,
      s.resolveAt,
      s.asks,
      s.locality.scope.written,
      Weight.value(s.locality.weight)
    )
}

/** The builders' constants a corpus was captured under: triage's thread
  * ([[TriageQuestion.ThreadChars]]), and stitching's latest messages, characters of a message
  * and BM25 hits ([[Stitching.Latest]], [[Stitching.MessageChars]], [[Stitching.Hits]]).
  */
final case class Constants(threadChars: Int, latest: Int, messageChars: Int, hits: Int)

object Constants {

  /** This build's. */
  val Shipped: Constants =
    Constants(TriageQuestion.ThreadChars, Stitching.Latest, Stitching.MessageChars, Stitching.Hits)
}
