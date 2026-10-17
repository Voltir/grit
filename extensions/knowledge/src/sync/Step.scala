package grit.knowledge.sync

import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardSummary, UnitVersion}

/** What a fetch's arguments are made from, and what its unit is believed at: [[Way.Run]],
  * everything the run read before it; [[Way.Through]], the listing at `listing` it was named in,
  * and what that listing was reached through.
  */
enum Way extends caps.Pure {
  case Run
  case Through(listing: String)
}

/** One move a walk asks its run to make. */
sealed trait Step extends caps.Pure

object Step {

  /** A step its run makes as a call to its source: [[List]] or [[Read]]. */
  sealed trait Fetching extends Step {

    /** The unit it lists or reads, as its source names it. */
    def path: String

    /** How its run reached `path`. */
    def way: Way
  }

  /** List the unit `path`, one holding others (a directory, a folder), reached `way`. */
  final case class List(path: String, way: Way) extends Fetching

  /** Read the file `path`, reached `way`. */
  final case class Read(path: String, way: Way) extends Fetching

  /** Summarise `head`, the first 8 KB of the file `path` as read at `version`; the card written
    * from it records `version`.
    */
  final case class Summarise(path: String, head: String, version: UnitVersion) extends Step

  /** Keep `card` at the run's floor, withdrawing its copy at `replacing`, if any, in that keep. */
  final case class Write(card: Card, replacing: Option[Label]) extends Step

  /** Copy the card under `key` from `from` up to the run's floor, and withdraw it at `from`. */
  final case class Relabel(key: CardKey, from: Label) extends Step

  /** Withdraw each of `gone`, at its label. */
  final case class Drop(gone: Vector[Held]) extends Step
}

/** What the run's last step came to, as a walk of a source listing as `L` reads it:
  * [[Outcome.Listed]] for a [[Step.List]], [[Outcome.Fetched]] for a [[Step.Read]],
  * [[Outcome.Summarised]] for a [[Step.Summarise]], [[Outcome.Kept]] for a [[Step.Write]],
  * [[Step.Relabel]] or [[Step.Drop]], [[Outcome.NotTaken]] for a [[Step.List]] or a
  * [[Step.Read]], and [[Outcome.Stopped]] for any. A listing's or a read's `at` is the label its
  * call's answer holds nothing above ([[grit.core.act.Called.Done]]).
  */
enum Outcome[+L <: caps.Pure] extends caps.Pure {
  case Listed(listing: L, at: Label)
  case Fetched(fetched: grit.knowledge.sync.Fetched, at: Label)
  case Summarised(summary: CardSummary)
  case Kept

  /** What the step read is not taken: `why`, as the run's reply will say it of its unit. */
  case NotTaken(why: String)

  /** The move was refused, or failed: `why`, as the run's reply will say it. */
  case Stopped(why: String)
}
