package grit.knowledge.sync

import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardSummary, UnitVersion}
import grit.knowledge.github.Listing

/** One move a walk asks its run to make. */
enum Step extends caps.Pure {

  /** List the directory `path`, `/` for the repository's root. */
  case List(path: String)

  /** Read the file `path`. */
  case Read(path: String)

  /** Summarise `head`, the first 8 KB of the file `path` as read at `version`; the card written
    * from it records `version`.
    */
  case Summarise(path: String, head: String, version: UnitVersion)

  /** Keep `card` at the run's floor, withdrawing its copy at `replacing`, if any, in that keep. */
  case Write(card: Card, replacing: Option[Label])

  /** Copy the card under `key` from `from` up to the run's floor, and withdraw it at `from`. */
  case Relabel(key: CardKey, from: Label)

  /** Withdraw each of `gone`, at its label. */
  case Drop(gone: Vector[Held])
}

/** What the run's last step came to, as a walk reads it: [[Outcome.Listed]] for a
  * [[Step.List]], [[Outcome.Fetched]] for a [[Step.Read]], [[Outcome.Summarised]] for a
  * [[Step.Summarise]], [[Outcome.Kept]] for a [[Step.Write]], [[Step.Relabel]] or [[Step.Drop]],
  * [[Outcome.NotTaken]] for a [[Step.List]] or a [[Step.Read]], and [[Outcome.Stopped]] for any.
  */
enum Outcome extends caps.Pure {
  case Listed(listing: Listing)
  case Fetched(fetched: grit.knowledge.github.Fetched)
  case Summarised(summary: CardSummary)
  case Kept

  /** What the step read is not taken: `why`, as the run's reply will say it of its unit. */
  case NotTaken(why: String)

  /** The move was refused, or failed: `why`, as the run's reply will say it. */
  case Stopped(why: String)
}
