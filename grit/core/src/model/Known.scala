package grit.core.model

import java.time.LocalDate

/** Where a fact about a pair came from. */
enum Source {

  /** A person said so: `by` names them, or `env` for a setting read from the environment. */
  case Declared(by: String, on: LocalDate)

  /** `held` of `runs` runs of the probe named `probe` showed the fact; `held <= runs`. */
  case Measured(probe: String, on: LocalDate, runs: Int, held: Int)

  /** The upstream's own metadata said so. It never predicted strict enforcement when that
    * was measured, so it is recorded, and outranked by anything else.
    */
  case Advertised(on: LocalDate)

  /** How much this source outranks others: declared 2, measured 1, advertised 0. */
  def rank: Int = this match {
    case Declared(_, _) => 2
    case Measured(_, _, _, _) => 1
    case Advertised(_) => 0
  }
}

/** What grit knows of one setting for a pair: a value and where it came from, or nothing. */
enum Known[+A] {
  case Of(value: A, source: Source)
  case Unmeasured

  /** This, unless `other` comes from a higher-ranked source; on a tie, `other`, the newer. */
  def orOver[B >: A](other: Known[B]): Known[B] = (this, other) match {
    case (_, Unmeasured) => this
    case (Unmeasured, _) => other
    case (Of(_, mine), Of(_, theirs)) => if (mine.rank > theirs.rank) this else other
  }

  /** The value, or `otherwise` when unmeasured. */
  def getOr[B >: A](otherwise: B): B = this match {
    case Of(value, _) => value
    case Unmeasured => otherwise
  }
}
