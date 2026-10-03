package grit.core.review

import scala.concurrent.duration.FiniteDuration

import grit.core.id.ShadowName

/** A review of `shadow`'s question set: heard messages said within `within`, at most `perDay`
  * of those only live's gate drafts on or neither does picked a day ([[Review.pick]]), one in
  * `sampleOneIn` of the latter eligible.
  */
final case class Reviewing private (
    shadow: ShadowName,
    perDay: Int,
    sampleOneIn: Int,
    within: FiniteDuration
)

object Reviewing {

  /** `None` unless `perDay` and `sampleOneIn` are at least 1 and `within` is positive. */
  def of(
      shadow: ShadowName,
      perDay: Int,
      sampleOneIn: Int,
      within: FiniteDuration
  ): Option[Reviewing] =
    Option.when(perDay >= 1 && sampleOneIn >= 1 && within.length > 0)(
      new Reviewing(shadow, perDay, sampleOneIn, within)
    )
}
