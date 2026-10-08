package grit.slack.edge

import scala.concurrent.duration.*

/** How a join's backfill is bounded: what was said in the `days` before grit's bot joined a
  * channel, its newest `messages` at most, and at most `joinsPerDay` joins backfilled in the 24
  * hours before each. Each message heard is triaged once, never answered, and each thread it
  * opens is closed once: at most `messages` × (one triage call and one closing) a join. A join
  * made while the day's spend is over the inbox's cap, or after `joinsPerDay` others in the day
  * before it, is not backfilled, and that is said.
  */
final case class Backfill private (days: Int, messages: Int, joinsPerDay: Int)

object Backfill {

  /** 2 days, 100 messages, 10 joins a day. */
  val Default: Backfill = new Backfill(2, 100, 10)

  /** These, or why not: each at least 1. */
  def of(days: Int, messages: Int, joinsPerDay: Int): Either[String, Backfill] =
    Vector("days" -> days, "messages" -> messages, "joinsPerDay" -> joinsPerDay)
      .collectFirst {
        case (name, n) if n < 1 => s"$name is $n: a backfill's $name must be at least 1"
      }
      .toLeft(new Backfill(days, messages, joinsPerDay))

  /** How long closing the edge waits for a backfill under way: 10 s. One cut short stays
    * pending, and is resumed when the edge next opens.
    */
  val StopWithin: FiniteDuration = 10.seconds
}
