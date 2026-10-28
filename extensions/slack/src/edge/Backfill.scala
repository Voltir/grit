package grit.slack.edge

import scala.concurrent.duration.*

/** How a join's backfill is bounded: what was said in the `days` before grit's bot joined a
  * channel, its newest `messages` at most, and at most `joinsPerDay` joins backfilled in the 24
  * hours before each. Each message heard is never answered, and costs what a live one heard
  * costs: one triage classifier call, and, for each thread it begins, its placement among the
  * room's exchanges, its settle question and its closing (a gate call, and a summary when its
  * messages earn one). A join made while the day's spend is over the inbox's cap, or after
  * `joinsPerDay` others in the day before it, is not backfilled, and that is said.
  */
final case class Backfill private (days: Int, messages: Int, joinsPerDay: Int)

object Backfill {

  /** 2 days, 100 messages, 10 joins a day. */
  val Default: Backfill = new Backfill(2, 100, 10)

  /** The variable saying how many days back `grit backfill` reads, and a join's backfill hears
    * ([[read]]); unset, [[Default]]'s days.
    */
  val DaysVar: String = "GRIT_BACKFILL_DAYS"

  /** The variable bounding how many messages a join's backfill hears; unset, [[Default]]'s. */
  val MessagesVar: String = "GRIT_BACKFILL_MESSAGES"

  /** The variable bounding how many joins a day a join's backfill hears; unset, [[Default]]'s. */
  val JoinsVar: String = "GRIT_BACKFILL_JOINS_PER_DAY"

  /** A join's bounds as `env` gives them: each variable unset takes [[Default]]'s; why not,
    * naming the first variable that is not a whole number above zero.
    */
  def read(env: Map[String, String]): Either[String, Backfill] =
    for {
      daysRead <- days(env)
      messages <- above(env, MessagesVar, Default.messages, "a whole number above zero")
      joins <- above(env, JoinsVar, Default.joinsPerDay, "a whole number above zero")
      bounds <- of(daysRead, messages, joins)
    } yield bounds

  /** The days `grit backfill` reads ([[DaysVar]]); why not, naming the variable, when it is not
    * a whole number of days above zero.
    */
  def days(env: Map[String, String]): Either[String, Int] =
    above(env, DaysVar, Default.days, "a whole number of days above zero")

  /** `variable`'s whole number above zero in `env`, `default` when unset; why not, saying it is
    * `what`, when it is not one.
    */
  private def above(
      env: Map[String, String],
      variable: String,
      default: Int,
      what: String
  ): Either[String, Int] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        raw.trim.toIntOption.filter(_ > 0).toRight(s"$variable is $what, not '$raw'")
    }

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
