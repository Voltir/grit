package grit.knowledge.topic

import java.time.LocalDate

import scala.concurrent.duration.*

/** What a topics run is told: at most how many topics it writes (`perRun`); how far back a
  * strand's last message (`recent`) and a card's write (`cardRecent`) may be for it to be looked
  * at; how long one set aside stays aside before it is offered again (`asideFor`); the `day` it writes for, the day of its slot in its job's zone when none: a backfill
  * names a past day, and its strands are read as they stood at that day's end; and the last day
  * it seeds on (`seedUntil`): a run for that day or one before reads the cards written within
  * [[TopicParams.SeedReach]] instead of `cardRecent`, so each night offers the next batch of
  * the cards no run has placed.
  */
final case class TopicParams(
    perRun: Int,
    recent: FiniteDuration,
    cardRecent: FiniteDuration,
    day: Option[LocalDate],
    seedUntil: Option[LocalDate],
    asideFor: FiniteDuration = TopicParams.AsideFor
) extends caps.Pure

object TopicParams {

  /** 3,650 days: how far back a seeding run reads cards. */
  val SeedReach: FiniteDuration = 3650.days

  /** 7 days: how long a strand or card set aside stays aside, unless a schedule says. */
  val AsideFor: FiniteDuration = 7.days

  /** Its record: `{"perRun", "recent", "cardRecent", "asideFor", "day"?, "seedUntil"?}`,
    * durations ISO-8601, the days `yyyy-mm-dd`.
    */
  def write(p: TopicParams): ujson.Value = {
    def iso(d: FiniteDuration): String = java.time.Duration.ofSeconds(d.toSeconds).toString
    val o = ujson.Obj(
      "perRun" -> p.perRun,
      "recent" -> iso(p.recent),
      "cardRecent" -> iso(p.cardRecent),
      "asideFor" -> iso(p.asideFor)
    )
    p.day.foreach(d => o("day") = d.toString)
    p.seedUntil.foreach(d => o("seedUntil") = d.toString)
    o
  }

  /** The parameters [[write]] wrote, `asideFor` [[AsideFor]] when absent (kept before it was),
    * or why `v` holds none: `perRun` not a whole number at least 1, a duration not positive
    * ISO-8601, or a day not `yyyy-mm-dd`.
    */
  def read(v: ujson.Value): Either[String, TopicParams] = {
    val shown = ujson.write(v).take(100)
    def duration(o: collection.Map[String, ujson.Value], key: String) =
      o.get(key)
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(java.time.Duration.parse(t)).toOption)
        .filter(d => !d.isNegative && !d.isZero)
        .map(_.getSeconds.seconds)
        .toRight(s"a topics run's parameters hold $key, a positive ISO-8601 duration: $shown")
    for {
      o <- v.objOpt.toRight(s"a topics run's parameters are an object: $shown")
      perRun <- o
        .get("perRun")
        .flatMap(_.numOpt)
        .filter(n => n.isWhole && n >= 1 && n <= Int.MaxValue)
        .toRight(s"a topics run's parameters hold perRun, a whole number at least 1: $shown")
      recent <- duration(o, "recent")
      cardRecent <- duration(o, "cardRecent")
      asideFor <- if (o.contains("asideFor")) duration(o, "asideFor") else Right(AsideFor)
      day <- date(o, "day", shown)
      seedUntil <- date(o, "seedUntil", shown)
    } yield TopicParams(perRun.toInt, recent, cardRecent, day, seedUntil, asideFor)
  }

  private def date(
      o: collection.Map[String, ujson.Value],
      key: String,
      shown: String
  ): Either[String, Option[LocalDate]] =
    o.get(key) match {
      case None => Right(None)
      case Some(d) =>
        d.strOpt
          .flatMap(t => scala.util.Try(LocalDate.parse(t)).toOption)
          .map(Some(_))
          .toRight(s"a topics run's $key is yyyy-mm-dd: $shown")
    }
}
