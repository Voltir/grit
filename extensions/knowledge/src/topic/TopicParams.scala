package grit.knowledge.topic

import java.time.LocalDate

import scala.concurrent.duration.*

/** What a topics run is told: at most how many topics it writes (`perRun`); how far back a
  * strand's last message (`recent`) and a card's write (`cardRecent`) may be for it to be looked
  * at; and the `day` it writes for, the day of its slot in its job's zone when none: a backfill
  * names a past day, and its strands are read as they stood at that day's end.
  */
final case class TopicParams(
    perRun: Int,
    recent: FiniteDuration,
    cardRecent: FiniteDuration,
    day: Option[LocalDate]
) extends caps.Pure

object TopicParams {

  /** Its record: `{"perRun", "recent", "cardRecent", "day"?}`, durations ISO-8601, the day
    * `yyyy-mm-dd`.
    */
  def write(p: TopicParams): ujson.Value = {
    val o = ujson.Obj(
      "perRun" -> p.perRun,
      "recent" -> java.time.Duration.ofSeconds(p.recent.toSeconds).toString,
      "cardRecent" -> java.time.Duration.ofSeconds(p.cardRecent.toSeconds).toString
    )
    p.day.foreach(d => o("day") = d.toString)
    o
  }

  /** The parameters [[write]] wrote, or why `v` holds none: `perRun` not a whole number at
    * least 1, a duration not positive ISO-8601, or a day not `yyyy-mm-dd`.
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
      day <- o.get("day") match {
        case None => Right(None)
        case Some(d) =>
          d.strOpt
            .flatMap(t => scala.util.Try(LocalDate.parse(t)).toOption)
            .map(Some(_))
            .toRight(s"a topics run's day is yyyy-mm-dd: $shown")
      }
    } yield TopicParams(perRun.toInt, recent, cardRecent, day)
  }
}
