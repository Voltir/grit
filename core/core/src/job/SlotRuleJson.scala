package grit.core.job

import java.time.{DayOfWeek, Duration, Instant, LocalTime, ZoneId}
import java.util.Locale

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** A [[SlotRule]]'s stored form, a schedule row's `rule`:
  * `{"kind":"once","at":"2026-10-07T09:00:00Z","grace":"PT1H"}` (the grace an ISO-8601
  * duration), `{"kind":"daily","at":"09:00","zone":"Europe/Paris"}`, `"weekdays"` likewise, and
  * `{"kind":"weekly","day":"wednesday","at":"17:00","zone":…}`.
  */
object SlotRuleJson {

  def write(rule: SlotRule): ujson.Value = rule match {
    case SlotRule.Once(at, grace) =>
      ujson.Obj(
        "kind" -> "once",
        "at" -> at.toString,
        "grace" -> Duration.ofNanos(Grace.value(grace).toNanos).toString
      )
    case SlotRule.Daily(at, zone) =>
      ujson.Obj("kind" -> "daily", "at" -> at.toString, "zone" -> zone.getId)
    case SlotRule.Weekdays(at, zone) =>
      ujson.Obj("kind" -> "weekdays", "at" -> at.toString, "zone" -> zone.getId)
    case SlotRule.Weekly(day, at, zone) =>
      ujson.Obj(
        "kind" -> "weekly",
        "day" -> day.name.toLowerCase(Locale.ROOT),
        "at" -> at.toString,
        "zone" -> zone.getId
      )
  }

  /** The rule stored as `v` ([[write]]'s form), or why it is none. */
  def read(v: ujson.Value): Either[String, SlotRule] = {
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      o.get(key).flatMap(_.strOpt).toRight(s"a slot rule has no $key")
    def parsed[A](o: collection.Map[String, ujson.Value], key: String, what: String)(
        parse: String => Option[A]
    ): Either[String, A] =
      str(o, key).flatMap(text => parse(text).toRight(s"a slot rule's $key is no $what: $text"))
    def localTime(o: collection.Map[String, ujson.Value]) =
      parsed(o, "at", "local time")(t => Try(LocalTime.parse(t)).toOption)
    def zone(o: collection.Map[String, ujson.Value]) =
      parsed(o, "zone", "zone")(z => Try(ZoneId.of(z)).toOption)
    for {
      o <- v.objOpt.toRight("a slot rule is not an object")
      kind <- str(o, "kind")
      rule <- kind match {
        case "once" =>
          for {
            at <- parsed(o, "at", "instant")(t => Try(Instant.parse(t)).toOption)
            grace <- parsed(o, "grace", "grace")(g =>
              Try(FiniteDuration(Duration.parse(g).toNanos, "nanos")).toOption.flatMap(Grace.of)
            )
          } yield SlotRule.Once(at, grace)
        case "daily" =>
          for { at <- localTime(o); z <- zone(o) } yield SlotRule.Daily(at, z)
        case "weekdays" =>
          for { at <- localTime(o); z <- zone(o) } yield SlotRule.Weekdays(at, z)
        case "weekly" =>
          for {
            day <- parsed(o, "day", "day")(d =>
              DayOfWeek.values.find(_.name.toLowerCase(Locale.ROOT) == d)
            )
            at <- localTime(o)
            z <- zone(o)
          } yield SlotRule.Weekly(day, at, z)
        case other => Left(s"no slot rule $other")
      }
    } yield rule
  }
}
