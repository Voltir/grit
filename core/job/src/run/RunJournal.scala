package grit.job.run

import java.time.Instant

import scala.util.Try

import grit.core.durable.Journaled
import grit.core.id.JobName
import grit.core.job.{ReportJson, Slot}

/** How a run's step outputs are recorded. A run in flight must read back what an earlier build
  * wrote, so change these only with the workflow's epoch (ADR 0004).
  *   - `read-slot`: `{"kind":"read","slot":key,"job":…,"version":n,"started":instant,
  *     "params":…,"report":…}` (a [[Slot.key]], a [[ReportJson]]), or
  *     `{"kind":"unreadable","why":…}`;
  *   - `reply`: `{"kind":"replied","text":…}`, `{"kind":"superseded","current":n}`,
  *     `{"kind":"jobless"}` or `{"kind":"failed","why":…}`.
  */
object RunJournal {

  given slotRead: Journaled[SlotRead] = Journaled.json(writeSlotRead, readSlotRead)

  given runEnd: Journaled[RunEnd] = Journaled.json(writeRunEnd, readRunEnd)

  def writeSlotRead(read: SlotRead): ujson.Value = read match {
    case SlotRead.Read(slot, job, version, started, params, report) =>
      ujson.Obj(
        "kind" -> "read",
        "slot" -> slot.key,
        "job" -> JobName.value(job),
        "version" -> version,
        "started" -> started.toString,
        "params" -> params,
        "report" -> ReportJson.write(report)
      )
    case SlotRead.Unreadable(why) => ujson.Obj("kind" -> "unreadable", "why" -> why)
  }

  /** The `read-slot` output recorded as `v` ([[writeSlotRead]]'s form), or why it is none. */
  def readSlotRead(v: ujson.Value): Either[String, SlotRead] =
    fields(v, "read-slot").flatMap { o =>
      str(o, "kind").flatMap {
        case "read" =>
          for {
            key <- str(o, "slot")
            slot <- Slot.read(key).toRight(s"read-slot: no slot $key")
            name <- str(o, "job")
            job <- JobName.of(name).left.map(why => s"read-slot: $why")
            version <- int(o, "version")
            at <- str(o, "started")
            started <- Try(Instant.parse(at)).toOption.toRight(s"read-slot: no instant $at")
            params <- o.get("params").toRight("read-slot has no params")
            report <- o.get("report").toRight("read-slot has no report").flatMap(ReportJson.read)
          } yield SlotRead.Read(slot, job, version, started, params, report)
        case "unreadable" => str(o, "why").map(SlotRead.Unreadable(_))
        case other => Left(s"read-slot: no kind $other")
      }
    }

  def writeRunEnd(end: RunEnd): ujson.Value = end match {
    case RunEnd.Replied(text) => ujson.Obj("kind" -> "replied", "text" -> text)
    case RunEnd.Superseded(current) => ujson.Obj("kind" -> "superseded", "current" -> current)
    case RunEnd.Jobless => ujson.Obj("kind" -> "jobless")
    case RunEnd.Failed(why) => ujson.Obj("kind" -> "failed", "why" -> why)
  }

  /** The `reply` output recorded as `v` ([[writeRunEnd]]'s form), or why it is none. */
  def readRunEnd(v: ujson.Value): Either[String, RunEnd] =
    fields(v, "reply").flatMap { o =>
      str(o, "kind").flatMap {
        case "replied" => str(o, "text").map(RunEnd.Replied(_))
        case "superseded" => int(o, "current").map(RunEnd.Superseded(_))
        case "jobless" => Right(RunEnd.Jobless)
        case "failed" => str(o, "why").map(RunEnd.Failed(_))
        case other => Left(s"reply: no kind $other")
      }
    }

  private def fields(
      v: ujson.Value,
      step: String
  ): Either[String, collection.Map[String, ujson.Value]] =
    v.objOpt.toRight(s"$step: expected an object")

  private def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
    o.get(key).flatMap(_.strOpt).toRight(s"expected a string '$key'")

  private def int(o: collection.Map[String, ujson.Value], key: String): Either[String, Int] =
    o.get(key)
      .flatMap(_.numOpt)
      .filter(n => n.isWhole && n.isValidInt)
      .map(_.toInt)
      .toRight(s"expected a whole number '$key'")
}
