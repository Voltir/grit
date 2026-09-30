package grit.lifecycle.triage

import grit.core.durable.Journaled
import grit.core.id.{EntryId, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Decision, SpeechJson}
import grit.core.stitch.{Placed, StitchJson}
import grit.core.store.PayloadJson
import grit.core.triage.{Kind, Tags}

/** How the triage's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`. A
  * triage in flight must read back what an earlier build wrote, so change these only with
  * the workflow's epoch (ADR 0004).
  */
private[triage] object TriageJournal {

  private val Weights: Vector[String] = Vector("kindP", "waiting", "durable", "helps")

  /** `tags` as recorded: its kind, the four probabilities, its model and usage; or
    * `{"unanswered": why}`.
    */
  def writeTags(tags: Tags): ujson.Value = tags match {
    case Tags.Weighed(kind, kindP, waiting, durable, helps, model, usage) =>
      val o = ujson.Obj("kind" -> Kind.written(kind))
      Weights.zip(Vector(kindP, waiting, durable, helps)).foreach { (key: String, p: Probability) =>
        o(key) = Probability.value(p)
      }
      o("model") = model
      o("usage") = PayloadJson.writeUsage(usage)
      o
    case Tags.Unanswered(why) => ujson.Obj("unanswered" -> why)
  }

  def readTags(v: ujson.Value): Either[String, Tags] =
    v.objOpt.toRight("tags: expected an object").flatMap { o =>
      o.get("unanswered") match {
        case Some(ujson.Str(why)) => Right(Tags.Unanswered(why))
        case Some(_) => Left("tags: unanswered is not a string")
        case None =>
          val ps = Weights.flatMap(k => o.get(k).flatMap(_.numOpt).flatMap(Probability.of))
          for {
            kind <- o.get("kind").flatMap(_.strOpt).flatMap(Kind.read).toRight("tags: bad kind")
            model <- o.get("model").flatMap(_.strOpt).toRight("tags: missing model")
            usage <- o.get("usage").toRight("tags: missing usage").flatMap(PayloadJson.readUsage)
            tags <- ps match {
              case Vector(kindP, waiting, durable, helps) =>
                Right(Tags.Weighed(kind, kindP, waiting, durable, helps, model, usage))
              case _ => Left("tags: expected four probabilities")
            }
          } yield tags
      }
    }

  given asked: Journaled[Either[String, (EntryId, Tags)]] =
    outcome(
      (entry, tags) => ujson.Obj("entry" -> EntryId.value(entry), "tags" -> writeTags(tags)),
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          entry <- o.get("entry").flatMap(_.strOpt).toRight("asked: missing entry")
          tags <- o.get("tags").toRight("asked: missing tags").flatMap(readTags)
        } yield (EntryId(entry), tags)
    )

  /** A `stitch` step's output: the heard message and where it was placed
    * ([[StitchJson.write]]), or `null` when nothing was asked.
    */
  given stitched: Journaled[Either[String, Option[(EntryId, Placed)]]] =
    outcome(
      {
        case Some((entry, placed)) =>
          ujson.Obj("entry" -> EntryId.value(entry), "placed" -> StitchJson.write(placed))
        case None => ujson.Null
      },
      {
        case ujson.Null => Right(None)
        case v =>
          for {
            o <- v.objOpt.toRight("stitched: expected an object")
            entry <- o.get("entry").flatMap(_.strOpt).toRight("stitched: missing entry")
            placed <- o.get("placed").toRight("stitched: missing placed").flatMap(StitchJson.read)
          } yield Some((EntryId(entry), placed))
      }
    )

  /** A `consider` step's output: the decision ([[SpeechJson.writeDecision]]). */
  given considered: Journaled[Either[String, Decision]] =
    outcome(SpeechJson.writeDecision, SpeechJson.readDecision)

  /** A `start` step's output: the workflow id of the turn it queued. */
  given started: Journaled[Either[String, WorkflowId]] =
    outcome(
      w => ujson.Str(WorkflowId.value(w)),
      v => v.strOpt.map(WorkflowId(_)).toRight("started: expected a workflow id")
    )

  given recorded: Journaled[Either[String, Boolean]] =
    outcome(ujson.Bool(_), _.boolOpt.toRight("recorded: expected a boolean"))

  private def outcome[A](
      write: A -> ujson.Value,
      read: ujson.Value -> Either[String, A]
  ): Journaled[Either[String, A]] =
    Journaled.json[Either[String, A]](
      {
        case Right(a) => ujson.Obj("ok" -> write(a))
        case Left(reason) => ujson.Obj("failed" -> reason)
      },
      v =>
        v.objOpt match {
          case None => Left("expected an object")
          case Some(o) =>
            (o.get("ok"), o.get("failed").flatMap(_.strOpt)) match {
              case (Some(value), None) => read(value).map(Right(_))
              case (None, Some(reason)) => Right(Left(reason))
              case _ => Left("expected {ok} or {failed}")
            }
        }
    )
}
