package grit.lifecycle.triage

import grit.core.durable.Journaled
import grit.core.id.{EntryId, WorkflowId}
import grit.core.speech.{Decision, SpeechJson}
import grit.core.stitch.{Placed, StitchJson}
import grit.core.triage.{Tags, TagsJson}

/** How the triage's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`. A
  * triage in flight must read back what an earlier build wrote, so change these only with
  * the workflow's epoch (ADR 0004).
  */
private[triage] object TriageJournal {

  given asked: Journaled[Either[String, (EntryId, Tags)]] =
    outcome(
      (entry, tags) => ujson.Obj("entry" -> EntryId.value(entry), "tags" -> TagsJson.write(tags)),
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          entry <- o.get("entry").flatMap(_.strOpt).toRight("asked: missing entry")
          tags <- o.get("tags").toRight("asked: missing tags").flatMap(TagsJson.read)
        } yield (EntryId(entry), tags)
    )

  /** A `stitch` step's output: the heard message and where it was placed
    * ([[StitchJson.write]]), or `null` when nothing was asked.
    */
  given stitched: Journaled[Either[String, Option[(EntryId, Placed)]]] =
    outcome(StitchJson.writeKept, StitchJson.readKept)

  /** A `stitched` step's output: what the placement it waited for did, or `null` when it
    * waited for none.
    */
  given waited: Journaled[Either[String, Option[String]]] =
    outcome(
      _.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      {
        case ujson.Null => Right(None)
        case ujson.Str(what) => Right(Some(what))
        case _ => Left("placed: expected a string or null")
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
