package grit.lifecycle.stitch

import grit.core.durable.Journaled
import grit.core.id.EntryId
import grit.core.stitch.{Placed, StitchJson}

/** How a placement's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`, as
  * the triage records its own. A placement in flight must read back what an earlier build
  * wrote, so change these only with the workflow's epoch (ADR 0004).
  */
private[stitch] object StitchJournal {

  /** A `stitch` step's output: the opening and where it was placed
    * ([[StitchJson.writeKept]]), or `null` when nothing was asked.
    */
  given stitched: Journaled[Either[String, Option[(EntryId, Placed)]]] =
    outcome(StitchJson.writeKept, StitchJson.readKept)

  /** A `record-stitch` step's output: whether the placement was kept now. */
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
