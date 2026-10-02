package grit.lifecycle.shadow

import grit.core.durable.Journaled
import grit.core.id.EntryId
import grit.core.store.StoreError
import grit.core.triage.{Shadowed, ShadowedJson}

/** How the shadow's step outputs are recorded: `{"ok": value}` or `{"failed": reason}`, as
  * triage's are. A shadow in flight must read back what an earlier build wrote, so change
  * these only with the workflow's epoch (ADR 0004).
  */
private[shadow] object ShadowJournal {

  /** An `ask` step's output: the heard message and what the variant made of it
    * ([[ShadowedJson.write]]).
    */
  given asked: Journaled[Either[String, (EntryId, Shadowed)]] =
    outcome(
      (entry, row) => ujson.Obj("entry" -> EntryId.value(entry), "row" -> ShadowedJson.write(row)),
      v =>
        for {
          o <- v.objOpt.toRight("asked: expected an object")
          entry <- o.get("entry").flatMap(_.strOpt).toRight("asked: missing entry")
          row <- o.get("row").toRight("asked: missing row").flatMap(ShadowedJson.read)
        } yield (EntryId(entry), row)
    )

  /** A `record` step's output: whether the row was kept. */
  given recorded: Journaled[Either[String, Boolean]] =
    outcome(ujson.Bool(_), _.boolOpt.toRight("recorded: expected a boolean"))

  /** `error` in the words the journal keeps. */
  def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }

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
