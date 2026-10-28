package grit.lifecycle.shadow

import grit.core.durable.Journaled
import grit.core.durable.Journaled.outcome
import grit.core.id.EntryId
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
  given recorded: Journaled[Either[String, Boolean]] = Journaled.flag

}
