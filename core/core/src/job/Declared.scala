package grit.core.job

import grit.core.id.{Declarer, ScheduleId, ScheduleKey}
import grit.core.visibility.Label

/** A schedule a deployment or plugin declares (ADR 0029): run for grit
  * (`grit.core.id.PrincipalId.Grit`), reported only in its runs' own conversations, and
  * reconciled with the stored ones at every start: changed in place when changed, ended when no
  * longer declared. `clearance` is what its runs read beyond their own conversations, and the
  * label they and what they keep are kept at (ADR 0030); a deployment is refused unless its
  * visibility declares each compartment `clearance` holds.
  */
final case class Declared[P <: caps.Pure](
    key: ScheduleKey,
    job: Job[P],
    rule: SlotRule,
    params: P,
    clearance: Label = Label.Public
) {

  /** Its id, declared by `by`. */
  def id(by: Declarer): ScheduleId = ScheduleId.declared(by, key)

  /** Its parameters as its schedule keeps them. */
  def written: ujson.Value = job.write(params)
}

/** How a schedule ended. */
enum Ending(val word: String) {
  case Ran extends Ending("ran")
  case Missed extends Ending("missed")
  case Failed extends Ending("failed")
  case Cancelled extends Ending("cancelled")
  case Undeclared extends Ending("undeclared")
}

object Ending {

  /** The ending whose stored [[Ending.word]] is `word`, or `None`. */
  def read(word: String): Option[Ending] = values.find(_.word == word)
}
