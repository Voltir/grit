package grit.core.durable

import java.time.Instant

/** One step a workflow recorded, as a reader after the fact sees it: its `name`, its `output`
  * as its [[Journaled]] encoded it, and when it `started`, to the millisecond.
  *
  * @param output
  *   `None` for a step that recorded none (a patch's marker, a wait's record) or threw
  * @param started
  *   `None` where the record holds no start, as a history kept outside the runtime does not
  */
final case class StepRecord(name: String, output: Option[String], started: Option[Instant])
