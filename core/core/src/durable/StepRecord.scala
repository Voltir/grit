package grit.core.durable

/** One step a workflow recorded, as a reader after the fact sees it: its `name`, and its
  * `output` as its [[Journaled]] encoded it; `None` for a step that recorded none (a patch's
  * marker, a wait's record) or threw.
  */
final case class StepRecord(name: String, output: Option[String])
