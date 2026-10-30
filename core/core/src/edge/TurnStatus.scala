package grit.core.edge

import java.time.Instant

import grit.core.durable.StepRecord

/** Where a turn's workflow is, as an edge sees it. */
enum TurnStatus {

  /** Queued or running, having recorded these `steps` so far. */
  case Running(steps: Vector[RecordedStep])

  /** Done: the workflow's output, or why it has none. */
  case Finished(output: String)

  /** Not (yet) known to DBOS: ingested but not enqueued. */
  case Unknown
}

/** A step a turn's workflow recorded: what a reader of its history sees of it (`record`), and
  * when it completed, as far as DBOS kept it.
  */
final case class RecordedStep(record: StepRecord, completed: Option[Instant]) {
  def name: String = record.name
  def started: Option[Instant] = record.started
}
