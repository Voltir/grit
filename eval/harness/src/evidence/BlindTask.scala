package grit.eval.harness.evidence

import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{BenchTask, Sha}

/** A frozen task as a proposer may see it: its name, the commit its coder started from, and the
  * brief its coder was given, and nothing that grades a run of it.
  */
final case class BlindTask private (name: TrialRows.TaskName, base: Sha, brief: String)
    extends caps.Pure

object BlindTask {

  /** `task` with its commits and its checks left out. */
  def of(task: BenchTask): BlindTask =
    BlindTask(task.name, task.base, task.brief)
}
