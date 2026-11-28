package grit.eval.harness.bench

/** A trial's grade, judged against its task's goal: Complete is the pass. */
enum Grade extends caps.Pure {
  case Complete, Partial, Broken, None
}

object Grade {

  /** `None` when the trial changed no source line ([[DiffLines]]), whatever its checks say;
    * `Broken` when a task check failed (a HEAD that does not compile fails them); `Partial` when a
    * held check failed, its suite not compiling among them, or a completion item was missed; else
    * `Complete`. An item `completed` could not read counts for nothing.
    */
  def of(checked: Checked, held: Checked, completed: Completed, changed: Boolean): Grade =
    if (!changed) Grade.None
    else if (!checked.passed) Broken
    else if (!held.passed || completed.missed.nonEmpty) Partial
    else Complete
}
