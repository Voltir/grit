package grit.core.durable

import grit.core.durable.InMemoryDurable.Outcome

/** What recorded histories hold, in bytes, against budgets: so a step that starts recording
  * more than its kind needs goes red beside the replay gate that reads its histories, rather
  * than growing every workflow's rows unseen.
  */
object RecordedSizes {

  /** The bytes `step` records: its output, or its error's message; none for a marker. */
  def of(step: InMemoryDurable.Step): Int = step.outcome match {
    case Outcome.Output(value) => value.getBytes("UTF-8").length
    case Outcome.Threw(message) => message.fold(0)(_.getBytes("UTF-8").length)
    case Outcome.Marker => 0
  }

  /** `name`'s family as a [[StepName]] records it, `family` or `family:key`, with a move's
    * phase kept (`family:key:phase`, as grit's moves name theirs): `post:3` is `post`,
    * `ask:a:record` is `ask:record`.
    */
  def familyOf(name: String): String = name.split(':').toVector match {
    case first +: _ +: rest if rest.nonEmpty => s"$first:${rest.last}"
    case first +: _ => first
    case _ => name
  }

  /** Every breach among `histories`, each by its file's name: a step whose family (`family`
    * names it) has no budget in `budgets`, a step recording more than its family's budget,
    * and a history recording more than `whole` in all. Each line names the history, the step,
    * its bytes and the budget. Empty when every history fits.
    */
  def breaches(
      histories: Vector[(String, History)],
      family: String => String,
      budgets: Map[String, Int],
      whole: Int
  ): Vector[String] =
    histories.flatMap { (file, history) =>
      val steps = history.steps.flatMap { step =>
        val (kind, size) = (family(step.name), of(step))
        budgets.get(kind) match {
          case None =>
            Vector(s"$file: step ${step.name}, of family $kind, has no budget: give it one")
          case Some(budget) if size > budget =>
            Vector(
              s"$file: step ${step.name} records $size bytes, over the budget of $budget for " +
                s"$kind: content with a content-addressed home is recorded by its id"
            )
          case Some(_) => Vector.empty
        }
      }
      val total = history.steps.map(of).sum
      steps ++ Option.when(total > whole)(
        s"$file: its steps record $total bytes in all, over the budget of $whole"
      )
    }
}
