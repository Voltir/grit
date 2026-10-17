package grit.knowledge.sync

/** One run's walk of one source, its listings read as `L`, as data: from the cards it holds,
  * each next step ([[Source.walk]]).
  */
trait Walk[L <: caps.Pure] extends caps.Pure {

  /** Its next step, or what the run comes to when no step is left or one stopped it. */
  def next: Either[Synced, Step]

  /** This walk once its last [[next]] step came to `outcome`; an outcome not of that step's kind
    * stops it, saying so; once it has come to its [[Synced]], this walk.
    */
  def after(outcome: Outcome[L]): Walk[L]
}
