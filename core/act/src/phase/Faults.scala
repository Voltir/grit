package grit.act.phase

/** How a phase's failures become its caller's `F`, so each caller's steps keep their own
  * recorded forms.
  */
trait Faults[F] {

  /** The store could not be read or written: `reason`. */
  def store(reason: String): F
}
