package grit.core.tool

/** What happens to a call whose run was cut short by the edge running it dying, before it
  * answered. It is about whether a call may run twice, never about permission, which is the
  * [[Gate]]'s.
  */
enum Retry(val key: String) {

  /** It is run again, by whichever edge serving its workspace finds it. Declaring this
    * promises that running the call twice leaves what running it once does.
    */
  case Rerun extends Retry("rerun")

  /** It is answered [[Outcome.Interrupted]] and never run again: the model is told it may
    * have partly run, and checks before redoing it. The default.
    */
  case Interrupt extends Retry("interrupt")
}

object Retry {

  /** The retry whose [[Retry.key]] is `key`; `None` for no retry's. */
  def of(key: String): Option[Retry] = Retry.values.find(_.key == key)
}
