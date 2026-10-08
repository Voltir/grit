package grit.act.phase

import grit.core.provider.Delta

/** Where a model's response is told as it is generated: a fresh hearer for each attempt, so a
  * retried attempt's pieces follow the failed one's (ADR 0006).
  */
trait Hearing extends caps.SharedCapability {

  /** The hearer of one attempt. */
  def attempt(): Heard^
}

/** The hearer of one attempt at a response. */
trait Heard extends caps.SharedCapability {

  /** Tells `delta`, in order. */
  def tell(delta: Delta): Unit

  /** The attempt ended, whatever it came to: anything held back is told now. */
  def done(): Unit
}

object Hearing {

  /** A hearing that tells no one. */
  def silent(): Hearing^ = new Hearing {
    def attempt(): Heard^ = new Heard {
      def tell(delta: Delta): Unit = ()
      def done(): Unit = ()
    }
  }
}
