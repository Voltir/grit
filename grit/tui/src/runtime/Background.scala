package grit.tui.runtime

import scala.util.control.NonFatal

/** A background activity whose lifetime is bounded by [[close]].
  *
  * `close` interrupts *and joins*, so the activity has stopped by the time it returns.
  * That is the whole point and the reason this is not a bare daemon thread: a daemon
  * that is never joined can still be inside a call to the terminal while the terminal is
  * being restored, and the failure is a garbled shell rather than an exception anyone
  * sees.
  *
  * Being `AutoCloseable` is what lets `Using.Manager` order it: registered last, released
  * first, so the reader is provably stopped before the things it touches go away.
  */
final class Background private[runtime] (thread: Thread) extends AutoCloseable {

  /** Stop the activity and wait for it. Idempotent: joining a dead thread returns. */
  def close(): Unit = {
    thread.interrupt()
    thread.join()
  }
}

object Background {

  /** Run `body` on a virtual thread until it returns or the background is closed.
    *
    * The capture set is the closure's, so the handle cannot outlive what it captured --
    * a background that escaped its resources is not expressible rather than merely
    * discouraged.
    */
  def start(name: String)(body: () => Unit): Background^{body} = {
    val thread = Thread
      .ofVirtual()
      .name(name)
      .unstarted { () =>
        // An interrupt is how `close` says stop, so it is an ending, not a failure.
        // Anything else would otherwise kill the thread silently and take whatever it
        // was doing -- reading input, usually -- with it.
        try body()
        catch {
          case _: InterruptedException => ()
          case NonFatal(_) => ()
        }
      }
    thread.start()
    new Background(thread)
  }

}
