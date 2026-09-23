package grit.tui

/** A background activity whose lifetime is bounded by `close`.
  *
  * `close` interrupts and joins, so the activity has stopped by the time it returns.
  */
final class Background private[tui] (thread: Thread) extends AutoCloseable {
  def close(): Unit = {
    thread.interrupt()
    thread.join()
  }
}

object Background {

  /** Run `body` on a virtual thread until it returns or the background is closed.
    */
  def start(name: String)(body: () => Unit): Background^{body} = {
    val thread = Thread.ofVirtual().name(name).unstarted { () =>
      try body()
      catch { case _: InterruptedException => () }
    }
    thread.start()
    new Background(thread)
  }
}
