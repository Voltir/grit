package grit.tui.runtime.loop

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import grit.tui.model.surface.Size
import grit.tui.wire.term.Terminal

/** A terminal that records instead of painting.
  *
  * STYLE rule 7's carve-out: a test double standing in for a mutable resource may hold
  * mutable state, because that state is the thing it exists to emulate. It records the
  * order of every lifecycle call, which is what makes "restore in reverse" and "the
  * scheduler stops before the terminal" assertable rather than hoped for.
  *
  * It enforces nothing the runtime owes: every `enterRaw` and `exitRaw` is logged, so a
  * runtime that calls either twice is seen doing it.
  */
final class FakeTerminal(val size: Size = Size(10, 20), readDelayMs: Long = 0L) extends Terminal {

  private val input = new LinkedBlockingQueue[String]()
  private val buffer = new StringBuilder
  private val flushed = new StringBuilder
  private val log = scala.collection.mutable.ArrayBuffer.empty[String]
  private var raw = false

  /** Every lifecycle call in the order it happened. */
  def calls: Vector[String] = synchronized { log.toVector }

  /** Everything flushed so far. */
  def painted: String = synchronized { flushed.result() }

  /** Whatever was last offered to the clipboard. */
  def copied: Vector[String] = synchronized {
    log.toVector.collect { case s if s.startsWith("copyOut:") => s.drop("copyOut:".length) }
  }

  /** Queue characters for the reader to pick up. */
  def send(chars: String): Unit = { val _ = input.offer(chars) }

  /** How many reads were **in flight** when [[close]] happened.
    *
    * The oracle for "the reader is provably stopped before the terminal goes away". It
    * has to be in-flight rather than begun-after: the reader stops on its own `running`
    * flag, so it never *starts* a read after teardown -- what an unjoined daemon does is
    * sit inside one while the modes are restored underneath it.
    */
  @volatile private var reading = 0
  @volatile private var inFlightAtClose = -1
  def readsInFlightAtClose: Int = inFlightAtClose

  def read(timeoutMs: Long): String = {
    reading += 1
    try {
      // A slow read is what makes the race real: without one, a reader racing teardown
      // usually happens to have finished already.
      if (readDelayMs > 0L) { Thread.sleep(readDelayMs) }
      val s = input.poll(timeoutMs, TimeUnit.MILLISECONDS)
      if (s == null) "" else s
    } finally { reading -= 1 }
  }

  /** Pretend the window changed; the next `resized()` reports it once. */
  def resize(): Unit = synchronized { pendingResize = true }

  private var pendingResize = false

  def resized(): Boolean = synchronized {
    val was = pendingResize
    pendingResize = false
    was
  }

  def write(s: String): Unit = synchronized { val _ = buffer.append(s) }

  /** Logged only when something was buffered: an empty flush writes nothing. */
  def flush(): Unit = synchronized {
    if (buffer.nonEmpty) {
      val _ = flushed.append(buffer.result())
      buffer.clear()
      log += "flush"
    }
  }

  def copyOut(text: String): Unit = synchronized { log += s"copyOut:$text" }

  def enterRaw(): Unit = synchronized { raw = true; log += "enterRaw" }

  def exitRaw(): Unit = synchronized { raw = false; log += "exitRaw" }

  /** Leaves the terminal as `exitRaw` does, logged as `close` alone. */
  def close(): Unit = synchronized { inFlightAtClose = reading; raw = false; log += "close" }

  /** True while the terminal is in raw mode -- for asserting it was given back. */
  def isRaw: Boolean = synchronized { raw }
}
