package grit.tui.wire.term

import grit.tui.model.surface.Size

/** The terminal, as a capability.
  *
  * The one seam. Everything above it is a function of values; this is where the process
  * touches a device, and the type says so -- a signature without a `Terminal` is a
  * promise that nothing was painted, read, or copied.
  *
  * Implementations own the escape bytes for mode setting and the clipboard. Nothing here
  * paints: the painter produces a string and the runtime hands it to [[write]], so the
  * "one write per frame" rule is visible at this boundary rather than buried.
  */
trait Terminal extends caps.SharedCapability {

  /** The terminal's current size in cells. Re-read after a resize; never cached by the
    * caller, because layoutz sampling width once at startup is exactly the bug.
    */
  def size: Size

  /** Characters available within `timeoutMs`, or `""` if none arrived.
    *
    * A timeout rather than a blocking read, because the ambiguous-ESC decision belongs
    * to whoever holds the clock: an empty return is what tells the runtime to resolve a
    * held prefix with [[grit.tui.wire.input.Decoder.flush]].
    */
  def read(timeoutMs: Long): String

  /** True exactly once per observed size change, consuming the notification.
    *
    * The runtime asks after every read, and an idle read already returns on the driver's
    * own timeout -- so the read timeout is the poll, and a resize costs no timer chain.
    * The layoutz spike needed a 200ms heartbeat for this only because it had no queue to
    * push a message onto.
    */
  def resized(): Boolean

  /** Buffer `s` for the next [[flush]]. */
  def write(s: String): Unit

  /** Send everything buffered, as a single write. */
  def flush(): Unit

  /** Offer `text` to the system clipboard. Best-effort and unverifiable -- OSC 52 cannot
    * be queried -- so an app should report what it *tried*, never what it achieved.
    */
  def copyOut(text: String): Unit

  /** Take the screen: raw mode, alternate screen, mouse reporting, cursor hidden.
    * Idempotent.
    */
  def enterRaw(): Unit

  /** Give it back, in the exact reverse of [[enterRaw]]. Idempotent, because it is
    * called from cleanup, from a `finally`, and from a shutdown hook.
    */
  def exitRaw(): Unit

  /** Release everything. Must leave the terminal as [[exitRaw]] does. */
  def close(): Unit
}

object Terminal {

  /** Gives the screen back, then releases the device -- so `Using` can order the
    * terminal against everything that paints on it.
    *
    * Both halves, and in that order, because they are different promises: [[exitRaw]]
    * undoes the modes in reverse, [[close]] releases whatever the implementation holds.
    * Both are idempotent, so this is safe however the caller already tore down.
    */
  given releasable[C^]: scala.util.Using.Releasable[Terminal^{C}] = (t: Terminal^{C}) => {
    t.exitRaw()
    t.close()
  }
}
