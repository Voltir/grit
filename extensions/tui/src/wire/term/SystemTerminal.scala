package grit.tui.wire.term

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.Base64
import java.util.concurrent.atomic.AtomicBoolean

import grit.tui.model.surface.Size

/** The real terminal.
  *
  * The only implementation behind [[Terminal]], and the only place in the library that
  * touches a device. Everything above it is a function of values.
  *
  * Modes are taken in one order and given back in exactly the reverse, guarded by a flag
  * that is set *before* the work -- because this is called from the runtime's `finally`,
  * from `close`, and from a shutdown hook, and a mode left on sprays escape sequences
  * into the user's shell for the rest of the session.
  */
final class SystemTerminal private (private val savedAttributes: Option[Array[Byte]])
    extends Terminal {

  import SystemTerminal.*

  private val out = new FileOutputStream(java.io.FileDescriptor.out)

  /* The raw fd, not `System.in`. With VMIN=0/VTIME=1 an idle read(2) returns 0 bytes,
   * which Java reports as -1 -- indistinguishable from end of stream. `System.in` is
   * wrapped and treats that as terminal EOF, so input dies after the first burst; a
   * FileInputStream on the descriptor keeps calling read(2) and recovers. */
  private val in = new java.io.FileInputStream(java.io.FileDescriptor.in)
  private val buffer = new StringBuilder
  private val winch = new AtomicBoolean(false)
  private val raw = new AtomicBoolean(false)
  private val decoder: CharsetDecoder = StandardCharsets.UTF_8
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)

  private val readBuf = new Array[Byte](2048)

  /** Leftover bytes plus one whole read. A multi-byte character split across two reads
    * must not decode as two replacement characters -- but the buffer has to hold the
    * *whole* read as well as the remainder, or a report longer than the leftover space
    * overflows it. It must never be smaller than `readBuf`.
    */
  private val pending = ByteBuffer.allocate(readBuf.length + 8)
  @volatile private var cached: Size = SystemTerminal.query()

  def size: Size = cached

  def resized(): Boolean =
    if (winch.getAndSet(false)) { cached = SystemTerminal.query(); true }
    else { false }

  /** Called by the signal handler. Stores a flag and nothing else. */
  private[term] def noteWinch(): Unit = { val _ = winch.set(true) }

  def read(timeoutMs: Long): String = {
    // The driver supplies the timeout (VMIN=0/VTIME=1), so an idle read returns nothing
    // rather than blocking; `timeoutMs` is the runtime's intent and the driver's
    // granularity is what it actually gets.
    val _ = timeoutMs
    val n = try { in.read(readBuf) }
    catch { case _: Throwable => -1 }
    if (n <= 0) { "" }
    else {
      val chars = java.nio.CharBuffer.allocate(pending.capacity)
      pending.put(readBuf, 0, n)
      pending.flip()
      val _ = decoder.decode(pending, chars, false)
      pending.compact()
      chars.flip()
      chars.toString
    }
  }

  def write(s: String): Unit = { val _ = buffer.append(s) }

  def flush(): Unit = {
    if (buffer.nonEmpty) {
      val bytes = buffer.result().getBytes(StandardCharsets.UTF_8)
      buffer.setLength(0)
      try { out.write(bytes); out.flush() }
      catch { case _: Throwable => () }
    }
  }

  /** OSC 52, plus `clip.exe` under WSL.
    *
    * There is no way to ask whether OSC 52 landed -- no reply, no error -- so an app
    * should report what it *tried*. Payloads above [[MaxOsc52Bytes]] are dropped rather
    * than truncated: many terminals cap the string parameter (xterm's default is ~8k)
    * and a silently halved clipboard is worse than an honest miss.
    */
  def copyOut(text: String): Unit = {
    val bytes = text.getBytes(StandardCharsets.UTF_8)
    val encoded = Base64.getEncoder.encodeToString(bytes)
    if (encoded.length <= MaxOsc52Bytes) {
      write(s"$Osc52Prefix$encoded$St")
      flush()
    }
    if (isWsl) {
      try {
        val p = new ProcessBuilder("clip.exe").redirectErrorStream(true).start()
        p.getOutputStream.write(bytes)
        p.getOutputStream.close()
        val _ = p.waitFor()
      } catch { case _: Throwable => () }
    }
  }

  def enterRaw(): Unit = {
    if (raw.compareAndSet(false, true)) {
      savedAttributes.foreach(a => { val _ = Posix.setAttributes(Posix.rawAttributes(a)) })
      write(AltScreenOn + ClearAll + Home + MouseOn + BracketedPasteOn + FocusOn + CursorHide)
      flush()
    }
  }

  /** The exact reverse of [[enterRaw]], including the two mouse modes. */
  def exitRaw(): Unit = {
    if (raw.compareAndSet(true, false)) {
      write(FocusOff + BracketedPasteOff + MouseOff + CursorShow + AltScreenOff)
      flush()
      savedAttributes.foreach(a => { val _ = Posix.setAttributes(a) })
    }
  }

  def close(): Unit = {
    exitRaw()
    try { out.flush() }
    catch { case _: Throwable => () }
  }
}

object SystemTerminal {

  private val Esc = "\u001b"
  private val Csi = Esc + "["
  private val Osc52Prefix = Esc + "]52;c;"
  private val St = Esc + "\\"

  private val AltScreenOn = Csi + "?1049h"
  private val AltScreenOff = Csi + "?1049l"
  private val ClearAll = Csi + "2J"
  private val Home = Csi + "H"
  private val CursorHide = Csi + "?25l"
  private val CursorShow = Csi + "?25h"
  private val FocusOn = Csi + "?1004h"
  private val FocusOff = Csi + "?1004l"
  private val BracketedPasteOn = Csi + "?2004h"
  private val BracketedPasteOff = Csi + "?2004l"

  /** Mode 1002 is button-event tracking: motion is reported only while a button is held.
    * 1003 adds a report per cell crossed with no button down, which this library does not
    * need -- the escaped drag is answered by a deadline, not by inferring a leave.
    */
  private val MouseOn = Csi + "?1002h" + Csi + "?1006h"
  private val MouseOff = Csi + "?1006l" + Csi + "?1002l"

  /** Base64 length above which OSC 52 is not attempted. */
  private val MaxOsc52Bytes = 8000

  /** The size when the terminal cannot be asked -- the conventional fallback. */
  private val Fallback = Size(24, 80)

  private def query(): Size =
    Posix.windowSize.map((r, c) => Size(r, c)).getOrElse(Fallback)

  private lazy val isWsl: Boolean =
    try {
      new String(Files.readAllBytes(Path.of("/proc/version")), StandardCharsets.UTF_8).toLowerCase
        .contains("microsoft")
    } catch { case _: Throwable => false }

  /** True when stdin and stdout are both terminals -- the gate for taking the screen.
    *
    * `isatty` rather than a heuristic: a non-tty run should render a snapshot, and this
    * is the same call that fails to produce a size.
    */
  def isInteractive: Boolean = Posix.isInteractive

  /** A terminal ready to be entered, or None when this process is not attached to one.
    *
    * Installs the SIGWINCH handler, whose body is a single flag store; the runtime
    * observes it through [[Terminal.resized]] on its next read. Also installs TERM and
    * HUP handlers that restore before exiting -- raw mode clears ISIG, so Ctrl-C arrives
    * as a byte and never as a signal, which leaves `kill` as the only way to stop the
    * process and makes an unrestored terminal the default outcome without this.
    */
  def open(): Option[SystemTerminal] =
    if (!isInteractive) { None }
    else {
      val term = new SystemTerminal(Posix.getAttributes)
      install("WINCH", () => term.noteWinch())
      install("TERM", () => { term.close(); Runtime.getRuntime.halt(0) })
      install("HUP", () => { term.close(); Runtime.getRuntime.halt(0) })
      val hook = new Thread(() => term.close(), "tui-restore")
      Runtime.getRuntime.addShutdownHook(hook)
      Some(term)
    }

  /** Best-effort signal registration. `sun.misc.Signal` is an internal API and may not be
    * present; a terminal without a WINCH handler still works, it just learns about a
    * resize only when the next size query happens to differ.
    */
  private def install(name: String, action: () => Unit): Unit =
    try {
      val _ = sun.misc.Signal.handle(
        new sun.misc.Signal(name),
        new sun.misc.SignalHandler { def handle(s: sun.misc.Signal): Unit = action() }
      )
    } catch { case _: Throwable => () }
}
