package grit.tui.wire.term

import java.lang.foreign.{
  Arena,
  FunctionDescriptor,
  Linker,
  MemorySegment,
  SymbolLookup,
  ValueLayout
}
import java.lang.invoke.MethodHandle

/** The libc calls the terminal seam needs, bound with FFM.
  *
  * No subprocess: the `stty` fork both siblings used costs a process per call and cannot
  * report failure usefully. No JLine either -- grit.tui has no dependencies -- so the struct
  * layouts and constants are written out here, for Linux x86-64.
  *
  * Requires `--enable-native-access=ALL-UNNAMED` (in `forkArgs`). Without it a downcall
  * prints a restricted-method warning to *stderr*, which for an app holding the alternate
  * screen lands in the middle of the frame.
  *
  * Every entry point returns an `Option` or a total value: this is the boundary where a
  * missing symbol or a non-tty has to become a value rather than an exception.
  */
private[term] object Posix {

  /** struct termios, Linux x86-64: four 4-byte flag words, c_line, then NCCS=32 control
    * characters, 3 bytes of padding, and the two speeds. 60 bytes total.
    */
  private val TermiosSize = 60
  private val OffIflag = 0
  private val OffOflag = 4
  private val OffLflag = 12
  private val OffCc = 17

  /** c_cc indices. Linux orders VTIME before VMIN -- the reverse of macOS. */
  private val VTime = 5
  private val VMin = 6

  // c_iflag
  private val IXON = 0x0400
  private val ICRNL = 0x0100
  private val INLCR = 0x0040
  // c_oflag
  private val OPOST = 0x0001
  // c_lflag
  private val ICANON = 0x0002
  private val ECHO = 0x0008
  private val ISIG = 0x0001
  private val IEXTEN = 0x8000

  private val TCSANOW = 0
  private val TIOCGWINSZ = 0x5413L

  private val StdIn = 0
  private val StdOut = 1

  private lazy val linker: Option[Linker] =
    try { Some(Linker.nativeLinker()) }
    catch { case _: Throwable => None }

  private def handle(name: String, descriptor: FunctionDescriptor): Option[MethodHandle] =
    linker.flatMap { l =>
      try {
        val lookup: SymbolLookup = l.defaultLookup()
        val found = lookup.find(name)
        if (found.isPresent) { Some(l.downcallHandle(found.get, descriptor)) }
        else { None }
      } catch { case _: Throwable => None }
    }

  private lazy val isattyH: Option[MethodHandle] =
    handle("isatty", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT))

  /** `int ioctl(int, unsigned long, ...)` -- the third argument is variadic, and on the
    * SysV x86-64 ABI that changes how the call is set up. Declaring it non-variadic
    * happens to work for one pointer argument but is not the contract.
    */
  private lazy val ioctlH: Option[MethodHandle] =
    linker.flatMap { l =>
      try {
        val found = l.defaultLookup().find("ioctl")
        if (found.isPresent) {
          Some(
            l.downcallHandle(
              found.get,
              FunctionDescriptor.of(
                ValueLayout.JAVA_INT,
                ValueLayout.JAVA_INT,
                ValueLayout.JAVA_LONG,
                ValueLayout.ADDRESS
              ),
              Linker.Option.firstVariadicArg(2)
            )
          )
        } else { None }
      } catch { case _: Throwable => None }
    }

  private lazy val tcgetattrH: Option[MethodHandle] =
    handle(
      "tcgetattr",
      FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.ADDRESS)
    )

  private lazy val tcsetattrH: Option[MethodHandle] =
    handle(
      "tcsetattr",
      FunctionDescriptor.of(
        ValueLayout.JAVA_INT,
        ValueLayout.JAVA_INT,
        ValueLayout.JAVA_INT,
        ValueLayout.ADDRESS
      )
    )

  private def callInt(h: MethodHandle, args: Object*): Int =
    try { h.invokeWithArguments(args*).asInstanceOf[Int] }
    catch { case _: Throwable => -1 }

  /** Whether `fd` is a terminal. False when FFM is unavailable, which is the honest
    * answer for the purpose: a process that cannot ask is not talking to a tty it can
    * drive.
    */
  def isTty(fd: Int): Boolean =
    isattyH.exists(h => callInt(h, Integer.valueOf(fd)) == 1)

  /** Whether stdin and stdout are both terminals -- the gate for taking the screen. */
  def isInteractive: Boolean = isTty(StdIn) && isTty(StdOut)

  /** The terminal's size via `ioctl(TIOCGWINSZ)`, or None when it cannot be asked.
    *
    * `struct winsize` is four unsigned shorts, **row first**.
    */
  def windowSize: Option[(Int, Int)] =
    ioctlH.flatMap { h =>
      val arena = Arena.ofConfined()
      try {
        val ws = arena.allocate(8)
        val rc = callInt(h, Integer.valueOf(StdOut), java.lang.Long.valueOf(TIOCGWINSZ), ws)
        if (rc != 0) { None }
        else {
          val rows = ws.get(ValueLayout.JAVA_SHORT, 0).toInt
          val cols = ws.get(ValueLayout.JAVA_SHORT, 2).toInt
          if (rows > 0 && cols > 0) Some((rows, cols)) else None
        }
      } catch { case _: Throwable => None }
      finally { arena.close() }
    }

  /** The current terminal attributes as raw bytes, for restoring later. */
  def getAttributes: Option[Array[Byte]] =
    tcgetattrH.flatMap { h =>
      val arena = Arena.ofConfined()
      try {
        val buf = arena.allocate(TermiosSize)
        val rc = callInt(h, Integer.valueOf(StdIn), buf)
        if (rc != 0) { None }
        else { Some(buf.toArray(ValueLayout.JAVA_BYTE)) }
      } catch { case _: Throwable => None }
      finally { arena.close() }
    }

  /** Write `attrs` back with `TCSANOW`. True on success. */
  def setAttributes(attrs: Array[Byte]): Boolean =
    tcsetattrH.exists { h =>
      val arena = Arena.ofConfined()
      try {
        val buf = arena.allocate(TermiosSize)
        MemorySegment.copy(
          attrs,
          0,
          buf,
          ValueLayout.JAVA_BYTE,
          0,
          math.min(attrs.length, TermiosSize)
        )
        callInt(h, Integer.valueOf(StdIn), Integer.valueOf(TCSANOW), buf) == 0
      } catch { case _: Throwable => false }
      finally { arena.close() }
    }

  /** `attrs` with raw-mode flags applied; the original is not modified.
    *
    * Clears ISIG, so Ctrl-C arrives as the byte 0x03 rather than a signal -- which is
    * what makes the app able to bind it, and also why the app must handle SIGTERM to
    * avoid being unkillable except by `kill -9`.
    *
    * **OPOST is cleared**, unlike JLine, which leaves it on so its apps can keep writing
    * bare `\n`. grit.tui addresses every row absolutely (README rule 1), so it never relies
    * on ONLCR and can take genuinely raw output. Relying on it is precisely what painted
    * the layoutz spike's frames as a diagonal staircase.
    *
    * `VMIN=0, VTIME=1` gives a ~100ms read timeout from the driver. JLine avoids these
    * values because they make a read look like EOF when idle; here that empty read is
    * exactly the signal that resolves a held ESC, so the kernel provides the timeout and
    * no Java-side clock is needed.
    */
  def rawAttributes(attrs: Array[Byte]): Array[Byte] = {
    val out = java.util.Arrays.copyOf(attrs, attrs.length)
    def flag(offset: Int): Int =
      (out(offset) & 0xff) | ((out(offset + 1) & 0xff) << 8) |
        ((out(offset + 2) & 0xff) << 16) | ((out(offset + 3) & 0xff) << 24)
    def setFlag(offset: Int, v: Int): Unit = {
      out(offset) = (v & 0xff).toByte
      out(offset + 1) = ((v >> 8) & 0xff).toByte
      out(offset + 2) = ((v >> 16) & 0xff).toByte
      out(offset + 3) = ((v >> 24) & 0xff).toByte
    }
    setFlag(OffIflag, flag(OffIflag) & ~(IXON | ICRNL | INLCR))
    setFlag(OffOflag, flag(OffOflag) & ~OPOST)
    setFlag(OffLflag, flag(OffLflag) & ~(ICANON | ECHO | IEXTEN | ISIG))
    out(OffCc + VMin) = 0.toByte
    out(OffCc + VTime) = 1.toByte
    out
  }
}
