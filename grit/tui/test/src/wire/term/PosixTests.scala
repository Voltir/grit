package grit.tui.wire.term

import utest.*

/** The pure half of the seam: the flag arithmetic that turns saved terminal attributes
  * into raw ones. A wrong bit here corrupts the user's terminal rather than returning an
  * error, and it is the one part that can be checked without a device.
  *
  * Layout asserted against Linux x86-64: four 4-byte flag words, `c_line`, then NCCS=32
  * control characters from offset 17. `VTIME` precedes `VMIN` -- the reverse of macOS.
  */
object PosixTests extends TestSuite {

  private val OffIflag = 0
  private val OffOflag = 4
  private val OffCflag = 8
  private val OffLflag = 12
  private val OffCc = 17
  private val TermiosSize = 60

  private def flag(a: Array[Byte], off: Int): Int =
    (a(off) & 0xff) | ((a(off + 1) & 0xff) << 8) | ((a(off + 2) & 0xff) << 16) |
      ((a(off + 3) & 0xff) << 24)

  private def withFlags(iflag: Int, oflag: Int, lflag: Int): Array[Byte] = {
    val a = new Array[Byte](TermiosSize)
    def put(off: Int, v: Int): Unit = {
      a(off) = (v & 0xff).toByte
      a(off + 1) = ((v >> 8) & 0xff).toByte
      a(off + 2) = ((v >> 16) & 0xff).toByte
      a(off + 3) = ((v >> 24) & 0xff).toByte
    }
    put(OffIflag, iflag)
    put(OffOflag, oflag)
    put(OffLflag, lflag)
    a
  }

  val tests = Tests {

    test("raw mode clears exactly the canonical, echo, extended and signal bits") {
      // ISIG is the load-bearing one: without clearing it Ctrl-C is a signal, not the
      // byte 0x03, and an app can never bind it.
      // Every bit set going in, so each one kept or cleared is seen.
      val raw = Posix.rawAttributes(withFlags(0, 0, -1))
      val cleared = 0x0002 | 0x0008 | 0x0001 | 0x8000 // ICANON, ECHO, ISIG, IEXTEN
      assert(flag(raw, OffLflag) == ~cleared)
    }

    test("raw mode clears flow control and both CR/LF translations on input") {
      val raw = Posix.rawAttributes(withFlags(0xffff, 0, 0))
      val iflag = flag(raw, OffIflag)
      assert((iflag & 0x0400) == 0) // IXON
      assert((iflag & 0x0100) == 0) // ICRNL
      assert((iflag & 0x0040) == 0) // INLCR
      assert((iflag & 0x0002) != 0) // BRKINT untouched
    }

    test("OPOST is cleared, so no output post-processing is relied upon") {
      // The divergence from JLine, and the reason it is safe here: grit.tui addresses every
      // row absolutely, so it never needs ONLCR to turn its newlines into carriage
      // returns. Relying on it is what painted the layoutz spike as a staircase.
      val raw = Posix.rawAttributes(withFlags(0, 0xffff, 0))
      assert((flag(raw, OffOflag) & 0x0001) == 0)
    }

    test("VMIN=0 and VTIME=1 give the read its timeout from the driver") {
      // JLine avoids these values because an idle read then looks like EOF. Here that
      // empty read is exactly what resolves a held ESC, so the kernel is the clock.
      val raw = Posix.rawAttributes(new Array[Byte](TermiosSize))
      assert(raw(OffCc + 6) == 0) // VMIN
      assert(raw(OffCc + 5) == 1) // VTIME
    }

    test("the saved attributes are not modified, so restore has something to restore") {
      val attrs = withFlags(0xffff, 0xffff, 0xffff)
      val before = attrs.toVector
      val _ = Posix.rawAttributes(attrs)
      assert(attrs.toVector == before)
    }

    test("everything outside the touched fields survives, including c_cflag and speeds") {
      val attrs = new Array[Byte](TermiosSize)
      var i = 0
      while (i < TermiosSize) { attrs(i) = (i + 1).toByte; i += 1 }
      val raw = Posix.rawAttributes(attrs)
      assert(raw.length == TermiosSize)
      assert(flag(raw, OffCflag) == flag(attrs, OffCflag))
      assert(raw(16) == attrs(16)) // c_line
      assert(raw.slice(52, 60).toVector == attrs.slice(52, 60).toVector) // c_ispeed/c_ospeed
    }

    test("asking whether this is a terminal is total, however the process was started") {
      // The test JVM has no controlling terminal, so this is the negative case; the
      // positive one is the scripted pty run. Neither call may throw.
      val _ = Posix.isTty(0)
      val _ = Posix.windowSize
      assert(!Posix.isInteractive || Posix.windowSize.isDefined)
    }
  }
}
