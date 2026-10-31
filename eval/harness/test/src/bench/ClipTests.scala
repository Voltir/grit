package grit.eval.harness.bench

import java.io.ByteArrayInputStream
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import utest.*

object ClipTests extends TestSuite {

  /** Every byte `clip` yields, as text, and whether it then says it was clipped. */
  private def drained(text: String, max: Long): (String, Boolean) = {
    val clipped = new AtomicBoolean(false)
    val clip = Bench.Clip(new ByteArrayInputStream(text.getBytes("UTF-8")), max, clipped)
    val out = new StringBuilder
    var b = clip.read()
    while (b >= 0) {
      out.append(b.toChar)
      b = clip.read()
    }
    (out.toString, clipped.get)
  }

  val tests = Tests {
    test(
      "a stream past its cut yields its first max bytes and says so; one at its cut is not clipped"
    ) {
      assert(
        drained("eleven byte", 10) == ("eleven byt", true),
        drained("ten bytes!", 10) == ("ten bytes!", false),
        drained("short", 10) == ("short", false)
      )
    }
    test("a bulk read takes the source in bulk, to its cut and one byte past it") {
      val max = 1L << 20
      val calls = new AtomicInteger(0)
      // The source, counting every read asked of it.
      val source = new ByteArrayInputStream("x".repeat(max.toInt + 10).getBytes("UTF-8")) {
        override def read(): Int = { calls.incrementAndGet(); super.read() }
        override def read(b: Array[Byte], off: Int, len: Int): Int = {
          calls.incrementAndGet()
          // Cast: ByteArrayInputStream writes into `b` for this call alone, the caller's buffer
          // handed through unchanged, and keeps no reference to it.
          super.read(b.asInstanceOf[Array[Byte]], off, len)
        }
      }
      val clipped = new AtomicBoolean(false)
      val all = Bench.Clip(source, max, clipped).readAllBytes()
      // readAllBytes asks for at most 16 KiB at first, then its buffer's room: 1 MiB is under
      // a hundred bulk reads, where a byte at a time is a million.
      (all.length, clipped.get, calls.get < 100) ==> (max.toInt, true, true)
    }
  }
}
