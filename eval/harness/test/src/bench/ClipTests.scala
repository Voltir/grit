package grit.eval.harness.bench

import java.io.ByteArrayInputStream
import java.util.concurrent.atomic.AtomicBoolean

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
  }
}
