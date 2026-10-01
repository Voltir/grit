package grit.tui.model.surface

import utest.*

/** `Style.over` is the algebra everything composable rests on -- the selection mask, the
  * modal's backdrop, a block's ground under its spans. These are its laws, and the
  * property that makes it safe as a mask: it never clears anything.
  */
object StyleTests extends TestSuite {

  private val red = Color.hex("#ff0000")
  private val blue = Color.hex("#0000ff")

  /** A spread of styles wide enough that a law which holds on all pairs means it. */
  private val samples: Vector[Style] = Vector(
    Style.plain,
    Style.Bold,
    Style.Dim,
    Style.Italic,
    Style.Underline,
    Style.Reverse,
    Style.fg(red),
    Style.bg(blue),
    Style.fg(red) + Style.bg(blue) + Style.Bold,
    Style.fg(blue) + Style.Italic + Style.Underline
  )

  val tests = Tests {

    test("hex parses with and without the hash") {
      assert(Color.hex("#7aa2f7") == Color(0x7a, 0xa2, 0xf7))
      assert(Color.hex("7aa2f7") == Color(0x7a, 0xa2, 0xf7))
      assert(Color.hex("#000000") == Color(0, 0, 0))
      assert(Color.hex("#ffffff") == Color(255, 255, 255))
    }

    test("plain is the identity on both sides") {
      samples.foreach { s =>
        assert(s.over(Style.plain) == s)
        assert(Style.plain.over(s) == s)
      }
    }

    test("over is associative") {
      samples.foreach { a =>
        samples.foreach { b =>
          samples.foreach { c =>
            assert(a.over(b).over(c) == a.over(b.over(c)))
          }
        }
      }
    }

    test("the style on top wins the colours it names, and inherits the rest") {
      val under = Style.fg(red) + Style.bg(blue)
      assert(Style.fg(blue).over(under).fg.contains(blue)) // named: wins
      assert(Style.fg(blue).over(under).bg.contains(blue)) // unnamed: inherited
      assert(Style.Bold.over(under).fg.contains(red))
      assert(Style.Bold.over(under).bg.contains(blue))
    }

    test("over never clears an attribute or a colour") {
      // The property that makes it safe as a mask: the selection's reverse and the
      // modal's dim are laid over cells whose own styles they must not know about, so
      // layering may only ever add. Turning something off is what `copy` is for.
      samples.foreach { a =>
        samples.foreach { b =>
          val r = a.over(b)
          assert(r.bold == (a.bold || b.bold))
          assert(r.dim == (a.dim || b.dim))
          assert(r.italic == (a.italic || b.italic))
          assert(r.underline == (a.underline || b.underline))
          assert(r.reverse == (a.reverse || b.reverse))
          assert(r.fg.isDefined == (a.fg.isDefined || b.fg.isDefined))
          assert(r.bg.isDefined == (a.bg.isDefined || b.bg.isDefined))
        }
      }
    }
  }
}
