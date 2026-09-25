package grit.app.look

import grit.tui.components.pane.Anchor
import grit.tui.components.tree.{Node, PaneKey, Scroller}
import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.{Size, Style, Surface}
import grit.tui.model.text.StyledText
import grit.tui.runtime.app.{App, Effect}
import grit.tui.runtime.loop.Headless

import utest.*

/** Replies rendered as prose, read off the painted screen: what a reader is shown, in
  * which colours, and how a reply looks at every length it passes through as it streams.
  */
object ProseLookTests extends TestSuite {

  enum Msg extends caps.Pure {
    case Read(m: Scroller.Msg)
  }

  /** `blocks` as the only thing on a screen, from the top, grounded as the chat is. */
  private final class Shown(blocks: Vector[Block], theme: Theme) extends App[Scroller.State, Msg] {
    def init: (Scroller.State, Effect[Msg]) =
      (Scroller.State(Anchor.At(DocPos.zero)), Effect.NoOp)
    def update(m: Msg, s: Scroller.State): (Scroller.State, Effect[Msg]) =
      m match {
        case Msg.Read(r) => (Scroller.update(r, s), Effect.NoOp)
      }
    def view(s: Scroller.State): Node[Msg] =
      Scroller
        .view(PaneKey.of("reply"), Doc(blocks), s)
        .map(Msg.Read(_))
        .grounded(Look(theme).ground)
  }

  private def painted(blocks: Vector[Block], size: Size, theme: Theme = Theme.Default): Surface =
    Headless.start(new Shown(blocks, theme), size).painted._1.surface

  /** The painted rows, right-trimmed. */
  private def rows(s: Surface): Vector[String] = s.lines.map(_.replaceAll("\\s+$", ""))

  /** Where `text` first appears on `s`: its row and column. */
  private def find(s: Surface, text: String): (Int, Int) = {
    val r = s.lines.indexWhere(_.contains(text))
    (r, s.lines.lift(r).fold(-1)(_.indexOf(text)))
  }

  val Sample: String =
    """Here is the **plan**, with *care* and `code`.
      |
      |# Steps
      |
      |1. Read the [docs](https://example.com/docs) first
      |2. Then **write
      |   it** out
      |   - nested *one*
      |   - nested two
      |
      |> A quote, with `inline` code.
      |
      |```scala
      |def twice(x: Int): Int = x + x // long enough to be cut at the pane's edge
      |```
      |
      |---
      |Done.""".stripMargin

  val tests = Tests {

    test("a reply's markdown is shown as prose: no markers, lists marked, code set apart") {
      val s = painted(Look(Theme.Default).assistant(Sample), Size(26, 60))
      rows(s).take(21) ==> Vector(
        "▌ᚨ Here is the plan, with care and code.",
        "",
        "Steps",
        "",
        "1. Read the docs (https://example.com/docs) first",
        "2. Then write",
        "   it out",
        "   ◦ nested one",
        "   ◦ nested two",
        "",
        "▎ A quote, with inline code.",
        "",
        " scala",
        " def twice(x: Int): Int = x + x // long enough to be cut at",
        "",
        "────────────────────────────────────────────────────────────",
        "",
        "Done.",
        "",
        "",
        ""
      )
    }

    test("strong is bold, emphasis is grit's colour, code is on the well; never italic or dim") {
      for (theme <- Theme.all) {
        val s = painted(Look(theme).assistant(Sample), Size(26, 60), theme)
        val (br, bc) = find(s, "plan")
        assert(s.at(br, bc).style.bold)
        val (er, ec) = find(s, "care")
        s.at(er, ec).style.fg ==> Some(theme.grit)
        assert(!s.at(er, ec).style.italic)
        val (cr, cc) = find(s, "code.")
        s.at(cr, cc).style.bg ==> Some(theme.well)
        s.at(cr, cc + 5).style.bg ==> Some(theme.ground)
        // A listing's ground runs the full width of each of its rows.
        val (lr, _) = find(s, "def twice")
        assert((0 until 60).forall(c => s.at(lr, c).style.bg.contains(theme.well)))
        assert(s.cells.forall(c => !c.style.italic && !c.style.dim))
      }
    }

    test("code is cut at the pane's edge, never wrapped; prose wraps") {
      val look = Look(Theme.Default)
      val narrow = rows(
        painted(
          look.assistant(
            "a paragraph that is long enough to wrap\n\n```\nlong code line that is cut\n```"
          ),
          Size(8, 20)
        )
      )
      narrow.take(5) ==> Vector(
        "▌ᚨ a paragraph that",
        "is long enough to",
        "wrap",
        "",
        " long code line that"
      )
    }

    test("a table's columns line up, the header over a rule") {
      val s = painted(
        Look(Theme.Default).assistant("| rune | who |\n|---|---|\n| ᚨ | grit |\n| ᛗ | the user |"),
        Size(8, 40)
      )
      rows(s).take(5) ==> Vector(
        "▌ᚨ",
        " rune │ who",
        " ─────┼─────────",
        " ᚨ    │ grit",
        " ᛗ    │ the user"
      )
    }

    test("an empty reply is grit's rune alone") {
      rows(painted(Look(Theme.Default).assistant(""), Size(3, 20))).take(1) ==> Vector("▌ᚨ")
    }

    test(
      "streaming: a prefix never shows a marker, and rows above the one being written never change"
    ) {
      val look = ProseLook(Look(Theme.Default))
      val caret = StyledText.styled("▍", Style.plain)
      val size = Size(30, 60)
      val whole = rows(painted(look.reply(Sample), size))
      val problems = (0 to Sample.length).flatMap { k =>
        val shown = rows(painted(look.streaming(Sample.take(k), caret), size))
        val last = shown.lastIndexWhere(_.nonEmpty)
        val settled = math.max(0, last - 1)
        val leaked = shown.exists(r => r.contains("**") || r.contains("`") || r.startsWith("#"))
        val moved = shown.take(settled) != whole.take(settled)
        Option.when(leaked || moved)(s"at $k:\n${shown.take(last + 1).mkString("\n")}")
      }
      problems ==> Vector.empty
    }

    test("streaming: the finished prefix renders as the reply does, but for the caret") {
      val look = ProseLook(Look(Theme.Default))
      val caret = StyledText.styled("▍", Style.plain)
      val done = rows(painted(look.streaming(Sample, caret), Size(30, 60)))
      val whole = rows(painted(look.reply(Sample), Size(30, 60)))
      done.map(_.stripSuffix("▍")) ==> whole
    }
  }
}
