package grit.tui.components.overlay

import grit.tui.components.View
import grit.tui.model.input.Input
import grit.tui.model.surface.{Frame, PaneId, Pos, Rect, Size, Surface}
import utest.*

/** The compositor's combinators: overlays over a painted frame, each reading the
  * frame's own placement map.
  *
  * The oracle is the composition they replaced: whatever a combinator paints must be
  * what the hand-written blit sequence painted, cell for cell -- the combinators are a
  * vocabulary for the demo's overlay dance, not a new way to draw it.
  */
object ComposeTests extends TestSuite {

  private val Base = PaneId.of("base")
  private val BodyPane = PaneId.of("modal-body")
  private val PromptPane = PaneId.of("prompt")
  private val PopupPane = PaneId.of("popup")

  private val screen = Size(20, 60)
  private val modal = Modal("help", rows = 5, cols = 10)

  /** The base: one named pane painted at the origin. */
  private def base: Frame = Frame(
    Surface.blank(screen).blit(Surface.blank(Size(6, 12)).write(0, 0, "hello"), Pos(0, 0), Base)
  )

  /** A body view: `text` in a frame-less block, sized to what it is given. */
  private def bodyView(text: String): View = new View {
    type Route = Input
    def measure(avail: Size): Size = avail
    def render(size: Size): Surface = Surface.blank(size).write(0, 0, text)
    def route(input: Input, at: Rect): Input = input
  }

  /** The modal's content rect at this screen: centred, from `place`. */
  private val rect = modal.place(screen).get

  /** What the hand-written overlay dance paints -- the oracle. */
  private def dimmedByHand(f: Frame, body: View): Frame = {
    val dimmed = modal.render(f.surface, rect)
    Frame(dimmed.blit(body.render(rect.size), Pos(rect.top, rect.left), BodyPane))
  }

  val tests = Tests {

    test("dimmedBy with the modal closed is exactly the identity") {
      val f = base
      val next = f.dimmedBy(modal, bodyView("body"), BodyPane, screen, when = false)
      assert(next == f)
    }

    test("dimmedBy paints what the hand-written overlay dance paints") {
      val f = base
      val next = f.dimmedBy(modal, bodyView("body"), BodyPane, screen, when = true)
      assert(next == dimmedByHand(f, bodyView("body")))
      // The body is where the routing rule says it is: found by the next onInput.
      assert(next.placements(BodyPane).contains(rect))
      // The app beneath is dimmed, not cleared: the base glyph survives with dim added.
      assert(f.surface.at(0, 0).ch == next.surface.at(0, 0).ch)
      assert(next.surface.at(0, 0).style.dim)
    }

    test("a screen too small for a modal paints nothing and records nothing") {
      val f = base
      val next =
        f.dimmedBy(Modal("help", rows = 5, cols = 10), bodyView("b"), BodyPane, Size(6, 12))
      assert(next == f)
      assert(next.placements(BodyPane).isEmpty)
    }

    test("floating anchors the popup to where the anchor painted") {
      // A prompt painted at a known rect, above which the list must appear.
      val withPrompt = Frame(
        base.surface.blit(Surface.blank(Size(3, 12)), Pos(12, 20), PromptPane)
      )
      val promptRect = Rect(12, 20, 3, 12)
      val popup = Popup.of("/alpha", "/beta")
      val next =
        withPrompt.floating(Some((popup, PopupPane)), anchor = PromptPane, screen = screen)
      // boxSize: 2 matches -> 2 rows + 2 frame; widest match 6 + 2. Above the prompt.
      val expected = popup.place(promptRect, screen).get
      assert(expected.top < promptRect.top)
      assert(next.placements(PopupPane).contains(expected))
      // The frame and its own list rows are drawn inside that rect.
      assert(next.surface.at(expected.top, expected.left).ch == '╭')
      assert(next.surface.at(expected.top + 1, expected.left + 1).ch == '/')
    }

    test("a popup with nothing matched is absent, not small") {
      val f = Frame(base.surface.blit(Surface.blank(Size(3, 12)), Pos(12, 20), PromptPane))
      val popup = Popup.of("/alpha").withQuery("zzz") // filters everything out
      val next = f.floating(Some((popup, PopupPane)), anchor = PromptPane, screen = screen)
      assert(next == f)
      assert(next.placements(PopupPane).isEmpty)
    }

    test("no anchor rect means nothing to float over") {
      val f = base // no prompt painted
      val popup = Popup.of("/alpha")
      assert(f.floating(Some((popup, PopupPane)), anchor = PromptPane, screen = screen) == f)
      assert(f.floating(None, anchor = PromptPane, screen = screen) == f)
    }

    test("caretIn places the cursor from the pane's painted rect") {
      val f = Frame(base.surface.blit(Surface.blank(Size(3, 12)), Pos(12, 20), PromptPane))
      val next = f.caretIn(PromptPane, _ => Some(Pos(1, 5)))
      assert(next.cursor == Some(Pos(13, 25)))
    }

    test("a frozen app shows no caret, and an unpainted pane offers no rect") {
      val f = Frame(base.surface.blit(Surface.blank(Size(3, 12)), Pos(12, 20), PromptPane))
      assert(f.caretIn(PromptPane, _ => Some(Pos(1, 5)), when = false) == f.copy(cursor = None))
      val bare = base
      assert(bare.caretIn(PromptPane, _ => Some(Pos(0, 0))).cursor.isEmpty)
    }

    test("the layers compose in the order the demo reads: base, modal, popup, caret") {
      val f = base
      val popup = Popup.of("/alpha", "/beta")
      val next = f
        .dimmedBy(modal, bodyView("body"), BodyPane, screen, when = true)
        .floating(Some((popup, PopupPane)), anchor = PromptPane, screen = screen)
        .withCaret(Some(Pos(0, 1)))
      // The modal painted, the popup did not (no anchor), the cursor is set.
      assert(next.placements(BodyPane).contains(rect))
      assert(next.placements(PopupPane).isEmpty)
      assert(next.cursor == Some(Pos(0, 1)))
    }
  }
}
