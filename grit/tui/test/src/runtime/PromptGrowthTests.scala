package grit.tui.runtime

import utest.*

import grit.tui.runtime.std.{Ambient, Prompting, Std, StdBase}
import grit.tui.components.editor.Editor
import grit.tui.components.layout.{Region, Regions, Stack}
import grit.tui.components.pane.{Panes, TextPane}
import grit.tui.components.widget.StatusBar
import grit.tui.model.input.Input
import grit.tui.model.select.Doc
import grit.tui.model.input.Key
import grit.tui.model.surface.{Frame, PaneId, Placement, Placements, Size, Surface}
import grit.tui.model.text.WrapCache

/** A prompt that grows with its draft, end to end: the region, the editor's report, and
  * the re-layout that has to follow an edit rather than a resize.
  *
  * `LayoutTests` pins what `Region.Fit` resolves to and `ViewTests` pins what the editor
  * reports. What is left, and what only shows up once the two are wired to a running
  * app, is *when*: the prompt's height is now an output of typing, so a pane laid out
  * once per SIGWINCH is a pane wrapping at a height that stopped being true two
  * keystrokes ago. `Prompting.onPrompt` is where that is put right, and these tests are
  * what say so.
  */
object PromptGrowthTests extends TestSuite {

  private val Header = PaneId.of("header")
  private val Body = PaneId.of("body")
  private val Prompt = PaneId.of("prompt")

  /** The app's own submit: the draft cleared, exactly as `Demo2.submit` does it and by
    * the same route -- an `Own` message, not one of the layer's.
    */
  private enum Own { case Submit }

  final case class S(panes: Panes, editor: Editor, std: Std.State = Std.State())

  /** A 24x22 screen: one header row, a flexing body with a floor, a prompt that grows
    * to at most half, and no status bar to distract the arithmetic.
    */
  private val Screen = Size(24, 22)

  private val chrome = Stack.of(
    Header -> Region.Fixed(1),
    Body -> Region.Flex(4),
    Prompt -> Region.Fit(min = 3, upTo = 0.5)
  )

  private object App extends StdBase[S, Own] with Ambient[S, Own] with Prompting[S, Own] {
    val panes: S -> Panes = s => s.panes
    val withPanes: (S, Panes) -> S = (s, p) => s.copy(panes = p)
    val std: S -> Std.State = s => s.std
    val withStd: (S, Std.State) -> S = (s, c) => s.copy(std = c)
    protected val ownUpdate: (Own, S) -> (S, Effect[Std | Own]) =
      (_, s) => (s.copy(editor = s.editor.submitted), Effect.NoOp)

    override val prompt: S -> Option[(PaneId, Editor)] = s => Some((Prompt, s.editor))
    val withEditor: (S, Editor) -> S = (s, e) => s.copy(editor = e)

    /** The whole of what an app owes a growing prompt: lay out again, from the screen
      * the layer remembered for it.
      */
    override val onPrompt: S -> S = s => relaid(s, s.std.size)

    val onResize: (S, Size) -> S = (s, size) => relaid(s, size)

    val layers: Vector[Layer] = Vector(ambientLayer, promptLayer)
    val steps: Vector[Step] = Vector(ambientStep, editStep)

    def init: (S, Effect[Std | Own]) = (stateOf, Effect.NoOp)
    def view: S -> (Size -> Frame) = _ => size => Frame(Surface.blank(size))
  }

  /** The tree, resolved through the thing that holds the editor -- `chrome.resolve`
    * cannot answer for a `Fit` and refuses to try.
    */
  private def screen(s: S): Regions =
    chrome.views(StatusBar(Vector("head"), Vector.empty), s.panes.view(Body), s.editor)

  private def relaid(s: S, size: Size): S =
    s.copy(panes = s.panes.layoutIn(screen(s).placed(size)))

  private def stateOf: S = S(
    panes = Panes
      .of(
        TextPane(
          Body,
          Doc.of((0 until 40).toVector.map(i => s"row$i")*),
          cache = WrapCache.empty(22)
        )
      )
      .layout(Body, Size(20, 22)),
    editor = Editor("", 0)
  )

  private def placed(s: S) = screen(s).placed(Screen)

  /** Where the layout says everything is, as a placement map -- what the runtime hands
    * `onInput` off the frame it last painted.
    */
  private def placements(s: S): Placements =
    Placements(screen(s).placed(s.std.size).rects.toVector.map((id, r) => Placement(id, r)))

  /** Typed, one character at a time, the way the layer sees it: the router turns each
    * keystroke into an `EditTo` and the step chain applies it.
    *
    * The placements are rebuilt every keystroke rather than passed once, because the box the
    * editor is routed against is the box it was painted into -- and that box is the
    * thing this whole slice makes move.
    */
  private def typed(s: S, text: String): S =
    text.foldLeft(s) { (acc, c) =>
      App.onInput(Input.Keyboard(Key.Printable(c)), acc, placements(acc)) match {
        case Some(msg) => App.update(msg, acc)._1
        case None => acc
      }
    }

  /** The prompt is 20 columns of content inside its border, so 20 characters is one
    * wrapped row and 60 is three.
    */
  private val Started = App.update(Std.Resized(Screen), stateOf)._1

  val tests = Tests {

    test("an empty prompt is its minimum, and the body has everything else") {
      val p = placed(Started)
      assert(p(Prompt).rows == 3)
      assert(p(Body).rows == 20)
    }

    test("a draft that wraps takes its rows from the body, and gives them back") {
      // The behaviour asked for, stated as arithmetic: one row of box per wrapped row
      // of draft, and the region above it yields exactly that much.
      val two = typed(Started, "x" * 40)
      assert(placed(two)(Prompt).rows == 4)
      assert(placed(two)(Body).rows == 19)

      val five = typed(Started, "x" * 100)
      assert(placed(five)(Prompt).rows == 7)
      assert(placed(five)(Body).rows == 16)

      // and back down: growth that could not reverse is a prompt that only swells.
      val shrunk = App.update(Std.EditTo(Editor("hi", 2)), five)._1
      assert(placed(shrunk)(Prompt).rows == 3)
      assert(placed(shrunk)(Body).rows == 20)
    }

    test("a pane is laid out at the height the prompt left it, on the edit not the resize") {
      // The assertion that fails without `onPrompt` re-laying out. `Std.Resized` is the
      // only thing that used to lay panes out, and nothing resizes while you type -- so
      // a body pane that still thinks it is 20 rows tall wraps and scrolls against a
      // window that has not existed for three keystrokes.
      assert(Started.panes.rendered(Body).size.rows == 20)
      val grown = typed(Started, "x" * 100)
      assert(placed(grown)(Body).rows == 16)
      assert(grown.panes.rendered(Body).size.rows == 16)
      // and the pane follows it back up as the draft shrinks.
      val shrunk = App.update(Std.EditTo(Editor("", 0)), grown)._1
      assert(shrunk.panes.rendered(Body).size.rows == 20)
    }

    test("the prompt stops at half the screen however long the draft runs") {
      // Forty-five wrapped rows of draft, and the box stops at twelve. Which of the two
      // caps bit is `LayoutTests`' business; what matters here is that one of them did,
      // and that the pane below was laid out at what was left rather than at what it
      // would have liked.
      val essay = typed(Started, "x" * 900)
      val p = placed(essay)
      assert(p(Prompt).rows == 12)
      assert(p(Body).rows == 11)
      assert(essay.panes.rendered(Body).size.rows == 11)
    }

    test("an app message that shrinks the prompt re-lays out the pane it grew into") {
      // The bug this test exists for, reported from a real terminal: paste a long draft
      // and press Enter, and the prompt collapses correctly while the body pane keeps
      // the viewport it had while the prompt was tall -- so the rows the prompt gave
      // back paint as a band of blank cells until the next keystroke.
      //
      // Submitting is the app's message, not the layer's, so no amount of care in
      // `onPrompt` reaches it. `StdBase.relayout` runs after `ownUpdate` for exactly
      // this reason: an `Own` message is opaque, so the layer must assume it moved
      // something.
      val grown = typed(Started, "x" * 100)
      assert(placed(grown)(Prompt).rows == 7)
      assert(grown.panes.rendered(Body).size.rows == 16)

      val submitted = App.update(Own.Submit, grown)._1
      assert(submitted.editor.text == "")
      assert(placed(submitted)(Prompt).rows == 3) // the box collapsed
      assert(placed(submitted)(Body).rows == 20) // and the body reclaimed the rows
      // The assertion that was false: the pane must be laid out at what it reclaimed,
      // not at what it had while the prompt was tall.
      assert(submitted.panes.rendered(Body).size.rows == 20)
    }

    test("the screen the layer remembers is what an edit lays out against") {
      // `onPrompt` is handed only the state, so the size has to have been kept. It is
      // kept in the layer's own fragment rather than the app's, because it is the one
      // piece of input an app was told and then threw away.
      assert(Started.std.size == Screen)
      val resized = App.update(Std.Resized(Size(30, 22)), Started)._1
      assert(resized.std.size == Size(30, 22))
      val grown = typed(resized, "x" * 900)
      // Half of thirty, not half of the screen it was started at.
      assert(screen(grown).placed(Size(30, 22))(Prompt).rows == 15)
      assert(grown.panes.rendered(Body).size.rows == 14)
    }
  }
}
