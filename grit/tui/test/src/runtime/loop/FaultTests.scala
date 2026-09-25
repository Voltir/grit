package grit.tui.runtime.loop

import grit.tui.components.tree.Node
import grit.tui.components.tree.Node.*
import grit.tui.components.view.View
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.{Size, Surface}
import grit.tui.runtime.app.{App, Effect, Fault}

import utest.*

/** An app that throws, stepped headlessly: the loop keeps the last frame that painted,
  * says what failed on the bottom row, and goes on handling input -- so a quit key still
  * quits. The failure that motivated it: a `NoClassDefFoundError` during rendering
  * stopped repainting with nothing on screen to say so.
  */
object FaultTests extends TestSuite {

  enum Msg extends caps.Pure {
    case Up
    case Boom
    case Quit
  }

  private final case class Says(text: String) extends View {
    def measure(avail: Size): Size = avail
    def render(size: Size): Surface = Surface.blank(size).write(0, 0, text)
  }

  /** A counter. Its view throws at 2; `Boom` throws in update; `h` throws in a handler. */
  object Counter extends App[Int, Msg] {
    def init: (Int, Effect[Msg]) = (0, Effect.NoOp)
    def update(m: Msg, n: Int): (Int, Effect[Msg]) = m match {
      case Msg.Up => (n + 1, Effect.NoOp)
      case Msg.Boom => throw new IllegalStateException("no")
      case Msg.Quit => (n, Effect.Quit)
    }
    def view(n: Int): Node[Msg] =
      if (n == 2) throw new NoClassDefFoundError("grit/Missing")
      else
        paint(Says(s"count $n")).onKey {
          case Input.Keyboard(Key.Printable('u')) => Some(Msg.Up)
          case Input.Keyboard(Key.Printable('b')) => Some(Msg.Boom)
          case Input.Keyboard(Key.Printable('h')) => throw new IllegalArgumentException("h")
          case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
          case _ => None
        }
  }

  private def key(c: Char): Input = Input.Keyboard(Key.Printable(c))

  val tests = Tests {

    test("a view that throws leaves the last good frame up, and says so on the bottom row") {
      val h = Headless.start(Counter, Size(4, 80)).input(key('u')).repainted
      h.screen(0).trim ==> "count 1"
      val broke = h.input(key('u'))
      val screen = broke.screen
      screen(0).trim ==> "count 1"
      assert(screen(3).contains("view failed"), screen(3).contains("NoClassDefFoundError"))
      broke.repainted.loop.fault.map(_.stage) ==> Some(Fault.Stage.View)
      broke.state ==> 2
    }

    test("input goes on past a view that throws: ctrl-q still quits, a key moves on") {
      val broke = Headless.start(Counter, Size(4, 80)).inputs(key('u'), key('u')).repainted
      val quit = broke.input(Input.Keyboard(Key.Ctrl('q')))
      assert(quit.effects.contains(Effect.Quit))
      val on = broke.input(key('u')).repainted
      on.screen(0).trim ==> "count 3"
      // The key that moved on also dismissed the fault it had been shown.
      assert(!on.screen(3).contains("failed"), on.loop.fault.isEmpty)
    }

    test("an update that throws drops its message; the state is as it was") {
      val h = Headless.start(Counter, Size(4, 80)).input(key('u')).input(key('b'))
      h.state ==> 1
      h.loop.fault.map(_.stage) ==> Some(Fault.Stage.Update)
      assert(h.screen(3).contains("update failed"), h.screen(0).trim == "count 1")
    }

    test("a handler that throws drops its input") {
      val h = Headless.start(Counter, Size(4, 80)).input(key('h'))
      h.state ==> 0
      h.loop.fault.map(_.stage) ==> Some(Fault.Stage.Handler)
      assert(h.loop.faults == 1L)
    }

    test("what the loop cannot carry on past still escapes it") {
      assert(!Fault.survivable(new InterruptedException()))
      assert(!Fault.survivable(new OutOfMemoryError()))
      assert(Fault.survivable(new NoClassDefFoundError("x")))
    }
  }
}
