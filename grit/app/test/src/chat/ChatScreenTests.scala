package grit.app.chat

import grit.app.look.{Look, Theme}
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.Size
import grit.tui.runtime.app.Effect
import grit.tui.runtime.loop.Headless

import utest.*

/** The chat screen with no terminal and no engine, read off the painted screen: what a
  * submission asks the host for, and what the host's answers put on screen.
  */
object ChatScreenTests extends TestSuite {

  import ChatScreen.{Msg, Said}

  private val size = Size(20, 60)

  private def started: Headless[ChatScreen.State, Msg] =
    Headless.start(new ChatScreen.App("test-model", Look(Theme.Default)), size)

  /** Started, and told the engine is open. */
  private def ready: Headless[ChatScreen.State, Msg] = started.message(Msg.Opened)

  private def typed(h: Headless[ChatScreen.State, Msg], text: String) =
    h.inputs(text.map(c => Input.Keyboard(Key.Printable(c)))*)

  /** The transcript's painted rows that say something, in screen order: below the header
    * (row 0), and left of the scrollbar (the last column).
    */
  private def said(h: Headless[ChatScreen.State, Msg]): Vector[String] =
    h.screen
      .drop(1)
      .map(_.dropRight(1).trim)
      .filter(r =>
        r.startsWith("▌") || r.startsWith("ᚺ") || r.endsWith("thinking…") || r.endsWith("engine…")
      )

  val tests = Tests {
    test("every cell has a background from the theme, none left to the terminal") {
      for (theme <- Theme.all) {
        val h = Headless
          .start(new ChatScreen.App("test-model", Look(theme)), size)
          .message(
            Msg.Arrived(Vector(Said(true, "hi"), Said(false, "hello")), step = Some("call-model"))
          )
        val surface = h.painted._1.surface
        val unset = surface.cells.count(_.style.bg.isEmpty)
        assert(unset == 0)
        surface.at(size.rows / 2, size.cols / 2).style.bg ==> Some(theme.ground)
      }
    }

    test("the screen paints at once: it asks for the conversation and wards while opening") {
      started.effects ==> Vector(
        Effect.Batch(
          Vector(
            Effect.ToHost(Msg.Load),
            Effect.After(ChatScreen.Runes, ChatScreen.TickMs, Msg.Tick)
          )
        )
      )
      said(started) ==> Vector("ᛉᛟᛁᛞᚨ opening the engine…")
      assert(started.screen.last.trim.endsWith("opening"))
    }

    test("the ward turns with each tick, and stops once the engine is open") {
      said(started.message(Msg.Tick)) ==> Vector("ᛟᛞᛁᚨᛉ opening the engine…")
      ready.effects.last ==> Effect.Cancel(ChatScreen.Runes)
      said(ready) ==> Vector()
      // A tick already on its way when the timer stopped ends the chain.
      ready.message(Msg.Tick).effects.last ==> Effect.NoOp
      ready.message(Msg.Tick).state.tick ==> ready.state.tick
    }

    test("a turn in progress spins the Futhark, and the spinner stops with the turn") {
      val asked = ready.message(Msg.Arrived(Vector(Said(true, "hi")), step = Some("call-model")))
      asked.effects.last ==> Effect.After(ChatScreen.Runes, ChatScreen.TickMs, Msg.Tick)
      said(asked) ==> Vector("▌ᛗ hi", "ᚠ grit is thinking…")
      said(asked.message(Msg.Tick).message(Msg.Tick)) ==> Vector("▌ᛗ hi", "ᚦ grit is thinking…")
      val done = asked.message(Msg.Arrived(Vector(), step = None))
      done.effects.last ==> Effect.Cancel(ChatScreen.Runes)
      assert(done.screen.last.trim.endsWith("idle"))
    }

    test("the status line names the step and times it, from the step's first tick") {
      def status(h: Headless[ChatScreen.State, Msg]) = h.screen.last.trim
      val assembling = ready.message(Msg.Arrived(Vector(Said(true, "hi")), Some("assemble")))
      assert(status(assembling).endsWith("ᛟ assembling · 0.0s"))
      val later = (1 to 10).foldLeft(assembling)((h, _) => h.message(Msg.Tick))
      assert(status(later).endsWith("ᛟ assembling · 1.2s"))
      assert(status(later.message(Msg.Arrived(Vector(), Some("assemble")))).endsWith("1.2s"))
      val answering = later.message(Msg.Arrived(Vector(), Some("call-model")))
      assert(status(answering).endsWith("ᚨ answering · 0.0s"))
      assert(status(answering.message(Msg.Tick)).endsWith("ᚨ answering · 0.1s"))
    }

    test("an engine that will not open ends the ward and says so") {
      said(started.message(Msg.Failed("could not open the engine: refused"))) ==>
        Vector("ᚺ could not open the engine: refused")
    }

    test("arrivals are painted in order, with the thinking line last while a turn runs") {
      val asked = ready.message(Msg.Arrived(Vector(Said(true, "hi")), step = Some("call-model")))
      said(asked) ==> Vector("▌ᛗ hi", "ᚠ grit is thinking…")
      val answered = asked.message(Msg.Arrived(Vector(Said(false, "hello")), step = None))
      said(answered) ==> Vector("▌ᛗ hi", "▌ᚨ hello")
    }

    test("a typed submission is sent to the host, and painted only once the store has it") {
      val sent = typed(ready, "hi").input(Input.Keyboard(Key.Enter))
      sent.effects.last ==> Effect.ToHost(Msg.Send("hi"))
      said(sent) ==> Vector()
      sent.state.editor.text ==> ""
    }

    test("an empty submission sends nothing") {
      ready.input(Input.Keyboard(Key.Enter)).effects.last ==> Effect.NoOp
    }

    test("a failure is painted before the thinking line, which stays") {
      val failed =
        ready.message(Msg.Arrived(Vector(), step = Some("call-model"))).message(Msg.Failed("down"))
      said(failed) ==> Vector("ᚺ down", "ᚠ grit is thinking…")
    }
  }
}
