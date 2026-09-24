package grit.app

import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.Size
import grit.tui.runtime.Headless
import grit.tui.runtime.Effect
import utest.*

/** The chat screen with no terminal and no engine, read off the painted screen: what a
  * submission asks the host for, and what the host's answers put on screen.
  */
object ChatScreenTests extends TestSuite {

  import ChatScreen.{Msg, Said}

  private val size = Size(20, 60)

  private def started: Headless[ChatScreen.State, Msg] =
    Headless.start(new ChatScreen.App("test-model"), size)

  private def typed(h: Headless[ChatScreen.State, Msg], text: String) =
    h.inputs(text.map(c => Input.Keyboard(Key.Printable(c)))*)

  /** The transcript's painted rows that say something, in screen order: below the header
    * (row 0), and left of the scrollbar (the last column).
    */
  private def said(h: Headless[ChatScreen.State, Msg]): Vector[String] =
    h.screen
      .drop(1)
      .map(_.dropRight(1).trim)
      .filter(r => r.startsWith("you>") || r.startsWith("grit") || r.startsWith("!"))

  val tests = Tests {
    test("the screen asks the host for the conversation at start") {
      started.effects ==> Vector(Effect.ToHost(Msg.Load))
    }

    test("arrivals are painted in order, with the thinking line last while a turn runs") {
      val asked = started.message(Msg.Arrived(Vector(Said(true, "hi")), thinking = true))
      said(asked) ==> Vector("you> hi", "grit is thinking…")
      val answered = asked.message(Msg.Arrived(Vector(Said(false, "hello")), thinking = false))
      said(answered) ==> Vector("you> hi", "grit> hello")
    }

    test("a typed submission is sent to the host, and painted only once the store has it") {
      val sent = typed(started, "hi").input(Input.Keyboard(Key.Enter))
      sent.effects.last ==> Effect.ToHost(Msg.Send("hi"))
      said(sent) ==> Vector()
      sent.state.editor.text ==> ""
    }

    test("an empty submission sends nothing") {
      started.input(Input.Keyboard(Key.Enter)).effects.last ==> Effect.NoOp
    }

    test("a failure is painted before the thinking line, which stays") {
      val failed =
        started.message(Msg.Arrived(Vector(), thinking = true)).message(Msg.Failed("down"))
      said(failed) ==> Vector("! down", "grit is thinking…")
    }
  }
}
