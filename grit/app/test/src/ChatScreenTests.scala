package grit.app

import grit.tui.model.surface.Size
import grit.tui.runtime.Effect
import grit.tui.runtime.std.Std
import utest.*

/** The chat screen's transitions, with no terminal and no engine: what a submission asks
  * the host for, and what the host's answers do to the transcript.
  */
object ChatScreenTests extends TestSuite {

  private val app = new ChatScreen.App("test-model")

  private def started: ChatScreen.State =
    app.update(Std.Resized(Size(20, 60)), app.init._1)._1

  private def transcript(state: ChatScreen.State): Vector[String] =
    state.panes
      .get(grit.tui.model.surface.PaneId.of("transcript"))
      .map(_.doc.blocks.map(_.text).filter(_.nonEmpty))
      .getOrElse(Vector.empty)

  val tests = Tests {
    test("the screen asks the host for the conversation at start") {
      app.init._2 ==> Effect.ToHost(ChatScreen.Msg.Load)
    }

    test("the conversation so far is shown in order") {
      val loaded = app
        .update(
          ChatScreen.Msg
            .Loaded(Vector(ChatScreen.Said(true, "hi"), ChatScreen.Said(false, "hello"))),
          started
        )
        ._1
      transcript(loaded) ==> Vector("you> hi", "grit> hello")
    }

    test("a submission is shown, sent to the host, and waited on") {
      val (sent, effect) =
        app.update(ChatScreen.Msg.Submit, started.copy(editor = started.editor.copy(text = "hi")))
      effect ==> Effect.ToHost(ChatScreen.Msg.Send("hi"))
      transcript(sent) ==> Vector("you> hi")
      sent.waiting ==> 1
      sent.editor.text ==> ""
    }

    test("an empty submission sends nothing") {
      app.update(ChatScreen.Msg.Submit, started)._2 ==> Effect.NoOp
    }

    test("a reply is shown and ends the wait; a failure is shown too") {
      val waiting = started.copy(waiting = 1)
      val replied = app.update(ChatScreen.Msg.Replied("hello"), waiting)._1
      (transcript(replied), replied.waiting) ==> (Vector("grit> hello"), 0)
      val failed = app.update(ChatScreen.Msg.Failed("down"), waiting)._1
      (transcript(failed), failed.waiting) ==> (Vector("! down"), 0)
    }
  }
}
