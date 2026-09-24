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

    test("arrivals are shown in order, with the thinking line last while a turn runs") {
      val asked = app
        .update(
          ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(true, "hi")), thinking = true),
          started
        )
        ._1
      transcript(asked) ==> Vector("you> hi", "grit is thinking…")
      val answered =
        app
          .update(
            ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(false, "hello")), thinking = false),
            asked
          )
          ._1
      transcript(answered) ==> Vector("you> hi", "grit> hello")
    }

    test("the reply that replaces the thinking line is painted") {
      // Painted, not read off the document: the wrap cache keys a block by position and
      // revision, so a reply given the thinking line's revision kept its old rows.
      val size = Size(20, 60)
      def painted(s: ChatScreen.State) = app.view(s)(size).surface.lines.mkString("\n")
      val asked =
        app.update(ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(true, "hi")), true), started)._1
      assert(painted(asked).contains("grit is thinking"))
      val answered =
        app.update(ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(false, "hello")), false), asked)._1
      val screen = painted(answered)
      assert(screen.contains("grit> hello"), !screen.contains("thinking"))
    }

    test("a submission is sent to the host, and shown only once the store has it") {
      val (sent, effect) =
        app.update(ChatScreen.Msg.Submit, started.copy(editor = started.editor.copy(text = "hi")))
      effect ==> Effect.ToHost(ChatScreen.Msg.Send("hi"))
      transcript(sent) ==> Vector()
      sent.editor.text ==> ""
    }

    test("an empty submission sends nothing") {
      app.update(ChatScreen.Msg.Submit, started)._2 ==> Effect.NoOp
    }

    test("a failure is shown before the thinking line, which stays") {
      val thinking = app.update(ChatScreen.Msg.Arrived(Vector(), thinking = true), started)._1
      transcript(app.update(ChatScreen.Msg.Failed("down"), thinking)._1) ==>
        Vector("! down", "grit is thinking…")
    }
  }
}
