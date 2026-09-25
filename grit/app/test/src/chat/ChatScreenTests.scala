package grit.app.chat

import grit.app.look.Theme
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.surface.{Pos, Size}
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
    Headless.start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), size)

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

  /** A click (press and release in place) on the first painted row that says `text`. */
  private def click(h: Headless[ChatScreen.State, Msg], text: String) = {
    val row = h.screen.indexWhere(_.contains(text))
    val col = h.screen.lift(row).map(_.indexOf(text)).getOrElse(0)
    def at(kind: MouseKind, button: Button) =
      Input.Mouse(MouseEvent(kind, button, Pos(row, col), Mods.none))
    h.inputs(at(MouseKind.Press, Button.Left), at(MouseKind.Release, Button.None))
  }

  /** Whether the last step asked the host to send anything to the model. */
  private def sends(h: Headless[ChatScreen.State, Msg]): Boolean = {
    def one(e: Effect[Msg]): Boolean = e match {
      case Effect.ToHost(Msg.Send(_)) => true
      case Effect.Batch(es) => es.exists(one)
      case _ => false
    }
    h.effects.exists(one)
  }

  val tests = Tests {
    test("every cell has a background from the theme, none left to the terminal") {
      for (theme <- Theme.all) {
        val h = Headless
          .start(new ChatScreen.App("test-model", theme, Tokens(16000)), size)
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
      assert(started.screen.last.contains("opening"))
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
      assert(done.screen.last.contains("ᛁ idle"))
    }

    test("the status line names the step and times it, from the step's first tick") {
      def status(h: Headless[ChatScreen.State, Msg]) = h.screen.last.trim
      val assembling = ready.message(Msg.Arrived(Vector(Said(true, "hi")), Some("assemble")))
      assert(status(assembling).contains("ᛟ assembling · 0.0s"))
      val later = (1 to 10).foldLeft(assembling)((h, _) => h.message(Msg.Tick))
      assert(status(later).contains("ᛟ assembling · 1.2s"))
      assert(
        status(later.message(Msg.Arrived(Vector(), Some("assemble")))).contains("assembling · 1.2s")
      )
      val answering = later.message(Msg.Arrived(Vector(), Some("call-model")))
      assert(status(answering).contains("ᚨ answering · 0.0s"))
      assert(status(answering.message(Msg.Tick)).contains("ᚨ answering · 0.1s"))
    }

    test("from 100 columns the turn panel stands beside the transcript; ctrl-b hides it") {
      val view = TurnView(
        TurnSeq(2),
        "what did we decide?",
        None,
        grit.turn.Turn.Step.all.map(TurnView.Step(_, Some(400))),
        Some("ward engine"),
        Some(
          TurnView.Window(Tokens(12), Tokens(3200), Tokens(1700), Tokens(40), Vector(TurnSeq(0)))
        ),
        Some(BigDecimal("0.00031")),
        Some(Tokens(5000))
      )
      def at(cols: Int) =
        Headless
          .start(
            new ChatScreen.App("test-model", Theme.Default, Tokens(16000)),
            Size(30, cols)
          )
          .message(Msg.Opened)
          .message(Msg.Turn(view))
      val shown = at(110).screen.mkString("\n")
      assert(
        shown.contains("TURN 3  · done"),
        shown.contains("ᛟ assemble"),
        shown.contains("query    ward engine"),
        shown.contains("recalled turn 1"),
        shown.contains("4.9k of 16k budget"),
        shown.contains("billed   5k in · $0.00031")
      )
      assert(at(110).screen.exists(r => r.contains("████") && r.contains("░")))
      assert(!at(99).screen.mkString.contains("TURN 3"))
      assert(!at(110).inputs(Input.Keyboard(Key.Ctrl('b'))).screen.mkString.contains("TURN 3"))
    }

    test("a click on an earlier message pins its turn to the panel; esc lets it go") {
      val two = ready.message(
        Msg.Arrived(
          Vector(
            Said(true, "one", TurnSeq(0)),
            Said(false, "first reply", TurnSeq(0)),
            Said(true, "two", TurnSeq(1)),
            Said(false, "second reply", TurnSeq(1))
          ),
          None
        )
      )
      val pinned = click(two, "first reply")
      pinned.effects.last ==> Effect.ToHost(Msg.Show(Some(TurnSeq(0))))
      pinned.state.pinned ==> Some(TurnSeq(0))
      // The latest turn is what the panel follows anyway: nothing to pin.
      click(two, "second reply").state.pinned ==> None
      val let = pinned.input(Input.Keyboard(Key.Escape))
      let.state.pinned ==> None
      let.effects.last ==> Effect.ToHost(Msg.Show(None))
      // A new message lets go too, and the draft is sent.
      val sent = typed(pinned, "three").input(Input.Keyboard(Key.Enter))
      sent.state.pinned ==> None
      sent.effects.last ==>
        Effect.Batch(Vector(Effect.ToHost(Msg.Send("three")), Effect.ToHost(Msg.Show(None))))
    }

    test("a click on the thinking line opens the running turn; esc closes it") {
      val view = TurnView(
        TurnSeq(0),
        "hi",
        Some("call-model"),
        Vector(TurnView.Step("assemble", Some(400))),
        None,
        None,
        None,
        None
      )
      val asked = ready
        .message(Msg.Arrived(Vector(Said(true, "hi", TurnSeq(0))), Some("call-model")))
        .message(Msg.Turn(view))
      val open = click(asked, "grit is thinking")
      open.state.modal ==> Some(ChatScreen.Dialog.Turn)
      val shown = open.screen.mkString("\n")
      assert(shown.contains("turn 1"), shown.contains("TURN 1  · ᚨ answering"))
      // The dialog takes the keys: typing does not reach the prompt beneath it.
      typed(open, "x").state.editor.text ==> ""
      open.input(Input.Keyboard(Key.Escape)).state.modal ==> None
    }

    test("a streaming reply takes the thinking line's place; the recorded one takes its") {
      val asked =
        ready.message(Msg.Arrived(Vector(Said(true, "hi", TurnSeq(0))), Some("call-model")))
      val streaming = asked.message(Msg.Heard(ChatScreen.Hearing(TurnSeq(0), "hmm", "Fehu is")))
      said(streaming).map(_.stripSuffix("▍").trim) ==> Vector("▌ᛗ hi", "▌ᚨ Fehu is")
      // What was heard of another turn is not this one's reply.
      said(asked.message(Msg.Heard(ChatScreen.Hearing(TurnSeq(7), "", "other")))) ==>
        Vector("▌ᛗ hi", "ᚠ grit is thinking…")
      val recorded = streaming.message(
        Msg.Arrived(Vector(Said(false, "Fehu is wealth.", TurnSeq(0))), None)
      )
      said(recorded) ==> Vector("▌ᛗ hi", "▌ᚨ Fehu is wealth.")
      recorded.state.hearing ==> None
    }

    test("the opened turn shows what has been heard: the reasoning, then the text") {
      val view =
        TurnView(TurnSeq(0), "hi", Some("call-model"), Vector.empty, None, None, None, None)
      val open = click(
        ready
          .message(Msg.Arrived(Vector(Said(true, "hi", TurnSeq(0))), Some("call-model")))
          .message(Msg.Turn(view)),
        "grit is thinking"
      ).message(Msg.Heard(ChatScreen.Hearing(TurnSeq(0), "the user wants a rune", "Fehu")))
      val shown = open.screen.mkString("\n")
      assert(
        shown.contains("heard so far"),
        shown.contains("the user wants a rune"),
        shown.contains("Fehu▍")
      )
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

    test("/ in an empty prompt opens the command list, and the draft filters it") {
      val listed = typed(ready, "/")
      listed.state.palette ==> Some(0)
      val shown = listed.screen.mkString("\n")
      assert(
        shown.contains("/theme  switch the colour theme"),
        shown.contains("/panel"),
        shown.contains("/help"),
        shown.contains("/quit")
      )
      // The list, too, is painted in the theme's colours, every cell of it.
      assert(listed.painted._1.surface.cells.count(_.style.bg.isEmpty) == 0)
      val narrowed = typed(listed, "th").screen.mkString("\n")
      assert(narrowed.contains("/theme  switch"), !narrowed.contains("/panel"))
      // A slash typed after other text is text, not a command.
      typed(ready, "a/").state.palette ==> None
    }

    test("the arrows move the selection, and Enter or Tab chooses and runs it") {
      val panel = typed(ready, "/").input(Input.Keyboard(Key.Down(Mods.none)))
      panel.state.palette ==> Some(1)
      val toggled = panel.input(Input.Keyboard(Key.Enter))
      toggled.state.panel ==> false
      toggled.state.palette ==> None
      toggled.state.editor.text ==> ""
      assert(!sends(toggled))
      typed(ready, "/h").input(Input.Keyboard(Key.Tab)).state.modal ==>
        Some(ChatScreen.Dialog.Help)
    }

    test("Escape dismisses the list and keeps the draft; ctrl-p brings it back") {
      val listed = typed(ready, "/pa")
      val dismissed = listed.input(Input.Keyboard(Key.Escape))
      dismissed.state.palette ==> None
      dismissed.state.editor.text ==> "/pa"
      assert(!dismissed.screen.mkString.contains("show or hide"))
      dismissed.input(Input.Keyboard(Key.Ctrl('p'))).state.palette ==> Some(0)
    }

    test("ctrl-p over a draft puts it aside, and Escape gives it back") {
      val opened = typed(ready, "half a thought").input(Input.Keyboard(Key.Ctrl('p')))
      opened.state.editor.text ==> "/"
      assert(opened.screen.mkString.contains("/theme  switch"))
      opened.input(Input.Keyboard(Key.Escape)).state.editor.text ==> "half a thought"
      val ran = typed(opened, "panel").input(Input.Keyboard(Key.Enter))
      ran.state.panel ==> false
      ran.state.editor.text ==> "half a thought"
    }

    test("/theme repaints every colour, whether typed out or chosen from the lists") {
      def ground(h: Headless[ChatScreen.State, Msg]) =
        h.painted._1.surface.at(size.rows / 2, size.cols / 2).style.bg
      val talked = ready.message(Msg.Arrived(Vector(Said(true, "hi"), Said(false, "hello")), None))
      ground(talked) ==> Some(Theme.Default.ground)
      val typedOut = typed(talked, "/theme tokyo-night").input(Input.Keyboard(Key.Enter))
      typedOut.state.theme ==> Theme.TokyoNight
      ground(typedOut) ==> Some(Theme.TokyoNight.ground)
      said(typedOut) ==> said(talked)
      assert(!sends(typedOut))
      // Chosen: /theme from the commands, then a theme from the second list.
      val themes = typed(talked, "/th").input(Input.Keyboard(Key.Enter))
      themes.state.editor.text ==> "/theme "
      val listed = themes.screen.mkString("\n")
      assert(listed.contains("tokyo-storm"), listed.contains("nightshade"))
      val chosen = typed(themes, "ab").input(Input.Keyboard(Key.Tab))
      chosen.state.theme ==> Theme.Abyss
      chosen.state.palette ==> None
      ground(chosen) ==> Some(Theme.Abyss.ground)
      assert(!chosen.screen.mkString.contains("nightshade"))
    }

    test("a / draft never reaches the host: one that will not run says why") {
      val unknown = typed(ready, "/foo").input(Input.Keyboard(Key.Enter))
      assert(!sends(unknown), unknown.screen.last.contains("no command /foo"))
      unknown.state.editor.text ==> ""
      val noTheme = typed(ready, "/theme sandstone").input(Input.Keyboard(Key.Enter))
      assert(!sends(noTheme), noTheme.screen.last.contains("no theme sandstone"))
      val dismissed = typed(ready, "/pa").input(Input.Keyboard(Key.Escape))
      val half = dismissed.input(Input.Keyboard(Key.Enter))
      assert(!sends(half), half.screen.last.contains("no command /pa"))
      typed(ready, "/quit").input(Input.Keyboard(Key.Enter)).effects.last ==> Effect.Quit
    }

    test("/help opens a dialog of the commands and keys; Escape closes it") {
      val helped = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(30, 110))
        .message(Msg.Opened)
      val open = typed(helped, "/help").input(Input.Keyboard(Key.Enter))
      open.state.modal ==> Some(ChatScreen.Dialog.Help)
      val shown = open.screen.mkString("\n")
      assert(
        shown.contains("─ help"),
        shown.contains("/theme"),
        shown.contains("switch the colour theme"),
        shown.contains("ctrl-p"),
        shown.contains("click the thinking line")
      )
      assert(!sends(open))
      val closed = open.input(Input.Keyboard(Key.Escape))
      closed.state.modal ==> None
      assert(!closed.screen.mkString.contains("click the thinking line"))
    }

    test("a failure is painted before the thinking line, which stays") {
      val failed =
        ready.message(Msg.Arrived(Vector(), step = Some("call-model"))).message(Msg.Failed("down"))
      said(failed) ==> Vector("ᚺ down", "ᚠ grit is thinking…")
    }
  }
}
