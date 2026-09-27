package grit.app.chat

import grit.app.look.{Look, Theme}
import grit.core.approval.Approval
import grit.core.id.TurnSeq
import grit.core.message.{Cost, Tokens}
import grit.tui.components.pane.Anchor
import grit.tui.components.tree.Scroller
import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.surface.{Color, Pos, Size}
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
            Msg.Arrived(
              Vector(Said(ChatScreen.Voice.User, "hi"), Said(ChatScreen.Voice.Reply, "hello")),
              step = Some("call-model")
            )
          )
        val surface = h.painted._1.surface
        val palette: Set[Color] = theme.productIterator.collect { case c: Color => c }.toSet
        // Set on every cell, and to one of this theme's colours, never another's or a literal.
        surface.cells.map(_.style.bg).filterNot(_.exists(palette)).distinct ==> Vector.empty
        surface.at(size.rows / 2, size.cols / 2).style.bg ==> Some(theme.ground)
      }
    }

    test("the header names the model, and the session beside it, faint") {
      val h = Headless.start(
        new ChatScreen.App("test-model", Theme.Default, Tokens(16000), "tuning"),
        size
      )
      val top = h.screen(0)
      assert(top.contains(" test-model · tuning "))
      val at = h.painted._1.surface.at(0, top.indexOf("tuning")).style
      at.fg ==> Some(Theme.Default.faint)
      at.bg ==> Some(Theme.Default.slab)
      h.painted._1.surface.at(0, top.indexOf("test-model")).style.fg ==> Some(Theme.Default.ink)
      assert(!started.screen(0).contains("·"))
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

    test("a turn in progress spins the Futhark last; its reply takes that line, spinner stopped") {
      val asked = ready.message(
        Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi")), step = Some("call-model"))
      )
      asked.effects.last ==> Effect.After(ChatScreen.Runes, ChatScreen.TickMs, Msg.Tick)
      said(asked) ==> Vector("▌ᛗ hi", "ᚠ grit is thinking…")
      said(asked.message(Msg.Tick).message(Msg.Tick)) ==> Vector("▌ᛗ hi", "ᚦ grit is thinking…")
      val done =
        asked.message(Msg.Arrived(Vector(Said(ChatScreen.Voice.Reply, "hello")), step = None))
      said(done) ==> Vector("▌ᛗ hi", "▌ᚨ hello")
      done.effects.last ==> Effect.Cancel(ChatScreen.Runes)
      assert(done.screen.last.contains("ᛁ idle"))
    }

    test("the status line names the step and times it, from the step's first tick") {
      def status(h: Headless[ChatScreen.State, Msg]) = h.screen.last.trim
      val assembling =
        ready.message(Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi")), Some("assemble")))
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

    test(
      "the turn panel lists the system prompt's fragments, each with its estimate, two to a row"
    ) {
      val view = TurnView(
        TurnSeq(0),
        "hello",
        None,
        Vector.empty,
        None,
        Some(
          TurnView.Window(Tokens(683), Tokens(0), Tokens(0), Tokens(0), Tokens(5), Vector.empty)
        ),
        None,
        None,
        None,
        Vector(
          TurnView.Part("base", Tokens(42)),
          TurnView.Part("edge", Tokens(31)),
          TurnView.Part("AGENTS.md", Tokens(610))
        )
      )
      val shown = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(35, 110))
        .message(Msg.Opened)
        .message(Msg.Turn(view))
        .screen
        .mkString("\n")
      assert(
        shown.contains("prompt   base 42 · edge 31"),
        shown.contains("          AGENTS.md 610")
      )
    }

    test("from 100 columns the turn panel stands beside the transcript; ctrl-b hides it") {
      val view = TurnView(
        TurnSeq(2),
        "what did we decide?",
        None,
        grit.turn.Turn.Step.all.map(TurnView.Step(_, Some(400))),
        Some("ward engine"),
        Some(
          TurnView.Window(
            Tokens(12),
            Tokens(300),
            Tokens(3200),
            Tokens(1700),
            Tokens(40),
            Vector(TurnSeq(0))
          )
        ),
        Some(Cost.AtLeast(BigDecimal("0.00031"))),
        Some(Tokens(5000)),
        Some(
          TurnView.Models(
            Vector(
              "model" -> grit.core.model.ModelRef(
                grit.core.model.ModelId.of("x/big").getOrElse(sys.error("id")),
                grit.core.model.Upstream.of("fireworks")
              ),
              "summary" -> grit.core.model.ModelRef(
                grit.core.model.ModelId.of("x/small").getOrElse(sys.error("id")),
                None
              )
            ),
            profiled = false,
            Some("Fireworks")
          )
        )
      )
      def at(cols: Int) =
        Headless
          .start(
            new ChatScreen.App("test-model", Theme.Default, Tokens(16000)),
            // Tall enough for every step's row, the model rows and the footer below them.
            Size(36, cols)
          )
          .message(Msg.Opened)
          .message(Msg.Turn(view))
      val shown = at(110).screen.mkString("\n")
      assert(
        shown.contains("TURN 3  · done"),
        shown.contains("ᛟ assemble"),
        shown.contains("query    ward engine"),
        shown.contains("recalled turn 1"),
        shown.contains("5.2k of 16k budget"),
        shown.contains("closing 300"),
        shown.contains("billed   5k in · ≥ $0.00031"),
        shown.contains("model    x/big @ fireworks"),
        shown.contains("Fireworks · unprofiled"),
        shown.contains("summary  x/small")
      )
      assert(at(110).screen.exists(r => r.contains("████") && r.contains("░")))
      assert(!at(99).screen.mkString.contains("TURN 3"))
      assert(!at(110).inputs(Input.Keyboard(Key.Ctrl('b'))).screen.mkString.contains("TURN 3"))
    }

    test("the panel's tabs are pills across its top; ctrl-t and a click switch them") {
      val wide = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(30, 110))
        .message(Msg.Opened)
      // The pills sit on the panel's first row, the one shown lit on the accent.
      val pills = wide.screen(1)
      assert(pills.contains(" turn "), pills.contains(" session "))
      val lit = wide.painted._1.surface.at(1, pills.indexOf(" turn ") + 1).style
      lit.bg ==> Some(Theme.Default.headerBg)
      wide.painted._1.surface.at(1, pills.indexOf(" session ") + 1).style.bg ==>
        Some(Theme.Default.slab)
      assert(wide.screen.mkString.contains("no turn yet"))

      // Ctrl-t: the session tab, and the lit pill moves with it.
      val keyed = wide.input(Input.Keyboard(Key.Ctrl('t')))
      keyed.state.tab ==> ChatScreen.Tab.Session
      keyed.painted._1.surface.at(1, pills.indexOf(" session ") + 1).style.bg ==>
        Some(Theme.Default.headerBg)
      // Ctrl-t again: the topics; and once more comes round to the turn.
      val topical = keyed.input(Input.Keyboard(Key.Ctrl('t')))
      topical.state.tab ==> ChatScreen.Tab.Topics
      topical.input(Input.Keyboard(Key.Ctrl('t'))).state.tab ==> ChatScreen.Tab.Turn

      // A click on a pill shows its tab.
      val clicked = click(wide, " session ")
      clicked.state.tab ==> ChatScreen.Tab.Session
      clicked.effects.last ==> Effect.NoOp
      click(clicked, " turn ").state.tab ==> ChatScreen.Tab.Turn
      // One on the tab already shown changes nothing, not even where the panel was scrolled.
      val scrolled = wide.message(Msg.PanelReader(Scroller.Msg.Scrolled(Anchor.Bottom)))
      click(scrolled, " turn ").state ==> scrolled.state
      // A hidden panel is shown by a tab.
      wide.input(Input.Keyboard(Key.Ctrl('b'))).input(Input.Keyboard(Key.Ctrl('t'))).state.panel ==>
        true
    }

    test("the session tab: its length, what it was billed and spent, recalls, and roles") {
      val keyed = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(30, 110))
        .message(Msg.Opened)
        .input(Input.Keyboard(Key.Ctrl('t')))
      // Waiting for the host until it describes the conversation.
      assert(keyed.screen.mkString.contains("reading the conversation"))
      val view = SessionView(
        turns = 2,
        messages = 4,
        input = Tokens(12300),
        output = Tokens(900),
        spent = Some(Cost.Exact(BigDecimal("0.0042"))),
        recalls = 1,
        recalled = Vector(TurnSeq(0)),
        roles = Vector(
          SessionView.Role(
            "turn",
            Vector("vendor/big-model"),
            2,
            Some(Cost.AtLeast(BigDecimal("0.001")))
          )
        )
      )
      val shown = keyed.message(Msg.Session(view)).screen.mkString("\n")
      assert(
        shown.contains("SESSION  · 2 turns"),
        shown.contains("said     4 messages"),
        shown.contains("billed   12.3k in · 900 out"),
        shown.contains("spent    $0.0042"),
        shown.contains("recalled turn 1"),
        shown.contains("by 1 of 2 turns"),
        shown.contains("turn     2 calls · ≥ $0.001"),
        shown.contains("vendor/big-model")
      )
    }

    test("the topics tab: the topics, the current marked, and how the shown turn was placed") {
      val wide = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(40, 110))
        .message(Msg.Opened)
      val on = click(wide, " topics ")
      on.state.tab ==> ChatScreen.Tab.Topics
      assert(on.screen.mkString.contains("reading the conversation"))
      val view = TopicsView(
        Vector(
          TopicsView.Row("Knots", 3, current = true),
          TopicsView.Row("Rust borrow checker", 1, current = false)
        ),
        Some(
          TopicsView.Placing(
            turn = TurnSeq(3),
            first = false,
            unclassified = None,
            pSame = Some(0.62),
            band = Some(grit.core.topic.Band.Uncertain),
            choice = Vector.empty,
            verdict = Some(grit.core.topic.Verdict.Earlier("Rust borrow checker")),
            anomaly = None,
            weights = Vector("Rust borrow checker" -> 1.0),
            elsewhere = 0.0,
            placed = Some("Rust borrow checker"),
            disagree = true
          )
        )
      )
      val painted = on.message(Msg.Topics(view))
      val shown = painted.screen.mkString("\n")
      assert(
        shown.contains("TOPICS  · 2 topics"),
        shown.contains(" ● Knots"),
        shown.contains("   Rust borrow checker"),
        shown.contains("TURN 4"),
        shown.contains("p(same)  0.62 · unsure"),
        shown.contains("model    earlier: Rust borrow chec…"),
        shown.contains("⚑ jev and the model disagree"),
        shown.contains("placed   Rust borrow checker"),
        shown.contains("1.00")
      )
      // The flag is painted in the failure colour.
      val flagRow = painted.screen.indexWhere(_.contains("⚑"))
      val flagCol = painted.screen.lift(flagRow).fold(0)(_.indexOf("jev and"))
      painted.painted._1.surface.at(flagRow, flagCol).style.fg ==> Some(Theme.Default.failure)
    }

    test("a click on a message shows its turn, whichever tab the panel was on") {
      val two = Headless
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(30, 110))
        .message(Msg.Opened)
        .message(
          Msg.Arrived(
            Vector(
              Said(ChatScreen.Voice.User, "one", TurnSeq(0)),
              Said(ChatScreen.Voice.Reply, "first reply", TurnSeq(0)),
              Said(ChatScreen.Voice.User, "two", TurnSeq(1))
            ),
            None
          )
        )
        .input(Input.Keyboard(Key.Ctrl('t')))
      val pinned = click(two, "first reply")
      pinned.state.pinned ==> Some(TurnSeq(0))
      pinned.state.tab ==> ChatScreen.Tab.Turn
    }

    test("a click on an earlier message pins its turn to the panel; esc lets it go") {
      val two = ready.message(
        Msg.Arrived(
          Vector(
            Said(ChatScreen.Voice.User, "one", TurnSeq(0)),
            Said(ChatScreen.Voice.Reply, "first reply", TurnSeq(0)),
            Said(ChatScreen.Voice.User, "two", TurnSeq(1)),
            Said(ChatScreen.Voice.Reply, "second reply", TurnSeq(1))
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

    test("a reply in markdown is painted as prose, and each of its blocks belongs to its turn") {
      val two = ready.message(
        Msg.Arrived(
          Vector(
            Said(ChatScreen.Voice.User, "one", TurnSeq(0)),
            Said(ChatScreen.Voice.Reply, "**first**\n\n- a point\n\n```\ncode\n```", TurnSeq(0)),
            Said(ChatScreen.Voice.User, "two", TurnSeq(1)),
            Said(ChatScreen.Voice.Reply, "second reply", TurnSeq(1))
          ),
          None
        )
      )
      val shown = two.screen.map(_.dropRight(1).trim)
      assert(shown.contains("▌ᚨ first"), shown.contains("• a point"), shown.contains("code"))
      assert(!shown.exists(r => r.contains("**") || r.contains("```")))
      // A click anywhere in the reply -- its list, its code -- pins the turn it answered.
      click(two, "a point").state.pinned ==> Some(TurnSeq(0))
      click(two, "code").state.pinned ==> Some(TurnSeq(0))
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
        .message(
          Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi", TurnSeq(0))), Some("call-model"))
        )
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
        ready.message(
          Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi", TurnSeq(0))), Some("call-model"))
        )
      val streaming = asked.message(Msg.Heard(ChatScreen.Hearing(TurnSeq(0), "hmm", "Fehu is")))
      said(streaming).map(_.stripSuffix("▍").trim) ==> Vector("▌ᛗ hi", "▌ᚨ Fehu is")
      // What was heard of another turn is not this one's reply.
      said(asked.message(Msg.Heard(ChatScreen.Hearing(TurnSeq(7), "", "other")))) ==>
        Vector("▌ᛗ hi", "ᚠ grit is thinking…")
      val recorded = streaming.message(
        Msg.Arrived(Vector(Said(ChatScreen.Voice.Reply, "Fehu is wealth.", TurnSeq(0))), None)
      )
      said(recorded) ==> Vector("▌ᛗ hi", "▌ᚨ Fehu is wealth.")
      recorded.state.hearing ==> None
    }

    test("a tool loop is one faint line per step, and the call being written is shown") {
      def tools(h: Headless[ChatScreen.State, Msg]): Vector[String] =
        h.screen.drop(1).map(_.dropRight(1).trim).filter(_.startsWith(Look.Runes.Tool))
      val asked = ready.message(
        Msg.Arrived(
          Vector(
            Said(ChatScreen.Voice.User, "what is in it?", TurnSeq(0)),
            Said(ChatScreen.Voice.Tool, "read notes.txt", TurnSeq(0)),
            Said(ChatScreen.Voice.Tool, "← 12 lines", TurnSeq(0))
          ),
          Some("call-model:1")
        )
      )
      tools(asked) ==> Vector("ᛏ read notes.txt", "ᛏ ← 12 lines")
      val calling = asked.message(
        Msg.Heard(ChatScreen.Hearing(TurnSeq(0), "", "", Vector("list", "search")))
      )
      tools(calling).lastOption ==> Some("ᛏ calling search…")
      // Another turn's call is not this one's.
      tools(asked.message(Msg.Heard(ChatScreen.Hearing(TurnSeq(3), "", "", Vector("list"))))) ==>
        tools(asked)
      val answered = calling.message(
        Msg.Arrived(Vector(Said(ChatScreen.Voice.Reply, "Twelve lines.", TurnSeq(0))), None)
      )
      tools(answered) ==> Vector("ᛏ read notes.txt", "ᛏ ← 12 lines")
      said(answered).lastOption ==> Some("▌ᚨ Twelve lines.")
    }

    test("a call that asks is shown, and y, or n and a reason, answers it through the host") {
      val q = ChatScreen.Asked(
        grit.core.id.WorkflowId("c:0"),
        grit.core.id.ToolCallId("t1"),
        "Edit notes.txt (1 replacement):\n- draft\n+ final"
      )
      val asking = ready
        .message(
          Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "edit it", TurnSeq(0))), Some("tool:0:0"))
        )
        .message(Msg.Asking(Some(q)))
      def shows(h: Headless[ChatScreen.State, Msg], text: String) =
        h.screen.exists(_.contains(text))
      assert(shows(asking, "Edit notes.txt"), shows(asking, "+ final"), shows(asking, "y approves"))
      assert(shows(asking, "waiting for you"))
      def answers(h: Headless[ChatScreen.State, Msg]) = h.effects.collect {
        case Effect.ToHost(a: Msg.Answer) => a
      }
      val enter = Input.Keyboard(Key.Enter)
      val unclear = typed(asking, "maybe").inputs(enter)
      answers(unclear) ==> Vector.empty
      assert(!sends(unclear), shows(unclear, "Edit notes.txt"), shows(unclear, "answer y"))
      val declined = typed(asking, "n not that file").inputs(enter)
      answers(declined) ==>
        Vector(Msg.Answer(q.workflow, q.call, Approval.Declined(Some("not that file"))))
      assert(!sends(declined), !shows(declined, "Edit notes.txt"))
      // The host says it is asking until the call moves on; this screen does not ask again.
      assert(!shows(declined.message(Msg.Asking(Some(q))), "y approves"))
      answers(typed(asking, "y").inputs(enter)) ==>
        Vector(Msg.Answer(q.workflow, q.call, Approval.Approved))
    }

    test("a person's wait has its own row, and the tool is timed only once they answer") {
      val before = Vector("call-model", "record-call:0", "ask:0:0").map(TurnView.Step(_, Some(300)))
      def view(running: String, steps: Vector[TurnView.Step]) =
        TurnView(TurnSeq(0), "edit it", Some(running), steps, None, None, None, None)
      def ticks(h: Headless[ChatScreen.State, Msg], n: Int) =
        (1 to n).foldLeft(h)((at, _) => at.message(Msg.Tick))
      def row(h: Headless[ChatScreen.State, Msg], name: String) =
        h.screen.find(_.contains(s" $name ")).fold("")(_.trim)
      val waiting = ticks(
        Headless
          .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(30, 110))
          .message(Msg.Opened)
          .message(Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "edit it")), Some("wait:0:0")))
          .message(Msg.Turn(view("wait:0:0", before))),
        20
      )
      assert(
        waiting.screen.exists(_.contains("TURN 1  · ᛗ waiting for you")),
        row(waiting, "wait:0:0").contains("ᛗ wait:0:0"),
        row(waiting, "wait:0:0").contains("2.4s"),
        row(waiting, "tool:0:0").isEmpty
      )
      val running = ticks(
        waiting
          .message(Msg.Arrived(Vector(), Some("tool:0:0")))
          .message(Msg.Turn(view("tool:0:0", before :+ TurnView.Step("wait:0:0", Some(2400))))),
        1
      )
      assert(
        row(running, "wait:0:0").contains("2.4s"),
        row(running, "tool:0:0").contains("ᛏ tool:0:0"),
        row(running, "tool:0:0").contains("0.1s")
      )
    }

    test("answer: y or yes approves; n or no declines, a reason after; anything else none") {
      Vector("y", " Yes ", "YES").map(ChatScreen.answer) ==> Vector.fill(3)(Some(Approval.Approved))
      ChatScreen.answer("n") ==> Some(Approval.Declined(None))
      ChatScreen.answer("no  use git mv instead ") ==>
        Some(Approval.Declined(Some("use git mv instead")))
      Vector("", "yes please", "nope", "sure").map(ChatScreen.answer) ==> Vector.fill(4)(None)
    }

    test("the opened turn shows what has been heard: the reasoning, then the text") {
      val view =
        TurnView(TurnSeq(0), "hi", Some("call-model"), Vector.empty, None, None, None, None)
      val open = click(
        ready
          .message(
            Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi", TurnSeq(0))), Some("call-model"))
          )
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

    test("a closed period paints as a rule marked closed, then why and what it came to") {
      val closed = ready.message(
        Msg.Arrived(
          Vector(
            Said(ChatScreen.Voice.User, "hi"),
            Said(ChatScreen.Voice.Closed, "resolved (0.86) · We chose staging.", TurnSeq(0))
          ),
          None
        )
      )
      val rows = closed.screen.map(_.dropRight(1).trim)
      val rule = rows.indexWhere(_.contains(" closed "))
      assert(rule > 0, rows.lift(rule).exists(_.startsWith("═")))
      rows.lift(rule + 1) ==> Some("resolved (0.86) · We chose staging.")
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
        shown.contains("/theme      switch the colour theme"),
        shown.contains("/panel"),
        shown.contains("/summaries  show or hide turn summaries"),
        shown.contains("/help"),
        shown.contains("/quit")
      )
      // The list, too, is painted in the theme's colours, every cell of it.
      assert(listed.painted._1.surface.cells.count(_.style.bg.isEmpty) == 0)
      val narrowed = typed(listed, "th").screen.mkString("\n")
      assert(narrowed.contains("/theme      switch"), !narrowed.contains("/panel"))
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
      assert(opened.screen.mkString.contains("/theme      switch"))
      opened.input(Input.Keyboard(Key.Escape)).state.editor.text ==> "half a thought"
      val ran = typed(opened, "panel").input(Input.Keyboard(Key.Enter))
      ran.state.panel ==> false
      ran.state.editor.text ==> "half a thought"
    }

    test("ctrl-p over an empty prompt, then Escape, leaves it empty: a command typed next runs") {
      val closed = ready
        .input(Input.Keyboard(Key.Ctrl('p')))
        .input(Input.Keyboard(Key.Escape))
      closed.state.editor.text ==> ""
      val ran = typed(closed, "/panel").input(Input.Keyboard(Key.Enter))
      ran.state.panel ==> false
      assert(!sends(ran))
    }

    test("/theme repaints every colour, whether typed out or chosen from the lists") {
      def ground(h: Headless[ChatScreen.State, Msg]) =
        h.painted._1.surface.at(size.rows / 2, size.cols / 2).style.bg
      val talked = ready.message(
        Msg.Arrived(
          Vector(Said(ChatScreen.Voice.User, "hi"), Said(ChatScreen.Voice.Reply, "hello")),
          None
        )
      )
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

    test("/theme asks the host to keep the choice; what the host notes is shown") {
      val chosen = typed(ready, "/theme nightshade").input(Input.Keyboard(Key.Enter))
      chosen.effects.last ==> Effect.ToHost(Msg.KeepTheme("nightshade"))
      assert(!sends(chosen))
      val noted = chosen.message(Msg.Noted("theme nightshade, not kept: read-only"))
      assert(noted.screen.last.contains("not kept: read-only"))
    }

    test("a // draft is a message starting with /: sent with one slash, no list opened") {
      val typing = typed(ready, "//")
      typing.state.palette ==> None
      assert(!typing.screen.mkString.contains("switch the colour theme"))
      val sent = typed(typing, "etc/hosts is odd").input(Input.Keyboard(Key.Enter))
      sent.effects.last ==> Effect.ToHost(Msg.Send("/etc/hosts is odd"))
      sent.state.editor.text ==> ""
      // Ctrl-p over it puts it aside, as over any message.
      val listed = typed(ready, "//x").input(Input.Keyboard(Key.Ctrl('p')))
      listed.state.editor.text ==> "/"
      listed.input(Input.Keyboard(Key.Escape)).state.editor.text ==> "//x"
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
        .start(new ChatScreen.App("test-model", Theme.Default, Tokens(16000)), Size(34, 110))
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
      // The dialog is as tall as what it lists: its frame closes under the last row.
      val rows = open.screen
      val last = rows.indexWhere(_.contains("click the thinking line"))
      assert(last > 0, rows.lift(last + 1).exists(_.contains("╰")))
      assert(!sends(open))
      val closed = open.input(Input.Keyboard(Key.Escape))
      closed.state.modal ==> None
      assert(!closed.screen.mkString.contains("click the thinking line"))
    }

    test("/summaries shows each turn's summary under its reply, faint; again hides them") {
      val talked = ready.message(
        Msg.Arrived(
          Vector(
            Said(ChatScreen.Voice.User, "one", TurnSeq(0)),
            Said(ChatScreen.Voice.Reply, "first reply", TurnSeq(0)),
            Said(ChatScreen.Voice.User, "two", TurnSeq(1))
          ),
          Some("call-model")
        )
      )
      // Turn 0's summary is written after turn 1 has begun: it belongs under turn 0's reply.
      val summarised = talked.message(
        Msg.Arrived(
          Vector(),
          Some("call-model"),
          Vector(ChatScreen.Summarised(TurnSeq(0), "the user said\n one"))
        )
      )
      def rows(h: Headless[ChatScreen.State, Msg]) =
        h.screen
          .drop(1)
          .map(_.dropRight(1).trim)
          .filter(r => r.nonEmpty && !r.startsWith("━"))
          .take(4)
      // Off by default: the summary is kept, not shown.
      assert(!summarised.screen.mkString.contains("the user said"))
      val shown = typed(summarised, "/summaries").input(Input.Keyboard(Key.Enter))
      assert(!sends(shown), shown.screen.last.contains("summaries shown"))
      rows(shown) ==> Vector("▌ᛗ one", "▌ᚨ first reply", "ᛚ the user said one", "▌ᛗ two")
      val at = shown.screen.indexWhere(_.contains("ᛚ the user said"))
      val col = shown.screen.lift(at).map(_.indexOf("the user")).getOrElse(0)
      shown.painted._1.surface.at(at, col).style.fg ==> Some(Theme.Default.faint)
      // A click on it is a click on its turn.
      click(shown, "the user said").state.pinned ==> Some(TurnSeq(0))
      // It survives a change of theme, and a summary arriving while shown is placed too.
      val themed = typed(shown, "/theme abyss").input(Input.Keyboard(Key.Enter))
      assert(themed.screen.mkString.contains("ᛚ the user said one"))
      val later = themed.message(
        Msg.Arrived(
          Vector(Said(ChatScreen.Voice.Reply, "second reply", TurnSeq(1))),
          None,
          Vector(ChatScreen.Summarised(TurnSeq(1), "then two"))
        )
      )
      assert(
        later.screen.indexWhere(_.contains("ᛚ then two")) ==
          later.screen.indexWhere(_.contains("second reply")) + 1
      )
      val hidden = typed(later, "/summaries").input(Input.Keyboard(Key.Enter))
      assert(!hidden.screen.mkString.contains("ᛚ"), hidden.screen.last.contains("summaries hidden"))
    }

    test("a new conversation is welcomed: grit's name, the tagline, the keys; gone once said") {
      val name = "ᚷ ᚱ ᛁ ᛏ"
      // Not while the engine opens, nor before the host has read the conversation.
      assert(!started.screen.mkString.contains(name), !ready.screen.mkString.contains(name))
      val empty = ready.message(Msg.Arrived(Vector(), None))
      val shown = empty.screen
      val at = shown.indexWhere(_.contains(name))
      assert(at > 1, shown.exists(_.contains("memory, not scrollback")))
      assert(shown.exists(r => r.contains("ctrl-t") && r.contains("the panel's tabs")))
      // Centred: as much room either side of the name, a cell or so apart.
      val row = shown.lift(at).getOrElse("").dropRight(1)
      val left = row.indexOf(name)
      val right = row.length - left - name.length
      assert(math.abs(left - right) <= 1)
      empty.painted._1.surface.at(at, left).style.fg ==> Some(Theme.Default.grit)
      // Anything said, or a failure, takes its place.
      val said1 =
        empty.message(Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi")), Some("assemble")))
      assert(!said1.screen.mkString.contains(name), said1.screen.mkString.contains("ᛗ hi"))
      assert(!empty.message(Msg.Failed("down")).screen.mkString.contains(name))
      // A conversation that already has something in it is never welcomed.
      assert(
        !ready
          .message(Msg.Arrived(Vector(Said(ChatScreen.Voice.User, "hi")), None))
          .screen
          .mkString
          .contains(name)
      )
    }

    test("a failure is painted before the thinking line, which stays") {
      val failed =
        ready.message(Msg.Arrived(Vector(), step = Some("call-model"))).message(Msg.Failed("down"))
      said(failed) ==> Vector("ᚺ down", "ᚠ grit is thinking…")
    }

    test("a failure whose reason breaks a line is painted on one") {
      val failed = ready.message(Msg.Failed("failed: Model(HTTP 504: error code: 504\n)"))
      said(failed) ==> Vector("ᚺ failed: Model(HTTP 504: error code: 504 )")
    }
  }
}
