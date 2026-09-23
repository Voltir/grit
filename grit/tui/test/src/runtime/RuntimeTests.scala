package grit.tui.runtime

import java.util.concurrent.{CountDownLatch, TimeUnit}
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.{Frame, PaneId, Placements, Pos, Rect, Size, Surface}
import utest.*

/** The loop, driven headlessly.
  *
  * Every scripted app here is a **pure** function -- capture checking rejects one that
  * closes over a counter, which is the discipline working rather than getting in the
  * way. Observations therefore live in the state and are read back through
  * [[Runtime.state]], which is also what a host embedding grit.tui would use.
  */
object RuntimeTests extends TestSuite {

  private enum Msg {
    case Typed(ch: Char)
    case Tick
    case Deadline
    case Escaped
    case Sized(size: Size)
    case Located(rect: Option[Rect], empty: Boolean)
    case Refresh
    case Done
  }

  private final case class St(
      typed: String = "",
      ticks: Int = 0,
      deadlines: Int = 0,
      escapes: Int = 0,
      dragging: Boolean = false,
      sized: Option[Size] = None,
      located: Option[(Option[Rect], Boolean)] = None
  )

  private val ticker = TimerId.of("ticker")
  private val deadline = TimerId.of("deadline")
  private val Esc = "\u001b"

  /** The binding most tests want: a printable character is a keystroke, Ctrl-Q quits. */
  private val defaultBind: ((Input, St)) -> Option[Msg] = { case (input, _) =>
    input match {
      case Input.Keyboard(Key.Printable(c)) => Some(Msg.Typed(c))
      case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Done)
      case Input.Keyboard(Key.Escape) => Some(Msg.Escaped)
      case _ => None
    }
  }

  private final class Scripted(
      on: ((Msg, St)) -> (St, Effect[Msg]),
      bind: ((Input, St)) -> Option[Msg] = defaultBind
  ) extends App[St, Msg] {
    def init: (St, Effect[Msg]) = (St(), Effect.NoOp)
    def update: (Msg, St) -> (St, Effect[Msg]) = (msg, state) => on((msg, state))
    def view: St -> (Size -> Frame) = state =>
      size => Frame(Surface.blank(size).write(0, 0, state.typed))
    def onInput: (Input, St, Placements) -> Option[Msg] = (input, state, at) => bind((input, state))
  }

  private val Widget = PaneId.of("widget")

  /** An app that paints one named widget and reports, on every keystroke, where the
    * runtime says it landed. The oracle for `onInput` being told what was painted.
    */
  private final class Located extends App[St, Msg] {
    def init: (St, Effect[Msg]) = (St(), Effect.NoOp)
    def update: (Msg, St) -> (St, Effect[Msg]) = (msg, state) =>
      msg match {
        case Msg.Located(r, e) => (state.copy(located = Some((r, e))), Effect.NoOp)
        case Msg.Refresh => (state.copy(located = None), Effect.Invalidate)
        case Msg.Done => (state, Effect.Quit)
        case _ => (state, Effect.NoOp)
      }
    def view: St -> (Size -> Frame) = _ =>
      size => Frame(Surface.blank(size).blit(Surface.blank(Size(2, 3)), Pos(4, 5), Widget))
    def onInput: (Input, St, Placements) -> Option[Msg] = (input, _, at) =>
      input match {
        case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Done)
        case Input.Keyboard(Key.Printable('r')) => Some(Msg.Refresh)
        case Input.Keyboard(Key.Printable(_)) => Some(Msg.Located(at(Widget), at.isEmpty))
        case Input.Resize(_) => Some(Msg.Located(at(Widget), at.isEmpty))
        case _ => None
      }
  }

  /** Run a scripted app on its own thread, hand the test its terminal and runtime, and
    * always quit and tear down afterwards.
    */
  private def runWith(
      on: ((Msg, St)) -> (St, Effect[Msg]),
      bind: ((Input, St)) -> Option[Msg] = defaultBind
  )(body: (FakeTerminal^, Runtime[St, Msg]^) => Unit): Unit = {
    val term = new FakeTerminal()
    val scheduler = Scheduler.create()
    val runtime = new Runtime(new Scripted(on, bind), term, scheduler, escapeTimeoutMs = 20L)
    val done = new CountDownLatch(1)
    val t = new Thread(() => { runtime.run(); done.countDown() }, "runtime-under-test")
    t.setDaemon(true)
    t.start()
    try { body(term, runtime) }
    finally {
      runtime.offer(Msg.Done)
      val _ = done.await(5L, TimeUnit.SECONDS)
      scheduler.close()
    }
  }

  /** Poll until `what` holds, up to ~5s. Returns whether it ever did. */
  private def waitUntil(what: () => Boolean): Boolean = {
    var i = 0
    while (i < 200 && !what()) { Thread.sleep(25L); i += 1 }
    what()
  }

  private def quit(s: St): (St, Effect[Msg]) = (s, Effect.Quit)

  val tests = Tests {

    test("typed input reaches update through the decoder, and the frame is painted") {
      runWith {
        case (Msg.Typed(c), s) => (s.copy(typed = s.typed + c), Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send("hi")
        assert(waitUntil(() => runtime.state.exists(_.typed == "hi")))
        assert(term.painted.nonEmpty)
      }
    }

    test("a bare ESC resolves across the loop's read timeout, not inside the decoder") {
      // The reader turns an empty read into Decoder.flush, so the ambiguous-ESC clock
      // lives here and the decoder stays a pure function with no clock of its own.
      runWith {
        case (Msg.Escaped, s) => (s.copy(escapes = s.escapes + 1), Effect.NoOp)
        case (Msg.Typed(_), s) => (s, Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send(Esc)
        assert(waitUntil(() => runtime.state.exists(_.escapes == 1)))
      }
    }

    test("an escape sequence arriving whole is the named key, never ESC plus its bytes") {
      // layoutz's parser answered Escape and leaked the rest as printable characters.
      runWith {
        case (Msg.Escaped, s) => (s.copy(escapes = s.escapes + 1), Effect.NoOp)
        case (Msg.Typed(c), s) => (s.copy(typed = s.typed + c), Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send(s"$Esc[A") // Up: bound to nothing here, so it must do nothing
        term.send("z")
        assert(waitUntil(() => runtime.state.exists(_.typed == "z")))
        assert(runtime.state.exists(_.escapes == 0))
      }
    }

    test("a scheduled effect delivers its message back into the loop") {
      runWith {
        case (Msg.Typed(_), s) => (s, Effect.After(ticker, 10L, Msg.Tick))
        case (Msg.Tick, s) => (s.copy(ticks = s.ticks + 1), Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send("x")
        assert(waitUntil(() => runtime.state.exists(_.ticks == 1)))
      }
    }

    test("a cancelled timer never delivers, so a chain can actually be stopped") {
      // What layoutz could not do: `Cmd.afterMs` had no handle, so the tick fired and
      // `update` had to recognise and discard it by generation.
      runWith {
        case (Msg.Typed('a'), s) => (s, Effect.After(ticker, 400L, Msg.Tick))
        case (Msg.Typed(_), s) => (s, Effect.Cancel(ticker))
        case (Msg.Tick, s) => (s.copy(ticks = s.ticks + 1), Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send("a")
        Thread.sleep(100L)
        term.send("b")
        Thread.sleep(600L)
        assert(runtime.state.exists(_.ticks == 0))
      }
    }

    test("re-arming a named timer does not fork the chain") {
      // FINDINGS 5.4 at the runtime level: ten re-arms, one delivery. Next door each
      // re-arm ran alongside the old chain and every round trip doubled them.
      runWith {
        case (Msg.Typed(_), s) => (s, Effect.After(ticker, 200L, Msg.Tick))
        case (Msg.Tick, s) => (s.copy(ticks = s.ticks + 1), Effect.NoOp)
        case (_, s) => quit(s)
      } { (term, runtime) =>
        var i = 0
        while (i < 10) { term.send("x"); i += 1 }
        Thread.sleep(700L)
        assert(runtime.state.exists(_.ticks == 1))
      }
    }

    test("a drag that never releases is ended by its own deadline") {
      // Neither sibling solved this. Under mode 1002 the terminal reports nothing once
      // the pointer leaves the window, so no release and no motion arrive, and every
      // termination path next door needed an inbound message. The mechanism grit.tui gives
      // an app is a named deadline that each extend replaces: while extends keep coming
      // it never matures, and when they stop it is the one thing still pending.
      runWith {
        case (Msg.Typed('d'), s) =>
          (s.copy(dragging = true), Effect.After(deadline, 150L, Msg.Deadline))
        case (Msg.Typed(_), s) => (s, Effect.After(deadline, 150L, Msg.Deadline))
        case (Msg.Deadline, s) =>
          (s.copy(dragging = false, deadlines = s.deadlines + 1), Effect.Cancel(ticker))
        case (_, s) => quit(s)
      } { (term, runtime) =>
        term.send("d")
        var i = 0
        while (i < 4) { Thread.sleep(60L); term.send("e"); i += 1 }
        // Extends kept arriving faster than the deadline, so the drag is still live.
        assert(runtime.state.exists(s => s.dragging && s.deadlines == 0))
        // The pointer leaves the window. Nothing else will ever arrive.
        assert(waitUntil(() => runtime.state.exists(s => !s.dragging && s.deadlines == 1)))
        Thread.sleep(400L)
        assert(runtime.state.exists(_.deadlines == 1)) // it does not re-arm itself
      }
    }

    test("an app is told the paintable screen's size before it is asked to lay anything out") {
      // `init` takes no size, so a synthetic Resize at startup is the only way an app can
      // lay out in `update` -- and laying out in `view` is what rule 7 forbids. The same
      // path serves startup and every SIGWINCH, so there is one code path to get wrong.
      // Rule 2 is translated here, at the boundary: the size an app sees is one column
      // narrower than the terminal's, and it is the only size an app is ever handed.
      runWith(
        {
          case (Msg.Sized(sz), s) => (s.copy(sized = Some(sz)), Effect.NoOp)
          case (_, s) => quit(s)
        },
        { case (input, _) =>
          input match {
            case Input.Resize(size) => Some(Msg.Sized(size))
            case _ => None
          }
        }
      ) { (term, runtime) =>
        assert(waitUntil(() => runtime.state.exists(_.sized.contains(Size.screen(term.size)))))
        assert(runtime.state.exists(_.sized.exists(_.cols == term.size.cols - 1)))
      }
    }

    test("a burst of messages is one frame, and every one of them still happened") {
      // Reported from a real terminal as "highlighting lags behind the mouse". Mode 1002
      // reports motion per cell, so dragging fast arrives as a burst of fifty events; a
      // loop that paints each one paints fifty frames of which forty-nine are overwritten
      // before a human could see them, and the selection trails the pointer by the whole
      // backlog. Measured against `Demo2`: the queue drained 48 -> 0 one event at a time,
      // update cost 180us and paint cost 2695us, so the loop lost ground on every event.
      //
      // The fix skips *frames*, never messages -- so the two halves of that are what this
      // asserts. Deterministic because the whole burst is queued before the loop starts.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val app = new Scripted({
        case (Msg.Typed(c), s) => (s.copy(typed = s.typed + c), Effect.NoOp)
        case (_, s) => (s, Effect.Quit)
      })
      val runtime = new Runtime(app, term, scheduler, escapeTimeoutMs = 20L)
      val burst = 50
      var i = 0
      while (i < burst) { runtime.offer(Msg.Typed('x')); i += 1 }

      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "bursting-runtime")
      t.setDaemon(true)
      t.start()
      assert(waitUntil(() => runtime.state.exists(_.typed.length == burst)))

      // Every message was handled: no input is ever dropped, only frames are.
      assert(runtime.state.map(_.typed) == Some("x" * burst))
      // and the burst cost a handful of frames rather than one per message. The exact
      // count is a race with the reader thread's first poll, so this asserts the order of
      // magnitude the fix is for, not an arithmetic identity.
      val frames = term.calls.count(_ == "flush")
      assert(frames < burst / 5)

      runtime.offer(Msg.Done)
      val _ = done.await(5L, TimeUnit.SECONDS)
      scheduler.close()
    }

    test("quit restores the terminal, in reverse, exactly once") {
      // The ordering claim asserted rather than commented: the scheduler stops before
      // the terminal is restored, or a late timer paints into a cooked-mode shell.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val app = new Scripted({ case (_, s) => (s, Effect.Quit) })
      val runtime = new Runtime(app, term, scheduler, escapeTimeoutMs = 20L)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "quitting-runtime")
      t.setDaemon(true)
      t.start()
      runtime.offer(Msg.Done)
      assert(done.await(5L, TimeUnit.SECONDS))
      val lifecycle = term.calls.filter(c => c == "enterRaw" || c == "exitRaw" || c == "close")
      assert(lifecycle == Vector("enterRaw", "exitRaw", "close"))
      assert(!term.isRaw)
      assert(scheduler.pendingCount == 0)

      // Called again from a finally or a shutdown hook: the flags make it a no-op.
      term.exitRaw()
      scheduler.close()
      assert(term.calls.count(_ == "exitRaw") == 1)
    }

    test("a timer pending at quit is cancelled, not left to fire after restore") {
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val app = new Scripted({
        case (Msg.Tick, s) => (s.copy(ticks = s.ticks + 1), Effect.NoOp)
        case (Msg.Typed(_), s) => (s, Effect.After(ticker, 400L, Msg.Tick))
        case (_, s) => (s, Effect.Quit)
      })
      val runtime = new Runtime(app, term, scheduler, escapeTimeoutMs = 20L)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "late-timer-runtime")
      t.setDaemon(true)
      t.start()
      term.send("x")
      Thread.sleep(150L)
      runtime.offer(Msg.Done)
      assert(done.await(5L, TimeUnit.SECONDS))
      Thread.sleep(600L)
      assert(runtime.state.exists(_.ticks == 0))
      assert(!term.isRaw)
    }

    test("CopyOut reaches the terminal as a copy, not as paint") {
      runWith {
        case (Msg.Typed(_), s) => (s, Effect.CopyOut("selected text"))
        case (_, s) => quit(s)
      } { (term, _) =>
        term.send("c")
        assert(waitUntil(() => term.copied == Vector("selected text")))
      }
    }

    test("onInput is told where the last painted frame put things") {
      // The generalisation of "a pane maps input back through the last Viewport it
      // produced": the runtime just painted the frame, so it already knows, and an app
      // that had to keep its own map of rects would have derived state to keep fresh.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val runtime = new Runtime(new Located, term, scheduler, escapeTimeoutMs = 20L)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "located-under-test")
      t.setDaemon(true)
      t.start()
      try {
        term.send("x")
        assert(waitUntil(() => runtime.state.exists(_.located.exists(_._1.isDefined))))
        val seen = runtime.state.get.located.get
        assert(seen._1.contains(Rect(4, 5, 2, 3)))
        assert(!seen._2) // and the map was not empty, so this is a real answer
      } finally {
        runtime.offer(Msg.Done)
        val _ = done.await(5L, TimeUnit.SECONDS)
        scheduler.close()
      }
    }

    test("a full repaint does not mean forgetting where everything is") {
      // Effect.Invalidate drops the diff baseline so the next paint is complete. If it
      // also dropped the placements, a routing decision would depend on whether the
      // last frame happened to be a full one -- which is exactly the kind of coupling
      // that shows up as input working until the window is resized.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val runtime = new Runtime(new Located, term, scheduler, escapeTimeoutMs = 20L)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "invalidate-under-test")
      t.setDaemon(true)
      t.start()
      try {
        term.send("x")
        assert(waitUntil(() => runtime.state.exists(_.located.exists(_._1.isDefined))))
        term.send("r") // Effect.Invalidate: the next paint is a full one
        assert(waitUntil(() => runtime.state.exists(_.located.isEmpty)))
        term.send("y")
        assert(waitUntil(() => runtime.state.exists(_.located.exists(_._1.isDefined))))
        assert(runtime.state.get.located.get._1.contains(Rect(4, 5, 2, 3)))
      } finally {
        runtime.offer(Msg.Done)
        val _ = done.await(5L, TimeUnit.SECONDS)
        scheduler.close()
      }
    }

    test("the reader is joined before the terminal is given back") {
      // The ordering `Using` exists here to state. The reader touches the terminal, so
      // it must be provably stopped before the terminal goes away -- it was a bare
      // daemon thread, never joined, which meant it could be inside `term.read` while
      // the modes were being restored underneath it. A slow read makes the race real.
      val term = new FakeTerminal(readDelayMs = 40L)
      val scheduler = Scheduler.create()
      val runtime = new Runtime(new Located, term, scheduler, escapeTimeoutMs = 20L)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "teardown-under-test")
      t.setDaemon(true)
      t.start()
      try {
        term.send("x")
        assert(waitUntil(() => runtime.state.exists(_.located.isDefined)))
      } finally {
        runtime.offer(Msg.Done)
        assert(done.await(5L, TimeUnit.SECONDS)) // run() returned, so teardown finished
        scheduler.close()
      }
      assert(term.readsInFlightAtClose == 0) // nothing was inside term.read
      assert(!term.isRaw)
      assert(term.calls.count(_ == "exitRaw") == 1)
    }

    test("teardown runs even when the loop dies") {
      // `Using.Manager` releases on the way out however the body left, so an app that
      // throws still gets its terminal back rather than leaving a raw-mode shell.
      val term = new FakeTerminal(readDelayMs = 40L)
      val scheduler = Scheduler.create()
      val boom = new App[St, Msg] {
        def init: (St, Effect[Msg]) = (St(), Effect.NoOp)
        def update: (Msg, St) -> (St, Effect[Msg]) = (_, _) => throw new RuntimeException("boom")
        def view: St -> (Size -> Frame) = _ => size => Frame(Surface.blank(size))
        def onInput: (Input, St, Placements) -> Option[Msg] = (_, _, _) => Some(Msg.Typed('x'))
      }
      val runtime = new Runtime(boom, term, scheduler, escapeTimeoutMs = 20L)
      val threw =
        try { term.send("x"); runtime.run(); false }
        catch { case _: RuntimeException => true }
      assert(threw)
      assert(!term.isRaw) // the screen came back anyway
      assert(term.calls.contains("close"))
      // The scheduler was closed too: arming after close is a no-op, so nothing pends.
      scheduler.after(TimerId.of("after-teardown"), 1L)(() => ())
      assert(scheduler.pendingCount == 0)
      assert(term.readsInFlightAtClose == 0) // and the reader was joined on the way out
    }
  }
}
