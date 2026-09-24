package grit.tui.runtime

import java.util.concurrent.{CountDownLatch, TimeUnit}
import grit.tui.components.{Node, OnInput, Passive}
import grit.tui.components.Node.*
import grit.tui.model.input.{Input, Key}
import grit.tui.model.surface.{Cell, Pos, Size, Surface}
import grit.tui.wire.paint.Vt
import utest.*

/** The loop, driven headlessly against a [[FakeTerminal]].
  *
  * Every scripted app here is a **pure** function -- capture checking rejects one that
  * closes over a counter, which is the discipline working rather than getting in the
  * way. Observations therefore live in the state and are read back through
  * [[Runtime.state]], which is also what a host embedding grit.tui would use.
  */
object RuntimeTests extends TestSuite {

  private enum Msg extends caps.Pure {
    case Typed(ch: Char)
    case Tick
    case Escaped
    case Located(pos: Pos)
    case Refresh
    case Done
  }

  private final case class St(
      typed: String = "",
      ticks: Int = 0,
      escapes: Int = 0,
      located: Option[Pos] = None
  )

  private val ticker = TimerId.of("ticker")
  private val Esc = "\u001b"

  /** The binding every scripted app has: a printable character is a keystroke, Escape is
    * Escaped, Ctrl-Q quits.
    */
  private val bind: OnInput[Msg] = {
    case Input.Keyboard(Key.Printable(c)) => Some(Msg.Typed(c))
    case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Done)
    case Input.Keyboard(Key.Escape) => Some(Msg.Escaped)
    case _ => None
  }

  /** Paints `text` on its first row, and `fill` across every cell it is given. */
  private final case class Card(text: String, fill: Char = ' ') extends Passive {
    def measure(avail: Size): Size = avail
    def render(size: Size): Surface = Surface.filled(size, Cell(fill)).write(0, 0, text)
  }

  private final class Scripted(on: ((Msg, St)) -> (St, Effect[Msg]), fill: Char = ' ')
      extends App[St, Msg] {
    def init: (St, Effect[Msg]) = (St(), Effect.NoOp)
    def update(msg: Msg, state: St): (St, Effect[Msg]) = on((msg, state))
    def view(state: St): Node[Msg] = paint(Card(state.typed, fill)).onKey(bind)
  }

  /** An app with one press target, four rows down and two tall, that records where each
    * press landed. The oracle for input being routed against what was painted.
    */
  private final class Located extends App[St, Msg] {
    def init: (St, Effect[Msg]) = (St(), Effect.NoOp)
    def update(msg: Msg, state: St): (St, Effect[Msg]) = msg match {
      case Msg.Located(p) => (state.copy(located = Some(p)), Effect.NoOp)
      case Msg.Typed('r') => (state.copy(located = None), Effect.Invalidate)
      case Msg.Done => (state, Effect.Quit)
      case _ => (state, Effect.NoOp)
    }
    def view(state: St): Node[Msg] =
      column(
        fixed(4) -> paint(Card("")),
        fixed(2) -> paint(Card("")).onPress(p => Some(Msg.Located(p))),
        flex() -> paint(Card(""))
      ).onKey(bind)
  }

  /** A mouse press at 0-based `pos`, as the terminal reports it (SGR, 1-based). */
  private def pressAt(pos: Pos): String = s"$Esc[<0;${pos.col + 1};${pos.row + 1}M"

  private def started[S, M <: caps.Pure](runtime: Runtime[S, M]^, name: String): CountDownLatch = {
    val done = new CountDownLatch(1)
    val t = new Thread(() => { runtime.run(); done.countDown() }, name)
    t.setDaemon(true)
    t.start()
    done
  }

  /** Run a scripted app on its own thread, hand the test its terminal and runtime, and
    * always quit and tear down afterwards.
    */
  private def runWith(on: ((Msg, St)) -> (St, Effect[Msg]))(
      body: (FakeTerminal^, Runtime[St, Msg]^) => Unit
  ): Unit = {
    val term = new FakeTerminal()
    val scheduler = Scheduler.create()
    val runtime = new Runtime(new Scripted(on), term, scheduler, escapeTimeoutMs = 20L)
    val done = started(runtime, "runtime-under-test")
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

    test("the frame is painted at the paintable screen's size, one column short") {
      // Rule 2, translated at the boundary: the last column is owed back, so a view that
      // fills everything it is given fills all but the terminal's last column.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val runtime = new Runtime(
        new Scripted({ case (_, s) => quit(s) }, fill = '#'),
        term,
        scheduler,
        escapeTimeoutMs = 20L
      )
      val done = started(runtime, "sized-runtime")
      try {
        assert(waitUntil(() => term.painted.nonEmpty))
        val v = new Vt(term.size.rows, term.size.cols)
        v.feed(term.painted)
        assert(v.cells(3)(term.size.cols - 2).ch == '#', v.cells(3)(term.size.cols - 1).ch == ' ')
      } finally {
        runtime.offer(Msg.Done)
        val _ = done.await(5L, TimeUnit.SECONDS)
        scheduler.close()
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

    test("ToHost hands the message to the host, which answers through its mailbox") {
      // The host answers from a thread of its own, as a real one must: the mailbox is
      // the loop's queue, safe from any thread.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val app = new Scripted({
        case (Msg.Tick, s) => (s, Effect.ToHost(Msg.Typed('h')))
        case (Msg.Typed(c), s) => (s.copy(typed = s.typed + c), Effect.NoOp)
        case (_, s) => (s, Effect.Quit)
      })
      val host = new Host[Msg] {
        def receive(msg: Msg, mailbox: Mailbox[Msg]): Unit = msg match {
          case Msg.Typed(c) =>
            val _ = Thread.ofVirtual().start(() => mailbox.offer(Msg.Typed(c.toUpper)))
          case _ => ()
        }
      }
      val runtime = new Runtime(app, term, scheduler, host, escapeTimeoutMs = 20L)
      runtime.offer(Msg.Tick)
      val done = new CountDownLatch(1)
      val t = new Thread(() => { runtime.run(); done.countDown() }, "hosted-runtime")
      t.setDaemon(true)
      t.start()
      // The request itself is never handled as the app's own message: only the answer is.
      assert(waitUntil(() => runtime.state.exists(_.typed == "H")))
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

    test("a press is routed to what was painted under it") {
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val runtime = new Runtime(new Located, term, scheduler, escapeTimeoutMs = 20L)
      val done = started(runtime, "located-under-test")
      try {
        term.send(pressAt(Pos(5, 3)))
        assert(waitUntil(() => runtime.state.exists(_.located.contains(Pos(5, 3)))))
        term.send(pressAt(Pos(1, 3))) // above the target: nothing there claims it
        term.send("x")
        assert(waitUntil(() => runtime.state.exists(_.typed.isEmpty)))
        assert(runtime.state.exists(_.located.contains(Pos(5, 3))))
      } finally {
        runtime.offer(Msg.Done)
        val _ = done.await(5L, TimeUnit.SECONDS)
        scheduler.close()
      }
    }

    test("a full repaint does not mean forgetting where everything is") {
      // Effect.Invalidate drops the diff baseline so the next paint is complete. If it
      // also dropped what routing reads, input would work until the window was resized.
      val term = new FakeTerminal()
      val scheduler = Scheduler.create()
      val runtime = new Runtime(new Located, term, scheduler, escapeTimeoutMs = 20L)
      val done = started(runtime, "invalidate-under-test")
      try {
        term.send(pressAt(Pos(4, 1)))
        assert(waitUntil(() => runtime.state.exists(_.located.isDefined)))
        term.send("r") // Effect.Invalidate: the next paint is a full one
        assert(waitUntil(() => runtime.state.exists(_.located.isEmpty)))
        term.send(pressAt(Pos(4, 2)))
        assert(waitUntil(() => runtime.state.exists(_.located.contains(Pos(4, 2)))))
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
        term.send(pressAt(Pos(4, 1)))
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
        def update(msg: Msg, state: St): (St, Effect[Msg]) = throw new RuntimeException("boom")
        def view(state: St): Node[Msg] = paint(Card("")).onKey(bind)
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
