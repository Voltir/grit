package grit.tui.runtime

import java.util.concurrent.LinkedBlockingQueue
import scala.util.{Failure, Success, Using}
import grit.tui.model.input.Input
import grit.tui.model.surface.{Frame, Size}
import grit.tui.wire.input.Decoder
import grit.tui.wire.paint.{Ansi, Painter}
import grit.tui.wire.term.{Stdout, SystemTerminal, Terminal}
import grit.tui.wire.term.Terminal.given

/** The loop.
  *
  * One thread owns the state and runs `update`; a reader thread only *enqueues*, so
  * nothing here needs a lock. Everything that arrives -- a decoded input, a fired timer,
  * a host's answer -- is an event on one queue, so ordering is the queue's and not a race
  * between threads. Everything the loop holds between events (the state, what was last
  * painted, the wrap memo, the pointer) is one [[Loop]] value, stepped purely; this class
  * only performs what comes back.
  */
final class Runtime[S, M <: caps.Pure](
    app: App[S, M],
    term: Terminal,
    scheduler: Scheduler,
    host: Host[M]^ = Host.none[M],
    escapeTimeoutMs: Long = Runtime.DefaultEscapeTimeoutMs
) {

  import Runtime.Event

  private val queue = new LinkedBlockingQueue[Event[M]]()
  @volatile private var running = true
  private var previous: Option[Frame] = None
  private var lastSize: Size = Size(0, 0)

  /** How many events one batch may absorb before a frame is owed. A burst of mouse motion
    * is tens, not hundreds, but a producer faster than the loop must never be able to
    * starve painting entirely.
    */
  private val BatchLimit = 256
  @volatile private var current: Option[S] = None

  /** The state as of the last event handled, or None before the loop has started. A
    * read-only window for a host embedding grit.tui, and how a test observes a pure app.
    */
  def state: Option[S] = current

  /** The one way a message reaches the loop from outside it -- a [[Host]]'s answers, and
    * an embedding program's or a test's own. Safe from any thread. It holds the queue
    * alone, not the runtime, which holds the terminal.
    */
  val mailbox: Mailbox[M] = msg => { val _ = queue.offer(Event.FromApp(msg)) }

  /** Run until the app quits, then give everything back in reverse. Blocks the calling
    * thread.
    *
    * Teardown runs innermost-outward: the **reader** is joined first, in the loop's own
    * `finally`, so it is never inside `term.read` while the terminal is restored; then the
    * **scheduler**, because a timer firing after the terminal is back in cooked mode
    * paints escape sequences into the user's shell; then the **terminal**. The last two
    * are `Using.Manager`'s reverse-registration order. Every release is idempotent.
    */
  def run(): Unit = Using.Manager { use =>
    use(term)
    use(scheduler)
    term.enterRaw()
    val (s0, e0) = app.init
    var loop = Loop.start[S, M](s0)
    current = Some(s0)
    interpret(e0)
    loop = repaint(loop, force = true)
    val reader = Background.start("tui-input")(() => readLoop())
    try {
      while (running) {
        var event: Option[Event[M]] = Some(queue.take())
        var handled = 0
        var seen = 0
        while (running && seen < BatchLimit && event.isDefined) {
          event match {
            case Some(e) =>
              val (next, touched) = step(e, loop)
              loop = next
              current = Some(loop.state)
              if (touched) handled += 1
            case None => ()
          }
          seen += 1
          // `poll` answers null for an empty queue: the one null, turned into an Option here.
          event = if (seen < BatchLimit) Option(queue.poll()) else None
        }
        if (running && handled > 0) { loop = repaint(loop, force = false) }
      }
    } finally {
      running = false
      reader.close()
    }
  } match {
    // Teardown has already run; an app that threw is unrecoverable, so rethrow it.
    case Failure(e) => throw e
    case Success(_) => ()
  }

  private def step(event: Event[M], loop: Loop[S, M]): (Loop[S, M], Boolean) = event match {
    case Event.Wake => (loop, false)
    // Layout is the paint's: a resize is only a repaint at the new size.
    case Event.FromTerminal(Input.Resize(_)) => (loop, true)
    case Event.FromTerminal(in) => perform(Loop.input(loop, in, app))
    case Event.FromApp(m) => perform(Loop.message(loop, m, app))
    case Event.Own(t) => perform(Loop.tick(loop, t, app))
  }

  private def perform(r: (Loop[S, M], Vector[Effect[M]], Vector[Timer])): (Loop[S, M], Boolean) = {
    val (next, effects, timers) = r
    effects.foreach(interpret)
    timers.foreach(arm)
    (next, true)
  }

  private def idOf(t: Tick): TimerId = t match {
    case Tick.Autoscroll => TimerId.of("node-autoscroll")
    case Tick.Deadline => TimerId.of("node-deadline")
  }

  private def arm(t: Timer): Unit = t match {
    case Timer.Arm(tick, ms) =>
      scheduler.after(idOf(tick), ms)(() => { val _ = queue.offer(Event.Own(tick)) })
    case Timer.Disarm(tick) => scheduler.cancel(idOf(tick))
  }

  private def repaint(loop: Loop[S, M], force: Boolean): Loop[S, M] = {
    val size = Size.screen(term.size)
    val fresh = force || size != lastSize
    val (frame, next) = Loop.paint(loop, app, size)
    term.write(Painter.paint(frame, if (fresh) None else previous))
    term.flush()
    previous = Some(frame)
    lastSize = size
    next
  }

  private def interpret(effect: Effect[M]): Unit = effect match {
    case Effect.NoOp => ()
    case Effect.Batch(effects) => effects.foreach(interpret)
    case Effect.Cancel(timer) => scheduler.cancel(timer)
    case Effect.CopyOut(text) => term.copyOut(text)
    case Effect.Invalidate => previous = None
    case Effect.Quit =>
      running = false
      val _ = queue.offer(Event.Wake)
    case Effect.ToHost(msg) => host.receive(msg, mailbox)
    case Effect.After(timer, delayMs, msg) =>
      scheduler.after(timer, delayMs)(() => { val _ = queue.offer(Event.FromApp(msg)) })
  }

  private def readLoop(): Unit = {
    var decoder = Decoder.empty
    while (running) {
      val chars = try { term.read(escapeTimeoutMs) }
      catch { case _: Throwable => "" }
      val (events, next) =
        if (chars.isEmpty) { decoder.flush }
        else { decoder.feed(chars) }
      decoder = next
      events.foreach(i => { val _ = queue.offer(Event.FromTerminal(i)) })
      if (term.resized()) {
        val _ = queue.offer(Event.FromTerminal(Input.Resize(Size.screen(term.size))))
      }
    }
  }
}

object Runtime {

  /** How long a held ESC waits for the rest of its sequence before it is answered as the
    * Escape key. JLine's default for the same decision is 80ms.
    */
  val DefaultEscapeTimeoutMs: Long = 80L

  /** Everything the loop can be woken by. In the companion, not the class: an enum nested
    * in the class is a path-dependent type, and a lambda that names it captures `this`.
    */
  private enum Event[+M] {
    case FromTerminal(input: Input)
    case FromApp(msg: M)
    case Own(tick: Tick)
    case Wake extends Event[Nothing]
  }

  /** Run `app` on the system terminal, or print one frame if there is none. */
  def run[S, M <: caps.Pure](app: App[S, M], host: Host[M]^): Unit =
    SystemTerminal.open() match {
      case None =>
        val size = Size.screen(app.fallbackSize)
        val (frame, _) = Loop.paint(Loop.start[S, M](app.init._1), app, size)
        Stdout.emit(Painter.paint(frame, None) + Ansi.reset + "\n")
      case Some(term) => new Runtime(app, term, Scheduler.create(), host).run()
    }
}
