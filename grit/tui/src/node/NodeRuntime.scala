package grit.tui.node

import java.util.concurrent.LinkedBlockingQueue
import scala.util.Using
import grit.tui.model.input.Input
import grit.tui.model.surface.{Frame, Size}
import grit.tui.runtime.{Background, Effect, Host, Mailbox, Runtime, Scheduler, TimerId}
import grit.tui.wire.input.Decoder
import grit.tui.wire.paint.{Ansi, Painter}
import grit.tui.wire.term.{Stdout, SystemTerminal, Terminal}
import grit.tui.wire.term.Terminal.given

/** The loop for a [[NodeApp]]: `grit.tui.runtime.Runtime`'s shape (one thread owns the
  * state, a reader only enqueues, one paint per batch, teardown in reverse), with the
  * difference that layout, the wrap memo and the pointer are the loop's own -- held in a
  * [[Loop]] value and stepped purely.
  */
final class NodeRuntime[S, M](
    app: NodeApp[S, M],
    term: Terminal,
    scheduler: Scheduler,
    host: Host[M]^ = Host.none[M]
) {

  import NodeRuntime.Event

  private val queue = new LinkedBlockingQueue[Event[M]]()
  @volatile private var running = true
  private var previous: Option[Frame] = None
  private var lastSize: Size = Size(0, 0)
  private val BatchLimit = 256
  private val mailbox: Mailbox[M] = msg => { val _ = queue.offer(Event.FromApp(msg)) }

  def run(): Unit = Using.Manager { use =>
    use(term)
    use(scheduler)
    term.enterRaw()
    val (s0, e0) = app.init
    var loop = Loop.start[S, M](s0)
    interpret(e0)
    loop = repaint(loop, force = true)
    val reader = Background.start("tui-input")(() => readLoop())
    try {
      while (running) {
        var event = queue.take()
        var handled = 0
        var seen = 0
        while (event != null && running && seen < BatchLimit) {
          val (next, touched) = step(event, loop)
          loop = next
          if (touched) handled += 1
          seen += 1
          event = if (seen < BatchLimit) queue.poll() else null
        }
        if (running && handled > 0) { loop = repaint(loop, force = false) }
      }
    } finally {
      running = false
      reader.close()
    }
  }.get

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
      val chars = try { term.read(Runtime.DefaultEscapeTimeoutMs) }
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

object NodeRuntime {

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
  def run[S, M](app: NodeApp[S, M], host: Host[M]^): Unit =
    SystemTerminal.open() match {
      case None =>
        val size = Size.screen(app.fallbackSize)
        val (frame, _) = Loop.paint(Loop.start[S, M](app.init._1), app, size)
        Stdout.emit(Painter.paint(frame, None) + Ansi.reset + "\n")
      case Some(term) => new NodeRuntime(app, term, Scheduler.create(), host).run()
    }
}
