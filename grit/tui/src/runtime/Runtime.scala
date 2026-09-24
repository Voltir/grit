package grit.tui.runtime

import java.util.concurrent.LinkedBlockingQueue
import scala.util.Using
import grit.tui.model.input.Input
import grit.tui.wire.input.Decoder
import grit.tui.wire.paint.Painter
import grit.tui.model.surface.{Frame, Placements, Size}
import grit.tui.wire.term.Terminal
import grit.tui.wire.term.Terminal.given

/** The loop.
  *
  * One thread owns the state and runs `update`; a reader thread only *enqueues*. That is
  * why nothing here needs a lock: next door, `updateState` was called from whichever
  * thread produced the message and interpreted commands while holding the state lock.
  *
  * Everything that arrives -- a decoded input, a fired timer -- is an [[Runtime.Event]]
  * on one queue, so ordering is the queue's and not a race between three daemon threads.
  */
final class Runtime[State, Msg](
    app: App[State, Msg]^,
    term: Terminal,
    scheduler: Scheduler,
    host: Host[Msg]^ = Host.none[Msg],
    escapeTimeoutMs: Long = Runtime.DefaultEscapeTimeoutMs
) {

  import Runtime.Event

  private val queue = new LinkedBlockingQueue[Event[Msg]]()
  @volatile private var running = true
  private var previous: Option[Frame] = None
  private var lastSize: Size = Size(0, 0)
  // Where the last painted frame put things: its own field rather than something
  // derived from `previous`, which `Effect.Invalidate` clears to force a full repaint.
  // Deriving it would tie "repaint everything" to "forget where everything is", and a
  // routing decision would depend on whether the last frame happened to be a full one.
  // This was written as a guard against a later refactor rather than a live bug; the
  // batching loop below is that refactor. Several messages now run between paints, so
  // the map an input routes against can be a few states old -- which is the rule stated
  // rather than a lapse from it: hit-testing inverts *what was painted*, and what was
  // painted is what the user was pointing at.
  private var placed: Placements = Placements.empty

  /** How many events one batch may absorb before a frame is owed. Nothing observed hits
    * this -- a burst of mouse motion is tens, not hundreds -- but a producer faster than
    * the loop must never be able to starve painting entirely.
    */
  private val BatchLimit = 256
  @volatile private var current: Option[State] = None

  /** The state as of the last message handled, or None before the loop has started.
    *
    * A read-only window for a host embedding grit.tui -- and the only way a test can observe a pure app, since `update` cannot record anything on the side.
    */
  def state: Option[State] = current

  /** Deliver a message from outside the loop -- a timer, or a host embedding grit.tui. */
  def offer(msg: Msg): Unit = { val _ = queue.offer(Event.FromApp(msg)) }

  /** What a [[Host]] answers through: the queue alone, not the runtime, which holds the
    * terminal -- a host keeps its mailbox on threads of its own.
    */
  private val mailbox: Mailbox[Msg] = msg => { val _ = queue.offer(Event.FromApp(msg)) }

  /** Run until the app quits, then give everything back in reverse. Blocks the calling
    * thread.
    *
    * Teardown runs innermost-outward, and each step says why it is where it is:
    *
    *   1. the **reader** is joined first, in the loop's own `finally`. It was a bare
    *      daemon thread before, never joined, so it could still be inside `term.read`
    *      while the terminal was being restored underneath it;
    *   2. the **scheduler** next, because a timer that fires after the terminal is back
    *      in cooked mode paints escape sequences into the user's shell;
    *   3. the **terminal** last, once nothing that paints is still running.
    *
    * 2 and 3 are `Using.Manager`'s reverse-registration order, so that ordering is
    * stated by *where* they are registered rather than by a hand-written sequence of
    * closes. Every release is idempotent, so this is safe however the loop ended.
    */
  def run(): Unit = Using.Manager { use =>
    use(term)
    use(scheduler)
    term.enterRaw()
    var state = start()
    val reader = Background.start("tui-input")(() => readLoop())
    // Closed inside the block, so it is joined before `use` releases anything below --
    // the same reverse order, said explicitly because a `Background` carries a capture
    // set and `Using.Releasable` will not widen to it.
    try {
      while (running) {
        // One paint per *batch* of events, not one per event. A terminal in mode 1002
        // reports motion per cell, so a fast drag arrives as a burst of fifty; painting
        // each one costs fifty frames of which forty-nine are overwritten before a human
        // could see them, and the selection trails the pointer by the whole backlog.
        // Every message is still handled and every effect still interpreted -- only the
        // intermediate *frames* are skipped, and those were never visible.
        var event = queue.take()
        var handled = 0
        var seen = 0
        while (event != null && running && seen < BatchLimit) {
          step(event, state) match {
            case Some(next) => state = next; handled += 1
            case None => ()
          }
          seen += 1
          event = if (seen < BatchLimit) queue.poll() else null
        }
        if (running && handled > 0) { repaint(state, force = false) }
      }
    } finally {
      running = false
      reader.close()
    }
  }.get

  /** The initial state, painted, with the terminal's size delivered as the first event.
    *
    * `init` takes no size, so without this an app cannot lay anything out until the user
    * happens to resize the window -- and laying out in `view` is what rule 7 forbids. A
    * synthetic `Resize` means there is exactly one path by which an app learns how big it
    * is, and it is the same one at startup as at every SIGWINCH.
    */
  private def start(): State = {
    val (initial, effect) = app.init
    current = Some(initial)
    interpret(effect)
    val _ = queue.offer(Event.FromTerminal(Input.Resize(Size.screen(term.size))))
    repaint(initial, force = true)
    initial
  }

  /** One event applied: the next state when the event meant a message, `None` when it
    * meant nothing. The distinction is what the batch counts -- a frame is owed only if
    * something was handled, which is the rule the per-event loop had too.
    */
  private def step(event: Event[Msg], state: State): Option[State] = {
    val msg = event match {
      case Event.Wake => None
      case Event.FromApp(m) => Some(m)
      case Event.FromTerminal(input) => app.onInput(input, state, placed)
    }
    msg.map { m =>
      val (next, effect) = app.update(m, state)
      current = Some(next)
      interpret(effect)
      next
    }
  }

  private def repaint(state: State, force: Boolean): Unit = {
    // The size the app paints at -- and the only size an app of this library ever
    // sees: rule 2 (the last column is owed back) is translated here, at the boundary,
    // so no app can forget it and nothing downstream compensates.
    val size = Size.screen(term.size)
    val fresh = force || size != lastSize
    val frame = app.view(state)(size)
    val bytes = Painter.paint(frame, if (fresh) None else previous)
    term.write(bytes)
    term.flush()
    previous = Some(frame)
    placed = Placements(frame.surface.panes)
    lastSize = size
  }

  /** Turn an effect into action. The only place in the library that holds both the
    * terminal and the scheduler, and its signature says so.
    */
  private def interpret(effect: Effect[Msg]): Unit = effect match {
    case Effect.NoOp => ()
    case Effect.Batch(effects) => effects.foreach(interpret)
    case Effect.Cancel(timer) => scheduler.cancel(timer)
    case Effect.CopyOut(text) => term.copyOut(text)
    case Effect.Invalidate => previous = None
    case Effect.Quit =>
      running = false
      val _ = queue.offer(Event.Wake) // unblock the taker; no message is delivered
    case Effect.ToHost(msg) => host.receive(msg, mailbox)
    case Effect.After(timer, delayMs, msg) =>
      scheduler.after(timer, delayMs)(() => { val _ = queue.offer(Event.FromApp(msg)) })
  }

  /** Read characters, decode them, and enqueue. An empty read is the clock speaking: it
    * resolves a held ESC prefix, which is the decision [[Decoder]] deliberately refuses
    * to make on its own.
    */
  private def readLoop(): Unit = {
    var decoder = Decoder.empty
    while (running) {
      // A throw here would kill the thread and take all input with it, silently -- the
      // failure mode is an app that paints once and then ignores the keyboard forever.
      val chars = try { term.read(escapeTimeoutMs) }
      catch { case _: Throwable => "" }
      val (events, next) =
        if (chars.isEmpty) { decoder.flush }
        else { decoder.feed(chars) }
      decoder = next
      events.foreach(enqueue)
      if (term.resized()) { enqueue(Input.Resize(Size.screen(term.size))) }
    }
  }

  private def enqueue(input: Input): Unit = { val _ = queue.offer(Event.FromTerminal(input)) }
}

object Runtime {

  /** How long a held ESC waits for the rest of its sequence before it is answered as the
    * Escape key. JLine's default for the same decision is 80ms.
    */
  val DefaultEscapeTimeoutMs: Long = 80L

  /** Everything the loop can be woken by, on one queue so their order is defined. */
  private enum Event[+Msg] {
    case FromTerminal(input: Input)
    case FromApp(msg: Msg)

    /** Carries nothing: it exists only to unblock `queue.take()` at quit, so the loop
      * never needs a `null` message to wake itself.
      */
    case Wake extends Event[Nothing]
  }
}
