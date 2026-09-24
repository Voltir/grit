package grit.tui.runtime

/** Names a timer so it can be cancelled or replaced.
  *
  * App-chosen and stable ("autoscroll", "stream"), which is the point: scheduling an id
  * that is already pending *replaces* it. The chain-forking bug next door -- a pointer
  * leaving an autoscroll edge and coming back started a second chain while the first was
  * still sleeping, doubling on every round trip -- is structurally impossible when the
  * timer has a name, and needs no generation counter to defend it.
  */
opaque type TimerId = String

object TimerId {
  def of(name: String): TimerId = name
  extension (id: TimerId) def name: String = id
}

/** What the runtime should do besides changing the state.
  *
  * **No case may contain a function** (README idea 4, CLAUDE.md rule 3). A thunk inside a
  * command launders a capability straight past capture checking: next door,
  * `Cmd.fire(term.copyOut(text))` type-checked happily from a capability-free `update`.
  * A sealed ADT of plain data structurally cannot capture the terminal, which makes
  * "`update` is pure" a fact about the types rather than a fact about a test.
  *
  * `Msg` is the app's own message type and appears only as *data* -- the message to
  * deliver when a timer fires. That is what lets grit.tui schedule on an app's behalf
  * without knowing what its messages mean, and without a callback: the layoutz spike had
  * to map effect-to-message by name in its interpreter because its `Effect` was closed,
  * which works for one app and not for a library.
  */
enum Effect[+Msg] {

  /** Nothing to do. */
  case NoOp extends Effect[Nothing]

  /** Several effects, in order. Never contains [[NoOp]] when built by [[Effect.batch]]. */
  case Batch(effects: Vector[Effect[Msg]])

  /** Deliver `msg` after `delayMs`, under `timer`. Replaces any pending timer of the
    * same id, and is cancelled by [[Cancel]] -- a real handle, not a stale-generation
    * stamp that lets the tick fire and be discarded.
    */
  case After(timer: TimerId, delayMs: Long, msg: Msg)

  /** Drop the pending timer named `timer`, if any. A no-op when nothing is pending. */
  case Cancel(timer: TimerId)

  /** Put `text` on the system clipboard. */
  case CopyOut(text: String)

  /** Discard the painter's memory of the screen, so the next frame paints in full. */
  case Invalidate

  /** Leave the loop and restore the terminal. */
  case Quit

  /** Hand `msg` to the [[Host]] that embeds this app: how a pure `update` asks for work
    * outside the terminal (a model call, a database write). Still plain data -- the
    * capability to do the work is the host's, never the app's -- and the host answers,
    * if at all, by offering a message back through its [[Mailbox]].
    */
  case ToHost(msg: Msg)
}

object Effect {

  /** The effects as one, dropping every [[NoOp]] and flattening nested batches.
    *
    * Normalising here is what makes `Effect` comparable in a test: an assertion can say
    * `== Effect.After(...)` without caring whether the update path wrapped it.
    */
  def batch[Msg](effects: Effect[Msg]*): Effect[Msg] = {
    val flat = Vector.newBuilder[Effect[Msg]]
    def add(e: Effect[Msg]): Unit = e match {
      case Effect.NoOp => ()
      case Effect.Batch(inner) => inner.foreach(add)
      case other => flat += other
    }
    effects.foreach(add)
    val kept = flat.result()
    if (kept.isEmpty) { Effect.NoOp }
    else if (kept.length == 1) { kept(0) }
    else { Effect.Batch(kept) }
  }
}
