package grit.core.context

import grit.core.id.{EntrySeq, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{Db, Nearby, StoreError}

/** Chooses what the model sees on a turn: a fresh window over the store, computed each
  * turn, never carried from the last one. Read-only: it holds no write capability.
  */
trait ContextAssembler {

  /** The window for `request.turn`, over entries recorded before that turn. */
  def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window]
}

/** What an assembler is asked to build a window for. */
final case class AssemblyRequest(turn: TurnRef)

/** What the model sees before the turn's own messages: each of `nearby`'s sections, then the
  * entries of its own conversation at `entries`, in that order; and notes on how they were
  * chosen. A nearby entry gone by the time the model is called (its period closed and was
  * purged) is left out, and a section left with no message is dropped; an own entry gone
  * fails the turn.
  */
final case class Window(
    entries: Vector[EntrySeq],
    notes: Vector[AssemblyNote] = Vector.empty,
    nearby: Vector[Nearby] = Vector.empty
)

/** Something an assembler did while choosing a window, kept for the record and never shown
  * to the model.
  */
enum AssemblyNote {

  /** It asked `model` for a search query and got `query` back, at `usage`; `estimate` is the
    * estimated input of that request.
    */
  case Queried(query: String, model: String, usage: Usage, estimate: Tokens)

  /** It added these earlier `turns`, found by search, to the recent ones; none when
    * nothing it found fitted.
    */
  case Recalled(turns: Vector[TurnSeq])

  /** It built a simpler window than it set out to, for `reason`. */
  case FellBack(reason: String)
}

/** An assembly that produced no window. */
enum AssemblyError {
  case Store(error: StoreError)
}
