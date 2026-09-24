package grit.core.context

import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{Db, StoreError}

/** Chooses what the model sees on a turn: a fresh window over the store, computed each
  * turn, never carried from the last one. Read-only: it holds no write capability.
  */
trait ContextAssembler {

  /** The window for `request.turn`, over entries recorded before that turn. */
  def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window]
}

/** What an assembler is asked to build a window for. */
final case class AssemblyRequest(turn: TurnRef)

/** The entries the model sees before the turn's own, in the order it sees them, and notes
  * on how they were chosen.
  */
final case class Window(entries: Vector[EntryId], notes: Vector[AssemblyNote] = Vector.empty)

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
