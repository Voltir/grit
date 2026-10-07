package grit.core.context

import grit.core.id.{DocumentVersion, EntrySeq, TurnRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{Db, Nearby, StoreError}

/** Chooses what the model sees on a turn: a fresh window over the store, computed each
  * turn, never carried from the last one. Read-only: it holds no write capability.
  */
trait ContextAssembler {

  /** The window for `request.turn`, read for that turn ([[grit.core.visibility.Subject.Turn]]),
    * over entries recorded before it.
    */
  def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window]
}

/** What an assembler is asked to build a window for, and how wide. */
final case class AssemblyRequest(turn: TurnRef, width: Width = Width.Deployed)

/** How wide a window is drawn. */
enum Width {

  /** As wide as the assembler was built to draw it. */
  case Deployed

  /** Within `budget` estimated tokens, wider or narrower than the assembler was built for, each
    * search ranking at most `hits` entries (none when it is not positive); an assembler that
    * does not search reads `budget` alone.
    */
  case Within(budget: Tokens, hits: Int)
}

/** What the model sees before the turn's own messages: each of `nearby`'s sections, then each
  * of `documents` under its plugin's label, then the entries of its own conversation at
  * `entries`, in that order; and notes on how they were chosen. A nearby entry, or a document,
  * gone by the time the model is called is left out, as is a document whose plugin is no
  * longer enabled, and a section left with no message is dropped; an own entry gone fails
  * the turn.
  */
final case class Window(
    entries: Vector[EntrySeq],
    notes: Vector[AssemblyNote] = Vector.empty,
    nearby: Vector[Nearby] = Vector.empty,
    documents: Vector[DocumentVersion] = Vector.empty
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
