package grit.core

/** Chooses what the model sees on a turn: a fresh window over the store, computed each
  * turn, never carried from the last one. Read-only: it holds no write capability.
  */
trait ContextAssembler {

  /** The window for `request.turn`, over entries recorded before that turn. */
  def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window]
}

/** What an assembler is asked to build a window for. */
final case class AssemblyRequest(turn: TurnRef)

/** The entries the model sees before the turn's own, in the order it sees them. */
final case class Window(entries: Vector[EntryId])

/** An assembly that produced no window. */
enum AssemblyError {
  case Store(error: StoreError)
}
