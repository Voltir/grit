package grit.core.act

/** Why a move was not made. */
enum MoveError {

  /** The run made a move of this name already. */
  case Repeated(name: MoveName)

  /** The run reached its limit of `limit` moves of `kind`. */
  case OverLimit(kind: MoveKind, limit: Int)

  /** The acting's allowance did not admit an ask: the day's recorded spend reached its cap. */
  case Capped

  /** No catalog could be read, or the provider failed after its retries: `why`. */
  case Model(why: String)

  /** The store failed, or a keep's body returned `Left`: `why`. Nothing the move wrote is kept,
    * and the move counts as made: making it again under its name is [[Repeated]].
    */
  case Store(why: String)

  /** On a rerun, the input of the move `name`, or of an earlier one, differs from what the run
    * recorded when it first made it: the run's code read something besides its `JobRun` and its
    * moves' results. Every later move is refused so too.
    */
  case Diverged(name: MoveName)
}
