package grit.core.act

import grit.core.visibility.{Integrity, Label}

/** Why a move was not made. */
enum MoveError {

  /** The run made a move of this name already. */
  case Repeated(name: MoveName)

  /** The run reached its limit of `limit` moves of `kind`. */
  case OverLimit(kind: MoveKind, limit: Int)

  /** The acting's allowance did not admit an ask: the day's recorded spend reached its cap. */
  case Capped

  /** No catalog could be read, the provider or classifier failed after its retries, or the
    * reply did not read: `why`.
    */
  case Model(why: String)

  /** No model or classifier the ask could go to is cleared for `classification`, the label of
    * what it is made from: a deployment's declaration, so asking again changes nothing.
    */
  case Uncleared(classification: Label)

  /** What a keep was to write from is believed at `integrity`, less than
    * [[Integrity.Outside]]: nothing was written, and the move counts as made.
    */
  case Below(integrity: Integrity)

  /** The store failed, or a keep's body returned `Left`: `why`. Nothing the move wrote is kept,
    * and the move counts as made: making it again under its name is [[Repeated]].
    */
  case Store(why: String)

  /** On a rerun, the input of the move `name`, or of an earlier one, differs from what the run
    * recorded when it first made it: the run's code read something besides its `JobRun` and its
    * moves' results. Every later move is refused so too.
    */
  case Diverged(name: MoveName)

  /** The move makes the downgrade `node` (its name), which the run's program does not declare
    * in its `within`: the run's code, so making it again changes nothing.
    */
  case Undeclared(node: String)
}

object MoveError {

  /** Why a move was not made, as a run's reply says it: one clause, lower case, naming the
    * move or the limit when it has one.
    */
  def said(error: MoveError): String = error match {
    case Below(_) => "it came from somewhere not trusted enough to keep"
    case Repeated(name) => s"the move ${MoveName.value(name)} was made already"
    case OverLimit(kind, limit) => s"the run reached its limit of $limit ${plural(kind)}"
    case Capped => "the day's recorded spend reached its cap"
    case Model(why) => why
    case Uncleared(at) => s"no model is cleared for ${Label.shown(at)}"
    case Store(why) => why
    case Diverged(name) => s"the run's moves differ from its record at ${MoveName.value(name)}"
    case Undeclared(node) => s"the job's code does not allow the $node it used"
  }

  private def plural(kind: MoveKind): String = kind match {
    case MoveKind.Ask => "asks"
    case MoveKind.Judge => "judgments"
    case MoveKind.Call => "calls"
    case MoveKind.Keep => "keeps"
  }
}
