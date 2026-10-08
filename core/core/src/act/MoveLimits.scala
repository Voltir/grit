package grit.core.act

/** How many asks and calls one run may make; a move past either is refused
  * ([[MoveError.OverLimit]]) and not made. Keeps are not limited: they spend nothing.
  */
final case class MoveLimits private (asks: Int, calls: Int)

object MoveLimits {

  /** These limits, or why not: either negative. */
  def of(asks: Int, calls: Int): Either[String, MoveLimits] =
    if (asks < 0 || calls < 0)
      Left(s"a run's limits are each at least zero: asks $asks, calls $calls")
    else Right(new MoveLimits(asks, calls))

  /** No asks and no calls. */
  val Zero: MoveLimits = new MoveLimits(0, 0)
}

/** What a move is: an ask, a call or a keep. A run's limits count asks and calls
  * ([[MoveLimits]]).
  */
enum MoveKind {
  case Ask, Call, Keep
}
