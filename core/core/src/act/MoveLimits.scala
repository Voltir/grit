package grit.core.act

/** How many moves of each limited kind one run may make; a move past its kind's count is
  * refused ([[MoveError.OverLimit]]) and not made. `asks` counts asks a model answers (a JSON
  * ask's repairs within it); `judgments`, judgments ([[Posed.Judgment]]); `calls`, calls. Keeps
  * are not limited: they spend nothing.
  */
final case class MoveLimits private (asks: Int, calls: Int, judgments: Int)

object MoveLimits {

  /** 10: a run's judgments, per ask it may make, when its job does not state them: about the
    * ratio of a model ask's cost to a classifier's.
    */
  val JudgmentsPerAsk: Int = 10

  /** These limits, with [[JudgmentsPerAsk]] judgments per ask (at most `Int.MaxValue`), or why
    * not: either negative.
    */
  def of(asks: Int, calls: Int): Either[String, MoveLimits] =
    of(asks, calls, (asks.max(0).toLong * JudgmentsPerAsk).min(Int.MaxValue.toLong).toInt)

  /** These limits, or why not: any negative. */
  def of(asks: Int, calls: Int, judgments: Int): Either[String, MoveLimits] =
    if (asks < 0 || calls < 0 || judgments < 0)
      Left(
        s"a run's limits are each at least zero: asks $asks, calls $calls, judgments $judgments"
      )
    else Right(new MoveLimits(asks, calls, judgments))

  /** No moves of any limited kind: zero asks, calls and judgments. */
  val Zero: MoveLimits = new MoveLimits(0, 0, 0)
}

/** What a move is: an ask a model answers (text or JSON), a judgment ([[Posed.Judgment]]), a
  * call or a keep. A run's limits count each but keeps ([[MoveLimits]]).
  */
enum MoveKind {
  case Ask, Judge, Call, Keep
}
