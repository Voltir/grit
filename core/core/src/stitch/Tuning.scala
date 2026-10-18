package grit.core.stitch

import scala.concurrent.duration.*

import grit.core.message.Tokens
import grit.core.period.Probability

/** How stitching is tuned; every placement keeps the tuning it was made under.
  *
  * @param horizon
  *   how far before a first message its room is searched for exchanges, and how far back a
  *   reader reads its strand
  * @param recent
  *   how many exchanges are offered because they were most recently spoken in
  * @param lexical
  *   how many more are offered for their best BM25 match with the message
  * @param followsAt
  *   the least probability at which a message follows an exchange
  * @param windowTokens
  *   the most a window's strand sections cost
  * @param strandChars
  *   the most of a strand triage's and the judge's thread show
  * @param burst
  *   a burst prior: when set, a message the classifier does not place in an exchange at
  *   `followsAt` follows the strand its author's previous opening in its room is in, when that
  *   opening was said at most the burst's gap before and that strand is given at least the
  *   burst's own least probability, whatever the classifier's top choice; `None` follows the
  *   top choice at `followsAt` alone
  */
final case class Tuning(
    horizon: FiniteDuration,
    recent: Int,
    lexical: Int,
    followsAt: Probability,
    windowTokens: Tokens,
    strandChars: Int,
    burst: Option[Tuning.Burst] = None
)

object Tuning {

  /** The strand a message's author opened a conversation of at most `gap` before it is
    * followed when given at least `followsAt`.
    */
  final case class Burst(followsAt: Probability, gap: FiniteDuration)

  /** A week; 2 recent and 2 lexical; 0.6; 1,500 tokens; 800 characters; a burst prior at 0.3
    * over 120 seconds.
    */
  val Default: Tuning = Tuning(
    7.days,
    2,
    2,
    Probability.clamped(0.6),
    Tokens(1_500),
    800,
    Some(Burst(Probability.clamped(0.3), 120.seconds))
  )
}
