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
  */
final case class Tuning(
    horizon: FiniteDuration,
    recent: Int,
    lexical: Int,
    followsAt: Probability,
    windowTokens: Tokens,
    strandChars: Int
)

object Tuning {

  /** A week; 2 recent and 2 lexical; 0.6; 1,500 tokens; 800 characters. */
  val Default: Tuning = Tuning(
    7.days,
    2,
    2,
    Probability.clamped(0.6),
    Tokens(1_500),
    800
  )
}
