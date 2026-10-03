package grit.core.triage

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.ClassifierError
import grit.core.id.{EntryId, ShadowName, TriageRef}
import grit.core.message.Usage
import grit.core.store.{StoreError, Tx}

/** What each declared shadow variant made of heard messages already triaged: recorded, never
  * acted on; kept beside the entry and deleted with it, so a period's raw purge takes them.
  */
trait TriageShadows {

  /** Keeps `row` for `entry` under `name`, made `at`; `false`, writing nothing, when the entry
    * is gone or that variant has shadowed it already.
    */
  def record(entry: EntryId, name: ShadowName, row: Shadowed, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** Up to `limit` heard messages tagged at or after `since` that `name` has not shadowed,
    * the oldest tagged first.
    */
  def unshadowed(name: ShadowName, since: Instant, limit: Int)(using
      Tx^
  ): Either[StoreError, Vector[TriageRef]]

  /** What `name`'s calls kept from `from` on cost, in USD; a call whose cost was not
    * reported counts as nothing.
    */
  def spent(name: ShadowName, from: Instant)(using Tx^): Either[StoreError, BigDecimal]

  /** The costs of `name`'s latest `n` answered calls that reported one, the newest first. */
  def recent(name: ShadowName, n: Int)(using Tx^): Either[StoreError, Vector[BigDecimal]]

  /** What `name` made of each of `entries` it has shadowed. */
  def of(name: ShadowName, entries: Vector[EntryId])(using
      Tx^
  ): Either[StoreError, Map[EntryId, Shadowed]]
}

/** What a shadow variant made of one heard message it asked about. */
enum Shadowed {

  /** Answered: the request's digest ([[grit.core.classify.Request.digest]]), its answers,
    * what the call consumed, the model `requested` and the one that `answered`, taking
    * `latency`.
    */
  case Answered(
      request: String,
      answers: ShadowAnswers,
      usage: Usage,
      requested: String,
      answered: String,
      latency: FiniteDuration
  )

  /** Asked, with no usable answer: the request's digest, which kind of failure, and how long
    * it took.
    */
  case Failed(request: String, failure: ClassifierError.Kind, latency: FiniteDuration)
}
