package grit.core.edge

import java.time.Instant

import grit.core.id.TurnRef
import grit.core.store.{StoreError, Tx}

/** The messages an edge marks as being answered while the turns answering them run, and how
  * far each mark has got: what lets an edge that restarts take down every mark it put up, and
  * put up none once its turn has ended.
  */
trait Acknowledgements {

  /** Marks `turn` as to be acknowledged at `to`, the edge's own address of the message it
    * answers (such as a Slack channel, thread and message), wanted `at`; nothing when `turn`
    * is already wanted, shown or cleared.
    */
  def want(turn: TurnRef, to: String, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Every acknowledgement not yet cleared, in the order it was wanted. */
  def standing()(using Tx^): Either[StoreError, Vector[Acknowledgement]]

  /** `turn`'s mark was put up `at`; nothing when `turn` was never wanted or is cleared. */
  def shown(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** `turn`'s mark was taken down `at`, or will never be put up: it is no longer standing.
    * Nothing when `turn` was never wanted or is cleared already.
    */
  def cleared(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit]
}

/** A standing acknowledgement: its turn, where its mark goes, and whether it is up. */
final case class Acknowledgement(turn: TurnRef, to: String, shown: Boolean)
