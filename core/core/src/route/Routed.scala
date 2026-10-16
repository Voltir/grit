package grit.core.route

import grit.core.model.Pins
import grit.core.store.{Db, Reads, StoreError, Tx}
import grit.core.visibility.{Classification, Clearances, Party, Subject}

/** What a read was routed for (ADR 0031): the classification of what its transaction knew, and
  * the deployment's clearances it read under. Made only from a transaction ([[Routed.of]]), so
  * its classification is some transaction's floor; pure, so a site carries it past the read.
  */
final case class Routed private (classification: Classification, clearances: Clearances)
    extends caps.Pure {

  /** `pins` whose pair is cleared for this, in order ([[Routing.cleared]]); `None` when none is. */
  def cleared(pins: Pins): Option[Pins] = Routing.cleared(pins, clearances, classification)

  /** Whether what was read under this may be sent to `party`
    * ([[grit.core.visibility.Clearances.clears]]).
    */
  def clears(party: Party): Boolean = clearances.clears(party, classification)

  /** `db`, every read through it bounded by this classification ([[Db.within]]): what a site
    * reads what it sends through, once it chose whom to send it to under this.
    */
  def bound(db: Db^): Db^ = Db.within(db, classification.label)

  /** `reads`, every read through it bounded by this classification, as [[bound]] bounds a
    * `Db`.
    */
  def bound(reads: Reads^): Reads^ = {
    val label = classification.label
    new Reads {
      def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        reads.read(Tx.within(label).flatMap(_ => body))
    }
  }
}

object Routed {

  /** What this transaction knows is classified at, and the clearances it reads under. */
  def of(using tx: Tx^): Routed = new Routed(Tx.classification, Tx.visibility(tx).clearances)

  /** `body`, handed what one read for `subject` through `db` was routed for, and reads for
    * `subject` bounded by it ([[bound]]); `Left` when that first read fails. The routing read
    * and each of the body's reads are transactions of their own. What a site that reads what
    * it sends calls.
    */
  def reading[A](db: Db^, subject: Subject)(body: (Routed, Reads^) => A): Either[StoreError, A] =
    db.read(subject)(Right(of)).map(r => body(r, r.bound(db.as(subject))))

  /** `body`, handed what one read through `reads` was routed for, and `reads` bounded by it
    * ([[bound]]); `Left` when that first read fails. As the other `reading`, for a site handed
    * one subject's reads.
    */
  def reading[A](reads: Reads^)(body: (Routed, Reads^) => A): Either[StoreError, A] =
    reads.read(Right(of)).map(r => body(r, r.bound(reads)))
}
