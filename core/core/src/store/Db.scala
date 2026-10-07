package grit.core.store

import grit.core.visibility.Subject

/** Capability to read the store outside any durable step: each call is a short read-only
  * transaction of its own, so a rerun reads again and may see newer rows.
  */
trait Db extends caps.SharedCapability {

  /** Runs `body` in a read-only transaction opened for `subject`, which ends before this
    * returns.
    */
  def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A]

  /** Reads for `subject` alone: what to hand code that must not choose whom it reads for. */
  final def as(subject: Subject): Reads^ = {
    val db: Db^{this} = this
    new Reads {
      def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        db.read(subject)(body)
    }
  }
}

/** Capability to read the store for one subject, fixed when it was made ([[Db.as]]): each call
  * is a short read-only transaction of its own.
  */
trait Reads extends caps.SharedCapability {

  /** Runs `body` in a read-only transaction for its subject; inside, [[Tx.cleared]] says which
    * variants it may read.
    */
  def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A]
}
