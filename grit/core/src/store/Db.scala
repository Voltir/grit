package grit.core.store

/** Capability to read the store outside any durable step: each call is a short read-only
  * transaction of its own, so a rerun reads again and may see newer rows.
  */
trait Db extends caps.SharedCapability {

  /** Runs `body` in a read-only transaction, which ends before this returns. */
  def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A]
}
