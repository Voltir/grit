package grit.core.store

/** Capability to write the store from inside a durable step, where no [[Durable.transact]]
  * is at hand: each call is a short read-write transaction of its own, committed before it
  * returns, or rolled back when `body` is a `Left` or the database fails. Not atomic with
  * the step: a step cut short by a crash has committed what it wrote so far, so what it
  * writes is keyed by ids fixed before the step, and a rerun reads them back.
  */
trait Jot extends caps.SharedCapability {

  /** Runs `body` in a read-write transaction, committed when it returns a `Right`. */
  def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A]
}
