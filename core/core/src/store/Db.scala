package grit.core.store

import grit.core.visibility.{Label, Subject}

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

object Db {

  /** `db`, every read through which runs its body only when what its transaction knows is
    * classified at most `bound`; any other runs nothing and fails as [[Tx.within]] says.
    * [[Reads]] made from it ([[Db.as]]) keep the bound. What a step hands code that must read
    * no more than its calls were routed for.
    */
  def within(db: Db^, bound: Label): Db^ = new Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      db.read(subject)(Tx.within(bound).flatMap(_ => body))
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
