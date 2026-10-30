package grit.core.store

import grit.core.visibility.{Label, Subject}

/** Capability to write the store from inside a durable step, where no [[Durable.transact]]
  * is at hand: each call is a short read-write transaction of its own, committed before it
  * returns, or rolled back when `body` is a `Left` or the database fails. Not atomic with
  * the step: a step cut short by a crash has committed what it wrote so far, so what it
  * writes is keyed by ids fixed before the step, and a rerun reads them back.
  */
trait Jot extends caps.SharedCapability {

  /** Runs `body` in a read-write transaction opened for `subject`, committed when it returns a
    * `Right`.
    */
  def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A]

  /** Writes for `subject` alone: what to hand code that must not choose whom it writes for. */
  final def as(subject: Subject): Keeps^ = {
    val jot: Jot^{this} = this
    new Keeps {
      def write[C, D](
          body: (Tx^) ?=> Either[StoreError, Keeps.Verdict[C, D]]
      ): Either[StoreError, Keeps.Verdict[C, D]] = {
        // A scoped local: the verdict of a body that discards, carried out of the `Left` that
        // makes `jot` roll its transaction back.
        var undone: Option[Keeps.Verdict[C, D]] = None
        jot.write(subject)(body.flatMap {
          case kept @ Keeps.Verdict.Commit(_) => Right(kept)
          case discarded @ Keeps.Verdict.Discard(_) =>
            undone = Some(discarded)
            Left(StoreError.Invalid("discarded"))
        }) match {
          case Left(error) => undone.toRight(error)
          case committed => committed
        }
      }
    }
  }
}

object Jot {

  /** `jot`, every write through which runs its body only when what its transaction knows is
    * classified at most `bound`; any other writes nothing and fails as [[Tx.within]] says.
    * [[Keeps]] made from it ([[Jot.as]]) keep the bound. What a step hands code that must keep
    * no more than its calls were routed for.
    */
  def within(jot: Jot^, bound: Label): Jot^ = new Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      jot.write(subject)(Tx.within(bound).flatMap(_ => body))
  }
}

/** Capability to write the store for one subject, fixed when it was made ([[Jot.as]]), from
  * inside a durable step: each call is a read-write transaction of its own, not atomic with the
  * step ([[Jot]]).
  */
trait Keeps extends caps.SharedCapability {

  /** Runs `body` in a read-write transaction for its subject, and returns its verdict: committed
    * on [[Keeps.Verdict.Commit]]; rolled back on [[Keeps.Verdict.Discard]]; rolled back, and the
    * `Left`, when `body` gives one or the database fails.
    */
  def write[C, D](
      body: (Tx^) ?=> Either[StoreError, Keeps.Verdict[C, D]]
  ): Either[StoreError, Keeps.Verdict[C, D]]
}

object Keeps {

  /** What a body run by [[Keeps.write]] decides of its transaction, with what it made there. */
  enum Verdict[+C, +D] {

    /** Kept: its transaction is committed. */
    case Commit(kept: C)

    /** Undone: its transaction is rolled back, and nothing it wrote is kept. */
    case Discard(undone: D)
  }
}
