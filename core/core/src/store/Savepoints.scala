package grit.core.store

/** Part of a transaction kept only when it succeeds. */
trait Savepoints {

  /** What `body` returns, run in this transaction. When that is a `Left`, nothing `body` wrote
    * is kept, a statement that failed under it included, and the transaction goes on as if
    * `body` had not run: what it writes after commits.
    */
  def atomic[A](body: Tx^ ?=> Either[StoreError, A])(using Tx^): Either[StoreError, A]
}
