package grit.core.store

import grit.core.id.EntryId
import grit.core.identity.{Account, TestAccounts}

import utest.*

/** The contract every [[Principals]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos.
  */
abstract class PrincipalsContract extends TestSuite {

  /** A store with no account named. */
  protected def fresh(): Principals

  /** Records a new inbound entry written by `by` (named, or `local`), in the database
    * `principals` reads; its id.
    */
  protected def said(principals: Principals, by: Account): EntryId

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private val ana = TestAccounts.account("slack:T1/U1")

  val tests = Tests {
    test(
      "a named account's inbound entries are named; local's, and entries not inbound, are not"
    ) {
      val principals = fresh()
      transaction(principals.name(ana, "Ana Lima")) ==> Right(())
      val hers = said(principals, ana)
      val mine = said(principals, Account.Local)
      val none = EntryId("reply:nobody:0")
      transaction(principals.speakers(Vector(hers, mine, none))) ==>
        Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("naming an account again renames it, the name trimmed") {
      val principals = fresh()
      transaction(principals.name(ana, "Ana")) ==> Right(())
      val hers = said(principals, ana) // named as she is now, not as she was when she wrote it
      transaction(principals.name(ana, "  Ana Lima ")) ==> Right(())
      transaction(principals.speakers(Vector(hers))) ==> Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("a blank name, or local or grit, is refused") {
      val principals = fresh()
      transaction(principals.name(ana, "  ")) ==>
        Left(StoreError.Invalid("a person's name is not blank"))
      transaction(principals.name(Account.Local, "Nick")) ==>
        Left(StoreError.Invalid("local is never named"))
      transaction(principals.name(Account.Grit, "grit")) ==>
        Left(StoreError.Invalid("grit is never named"))
    }
  }
}
