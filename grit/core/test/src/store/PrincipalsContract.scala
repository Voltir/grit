package grit.core.store

import grit.core.id.{EntryId, PrincipalId}

import utest.*

/** The contract every [[Principals]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos.
  */
abstract class PrincipalsContract extends TestSuite {

  /** A store with nobody enrolled. */
  protected def fresh(): Principals

  /** Records a new inbound entry written by `by` (enrolled, or `local`), in the database
    * `principals` reads; its id.
    */
  protected def said(principals: Principals, by: PrincipalId): EntryId

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private val ana = PrincipalId("slack:T1/U1")

  val tests = Tests {
    test(
      "an enrolled author's inbound entries are named; local's, and entries not inbound, are not"
    ) {
      val principals = fresh()
      transaction(principals.enroll(ana, "Ana Lima")) ==> Right(())
      val hers = said(principals, ana)
      val mine = said(principals, PrincipalId.Local)
      val none = EntryId("reply:nobody:0")
      transaction(principals.speakers(Vector(hers, mine, none))) ==>
        Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("enrolling again renames, the name trimmed") {
      val principals = fresh()
      transaction(principals.enroll(ana, "Ana")) ==> Right(())
      val hers = said(principals, ana) // named as she is now, not as she was when she wrote it
      transaction(principals.enroll(ana, "  Ana Lima ")) ==> Right(())
      transaction(principals.speakers(Vector(hers))) ==> Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("a blank name, or local or grit, is refused") {
      val principals = fresh()
      transaction(principals.enroll(ana, "  ")) ==>
        Left(StoreError.Invalid("a person's name is not blank"))
      transaction(principals.enroll(PrincipalId.Local, "Nick")) ==>
        Left(StoreError.Invalid("local is never enrolled"))
      transaction(principals.enroll(PrincipalId.Grit, "grit")) ==>
        Left(StoreError.Invalid("grit is never enrolled"))
    }
  }
}
