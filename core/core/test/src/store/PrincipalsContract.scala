package grit.core.store

import java.time.ZoneId

import grit.core.id.{EntryId, Href, PrincipalId}
import grit.core.identity.{Account, TestAccounts}

import utest.*

/** The contract every [[Principals]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos.
  */
abstract class PrincipalsContract extends TestSuite {

  /** A store with no account named. */
  protected def fresh(): Principals

  /** Records a new inbound entry written by `by` (named, or `local`), with `link`, in the
    * database `principals` reads; its id.
    */
  protected def said(principals: Principals, by: Account, link: Option[Href]): EntryId

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private val ana = TestAccounts.account("slack:T1/U1")

  val tests = Tests {
    test(
      "a named account's inbound entries are named; local's, and entries not inbound, are not"
    ) {
      val principals = fresh()
      transaction(principals.name(ana, "Ana Lima")) ==> Right(())
      val hers = said(principals, ana, None)
      val mine = said(principals, Account.Local, None)
      val none = EntryId("reply:nobody:0")
      transaction(principals.speakers(Vector(hers, mine, none))) ==>
        Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("naming an account again renames it, the name trimmed") {
      val principals = fresh()
      transaction(principals.name(ana, "Ana")) ==> Right(())
      val hers =
        said(principals, ana, None) // named as she is now, not as she was when she wrote it
      transaction(principals.name(ana, "  Ana Lima ")) ==> Right(())
      transaction(principals.speakers(Vector(hers))) ==> Right(Speakers(Map(hers -> "Ana Lima")))
    }

    test("each inbound entry is read with the link it was recorded with, named or not") {
      val principals = fresh()
      transaction(principals.name(ana, "Ana Lima")) ==> Right(())
      val link = Href.of("https://acme.slack.com/archives/C1/p1712300000000100")
      val hers = said(principals, ana, link)
      val unnamed = said(principals, TestAccounts.account("slack:T1/U2"), link)
      val unlinked = said(principals, ana, None)
      val read = transaction(principals.speakers(Vector(hers, unnamed, unlinked)))
      read.map(s => Vector(hers, unnamed, unlinked).map(e => (s.of(e), s.link(e)))) ==>
        Right(Vector((Some("Ana Lima"), link), (None, link), (Some("Ana Lima"), None)))
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

    test(
      "an account's zone is kept and read back by the entries it wrote; a second report replaces it; local's and grit's are refused"
    ) {
      val principals = fresh()
      val (pacific, lisbon) = (ZoneId.of("America/Los_Angeles"), ZoneId.of("Europe/Lisbon"))
      val hers = said(principals, ana, None)
      val unplaced = said(principals, TestAccounts.account("slack:T1/U2"), None)
      transaction(principals.zoneOf(hers)) ==> Right(None)
      transaction(principals.zoned(ana, pacific)) ==> Right(())
      transaction(principals.zoned(ana, pacific)) ==> Right(())
      val none = EntryId("reply:nobody:0")
      transaction(Vector(hers, unplaced, none).map(e => principals.zoneOf(e))) ==>
        Vector(Right(Some(pacific)), Right(None), Right(None))
      transaction(principals.zoned(ana, lisbon)) ==> Right(())
      transaction(principals.zoneOf(hers)) ==> Right(Some(lisbon))
      transaction(principals.zoned(Account.Local, pacific)) ==>
        Left(StoreError.Invalid("local is placed by no source"))
      transaction(principals.zoned(Account.Grit, pacific)) ==>
        Left(StoreError.Invalid("grit is placed by no source"))
    }

    test(
      "an inbound entry's author is its account's person, with the account's name and zone; grit's, and entries not inbound, have none"
    ) {
      val principals = fresh()
      val lisbon = ZoneId.of("Europe/Lisbon")
      val bo = TestAccounts.account("slack:T1/U2")
      transaction(principals.name(ana, "Ana Lima")) ==> Right(())
      transaction(principals.zoned(ana, lisbon)) ==> Right(())
      val first = said(principals, ana, None)
      val second = said(principals, ana, None)
      val his = said(principals, bo, None)
      val mine = said(principals, Account.Local, None)
      val grits = said(principals, Account.Grit, None)
      val none = EntryId("reply:nobody:0")
      val authors = transaction(principals.authors(Vector(first, second, his, mine, grits, none)))
        .fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)
      authors.keySet ==> Set(first, second, his, mine)
      Vector(first, his, mine).map(e => authors.get(e).map(a => (a.name, a.zone))) ==>
        Vector(Some((Some("Ana Lima"), Some(lisbon))), Some((None, None)), Some((None, None)))
      authors.get(mine).map(_.person) ==> Some(PrincipalId.Local)
      assert(authors.get(first).map(_.person) == authors.get(second).map(_.person))
      assert(authors.get(first).map(_.person) != authors.get(his).map(_.person))
    }
  }
}
