package grit.core.identity

import grit.core.id.AttesterName

import utest.*

object IdentitiesTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  private val t1 = ok(Realm.of("slack", "T1"))
  private val t2 = ok(Realm.of("slack", "T2"))
  private val t3 = ok(Realm.of("slack", "T3"))
  private val chat = AttesterName("chat")
  private val directory = AttesterName("directory")
  private val example = ok(Domain.of("example.com"))

  private def trusting(vouchings: Vouching*): Identities =
    ok(Identities.of(vouchings.toVector, Set(example)).left.map(_.message))

  val tests = Tests {
    test("realms each with one attester, and the domains claimed, are kept as given") {
      val vouchings = Vector(Vouching(chat, t1), Vouching(directory, t2))
      Identities.of(vouchings, Set(example)).map(i => (i.vouchings, i.domains)) ==>
        Right((vouchings, Set(example)))
    }

    test("no domain claimed is allowed: membership alone still counts") {
      Identities.of(Vector(Vouching(chat, t1)), Set.empty).map(_.domains) ==> Right(Set.empty)
    }

    test("a realm two attesters vouch for is refused, naming both") {
      val refused = Identities.of(Vector(Vouching(chat, t1), Vouching(directory, t1)), Set.empty)
      refused ==> Left(IdentityRefusal.RealmTwice(t1, chat, directory))
      refused.left.map(_.message) ==>
        Left("both chat and directory are trusted to attest slack:T1/")
    }

    test("an attester answers for the realms it is named for, and an attester not named for none") {
      val declared = trusting(Vouching(chat, t1), Vouching(directory, t3), Vouching(chat, t2))
      (
        declared.realmsOf(chat),
        declared.realmsOf(directory),
        declared.realmsOf(AttesterName("unnamed")),
        Identities.Shipped.realmsOf(chat)
      ) ==> (Set(t1, t2), Set(t3), Set.empty, Set.empty)
    }

    test("realms is every realm trusted, and attesters every attester named") {
      val declared = trusting(Vouching(chat, t1), Vouching(directory, t3), Vouching(chat, t2))
      (declared.realms, declared.attesters, Identities.Shipped.realms) ==>
        (Set(t1, t2, t3), Set(chat, directory), Set.empty)
    }

    test("trust names an attester, never an edge") {
      val error = assertCompileError("Vouching(grit.core.id.EdgeName.Slack, t1)")
      assert(error.msg.contains("EdgeName"))
    }
  }

}
