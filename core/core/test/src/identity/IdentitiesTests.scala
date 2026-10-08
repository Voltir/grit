package grit.core.identity

import grit.core.id.EdgeName

import utest.*

object IdentitiesTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  private val nick = ok(Handle.of("nick"))
  private val ana = ok(Handle.of("ana"))
  private val slackNick = ok(Account.of("slack", "T1/U1"))
  private val slackAna = ok(Account.of("slack", "T1/U2"))
  private val t1 = ok(Realm.of("slack", "T1"))
  private val t2 = ok(Realm.of("slack", "T2"))
  private val t3 = ok(Realm.of("slack", "T3"))
  private val chat = EdgeName("chat")
  private val other = EdgeName("other")

  val tests = Tests {
    test("a handle is 1 to 32 lowercase letters, digits or -") {
      List("nick", "a", "ana-2", "x" * 32).map(Handle.of(_).isRight) ==>
        List(true, true, true, true)
      List("", "Nick", "a b", "a_b", "x" * 33).map(Handle.of(_).isRight) ==>
        List(false, false, false, false, false)
      Handle.of("Nick") ==> Left("a handle is 1 to 32 lowercase letters, digits or -: Nick")
    }

    test("people each with their own accounts, and realms each with one edge, are declared") {
      val people = Vector(
        DeclaredPerson(nick, Set(slackNick)),
        DeclaredPerson(ana, Set(slackAna))
      )
      val vouchers = Vector(Vouching(chat, t1), Vouching(other, t2))
      Identities.of(people, vouchers).map(i => (i.people, i.vouchers)) ==>
        Right((people, vouchers))
    }

    test("an account two people declare is refused, naming both") {
      val refused = Identities.of(
        Vector(DeclaredPerson(nick, Set(slackNick)), DeclaredPerson(ana, Set(slackAna, slackNick))),
        Vector.empty
      )
      refused ==> Left(IdentityRefusal.AccountTwice(slackNick, nick, ana))
      refused.left.map(_.message) ==> Left("slack:T1/U1 is declared as both nick and ana")
    }

    test("a handle two people take is refused") {
      val refused = Identities.of(
        Vector(DeclaredPerson(nick, Set(slackNick)), DeclaredPerson(nick, Set(slackAna))),
        Vector.empty
      )
      refused ==> Left(IdentityRefusal.HandleTwice(nick))
      refused.left.map(_.message) ==> Left("two people are declared as nick")
    }

    test("a person declared with no account is refused") {
      val refused = Identities.of(Vector(DeclaredPerson(nick, Set.empty)), Vector.empty)
      refused ==> Left(IdentityRefusal.NoAccount(nick))
      refused.left.map(_.message) ==> Left("nick is declared with no account")
    }

    test("local and grit, principals of their own, are never declared a person's") {
      List(Account.Local, Account.Grit).map(reserved =>
        Identities.of(Vector(DeclaredPerson(nick, Set(slackNick, reserved))), Vector.empty)
      ) ==> List(
        Left(IdentityRefusal.Reserved(Account.Local, nick)),
        Left(IdentityRefusal.Reserved(Account.Grit, nick))
      )
      IdentityRefusal.Reserved(Account.Grit, nick).message ==>
        "nick is declared holding grit, which is a principal of its own"
    }

    test("a realm two edges vouch for is refused, naming both") {
      val refused = Identities.of(Vector.empty, Vector(Vouching(chat, t1), Vouching(other, t1)))
      refused ==> Left(IdentityRefusal.RealmTwice(t1, chat, other))
      refused.left.map(_.message) ==> Left("both chat and other are trusted to vouch for slack:T1/")
    }

    test("an edge vouches for the realms it is named for, and an edge not named for none") {
      val declared = ok(
        Identities
          .of(Vector.empty, Vector(Vouching(chat, t1), Vouching(other, t3), Vouching(chat, t2)))
          .left
          .map(_.message)
      )
      declared.realms(chat) ==> Set(t1, t2)
      declared.realms(other) ==> Set(t3)
      declared.realms(EdgeName("unnamed")) ==> Set.empty
      Identities.Shipped.realms(chat) ==> Set.empty
    }
  }
}
