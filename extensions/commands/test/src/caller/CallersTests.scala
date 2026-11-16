package grit.commands.caller

import grit.commands.routes.RouteFixtures.{
  AgentText,
  Away,
  AwayText,
  Credentialed,
  Own,
  OwnText,
  Owner,
  Owners
}
import grit.core.edge.Asked
import grit.core.either.Got.got
import grit.core.id.Sha256
import grit.core.identity.{Realm, Standing, TestAccounts}
import grit.edge.http.{Challenge, Request, Verb}

import utest.*

/** [[Callers]]: who a request is from, the owner whatever it is or the caller its credential names. */
object CallersTests extends TestSuite {

  private val realm = got(Realm.of("slack", "T1"))

  private def request(headers: (String, String)*): Request =
    got(Request.of(Verb.Post, "/command/x", headers.toVector, ""))

  val tests = Tests {
    test("a request with no credential is the owner's; one carrying any is refused") {
      (
        Owners.from(request("Host" -> "localhost")),
        Owners.from(request("Authorization" -> "Bearer t0ken"))
      ) ==> (Right(Owner), Left(Challenge.Refused))
    }

    test("a credential's bearer is its caller, the scheme read in any case") {
      Vector(
        request("Authorization" -> s"Bearer $OwnText"),
        request("authorization" -> s"bearer $AgentText"),
        request("Authorization" -> s"BEARER $AwayText")
      ).map(Credentialed.from) ==> Vector(Right(Own), Right(Owner), Right(Away))
    }

    test(
      "no credential, two, another scheme or an empty one is asked for; a text no digest is that of is refused"
    ) {
      Vector(
        request("Host" -> "localhost"),
        request("Authorization" -> s"Bearer $OwnText", "Authorization" -> s"Bearer $OwnText"),
        request("Authorization" -> s"Basic $OwnText"),
        request("Authorization" -> "Bearer "),
        request("Authorization" -> "Bearer unknown-0123456789abcdefghijklmnopqrstuvwxyz"),
        request("Authorization" -> s"Bearer ${OwnText}x"),
        request("Authorization" -> s"Bearer ${OwnText.dropRight(1)}")
      ).map(Credentialed.from) ==> Vector(
        Left(Challenge.Asked),
        Left(Challenge.Asked),
        Left(Challenge.Asked),
        Left(Challenge.Asked),
        Left(Challenge.Refused),
        Left(Challenge.Refused),
        Left(Challenge.Refused)
      )
    }

    test(
      "two credentials of one digest are refused; one account at two integrities and on two machines is one account"
    ) {
      val digest = Sha256.text(OwnText)
      (
        Callers.of(Callers.Token(digest, Own), Callers.Token(digest, Away)).map(_.accounts),
        Credentialed.accounts,
        (Owners.uncredentialed, Credentialed.uncredentialed)
      ) ==> (
        Left(s"two credentials have the digest ${digest.hex}: give each its own"),
        Vector(Owner.account),
        (true, false)
      )
    }

    test("the source says the owner is a full member of the realm, and no one else is") {
      val source = Owners.source(realm)
      val other = TestAccounts.account("slack:T1/U-other")
      (
        source.ask(Owner.account),
        source.ask(other),
        source.all(realm),
        source.all(got(Realm.of("slack", "T2")))
      ) ==> (
        Asked.Said(Standing.Full(None)),
        Asked.Said(Standing.Outside),
        Right(Map(Owner.account -> Standing.Full(None))),
        Right(Map.empty)
      )
    }
  }
}
