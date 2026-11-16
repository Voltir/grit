package grit.commands.caller

import grit.commands.machine.MachineName
import grit.core.edge.Asked
import grit.core.either.Got.got
import grit.core.id.Sha256
import grit.core.identity.{Realm, Standing, TestAccounts}
import grit.core.visibility.Integrity
import grit.edge.http.{Challenge, Request, Response, Status, Verb}

import utest.*

/** [[Callers]]: who a request is from, the owner whatever it is or the caller its credential names. */
object CallersTests extends TestSuite {

  private val realm = got(Realm.of("desk", "home"))
  private val desktop = got(MachineName.of("desktop"))
  private val owner = Caller(TestAccounts.account("desk:home/owner"), Integrity.Member, desktop)
  private val callers = Callers.owner(owner)

  private val own = owner.copy(believed = Integrity.Authority)
  private val laptop = owner.copy(machine = got(MachineName.of("laptop")))
  private val ownText = "own-0123456789abcdefghijklmnopqrstuvwxyz"
  private val agentText = "agent-0123456789abcdefghijklmnopqrstuvwxyz"
  private val laptopText = "laptop-0123456789abcdefghijklmnopqrstuvwxyz"
  private val credentialed = got(
    Callers.of(
      Callers.Token(Sha256.text(ownText), own),
      Callers.Token(Sha256.text(agentText), owner),
      Callers.Token(Sha256.text(laptopText), laptop)
    )
  )

  private def request(headers: (String, String)*): Request =
    got(Request.of(Verb.Post, "/command/x", headers.toVector, ""))

  val tests = Tests {
    test("a request with no credential is the owner's; one carrying any is refused") {
      (
        callers.from(request("Host" -> "localhost")),
        callers.from(request("Authorization" -> "Bearer t0ken"))
      ) ==> (Right(owner), Left(Challenge.Refused))
    }

    test("a credential's bearer is its caller, the scheme read in any case") {
      Vector(
        request("Authorization" -> s"Bearer $ownText"),
        request("authorization" -> s"bearer $agentText"),
        request("Authorization" -> s"BEARER $laptopText")
      ).map(credentialed.from) ==> Vector(Right(own), Right(owner), Right(laptop))
    }

    test(
      "no credential, two, another scheme or an empty one is asked for; a text no digest is that of is refused"
    ) {
      Vector(
        request("Host" -> "localhost"),
        request("Authorization" -> s"Bearer $ownText", "Authorization" -> s"Bearer $ownText"),
        request("Authorization" -> s"Basic $ownText"),
        request("Authorization" -> "Bearer "),
        request("Authorization" -> "Bearer unknown-0123456789abcdefghijklmnopqrstuvwxyz"),
        request("Authorization" -> s"Bearer ${ownText}x"),
        request("Authorization" -> s"Bearer ${ownText.dropRight(1)}")
      ).map(credentialed.from) ==> Vector(
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
      val digest = Sha256.text(ownText)
      (
        Callers.of(Callers.Token(digest, own), Callers.Token(digest, laptop)).map(_.accounts),
        credentialed.accounts,
        (callers.uncredentialed, credentialed.uncredentialed)
      ) ==> (
        Left(s"two credentials have the digest ${digest.hex}: give each its own"),
        Vector(owner.account),
        (true, false)
      )
    }

    test("the source says the owner is a full member of the realm, and no one else is") {
      val source = callers.source(realm)
      val other = TestAccounts.account("desk:home/other")
      (
        source.ask(owner.account),
        source.ask(other),
        source.all(realm),
        source.all(got(Realm.of("desk", "elsewhere")))
      ) ==> (
        Asked.Said(Standing.Full(None)),
        Asked.Said(Standing.Outside),
        Right(Map(owner.account -> Standing.Full(None))),
        Right(Map.empty)
      )
    }

    test("a request with no caller is 401, carrying its challenge and saying what to send") {
      Vector(Challenge.Asked, Challenge.Refused).map(Callers.refused) ==> Vector(
        Response.text(
          Status.Unauthorized(Challenge.Asked),
          "refused: this edge needs a credential: send Authorization: Bearer <your credential>"
        ),
        Response.text(
          Status.Unauthorized(Challenge.Refused),
          "refused: the credential sent is not one this edge knows"
        )
      )
    }
  }
}
