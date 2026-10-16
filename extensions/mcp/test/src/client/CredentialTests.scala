package grit.mcp.client

import grit.core.edge.{EdgeRefusal, Variable}
import grit.mcp.client.McpClientTests.Hands
import grit.mcp.wire.McpError

import utest.*

/** [[Credential]]s opened over the environment, and the [[Authorizer]]s they open as. */
object CredentialTests extends TestSuite {

  private val Key = Variable("DRIVE_SA_KEY")

  private val Scopes = Set("https://www.googleapis.com/auth/drive.readonly")

  /** A wall-clock reading in whole seconds. */
  private val Start = 1_800_000_000_000L

  /** Runs `body` with a fresh token endpoint, stopped after. */
  private def withTokens[A](body: FakeTokenServer => A): A = {
    val fake = FakeTokenServer.start()
    try body(fake)
    finally fake.stop()
  }

  /** The service account `key` opens as. */
  private def opened(key: String): Authorizer =
    Credential.open(Credential.serviceAccount(Key, Scopes), Map("DRIVE_SA_KEY" -> key)) match {
      case Right(a) => a
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  /** Why the service account `key` does not open, if it does not. */
  private def refusal(key: Option[String]): Option[EdgeRefusal] =
    Credential
      .open(Credential.serviceAccount(Key, Scopes), key.map("DRIVE_SA_KEY" -> _).toMap)
      .swap
      .toOption

  val tests = Tests {
    test(
      "a service account's token is got by a JWT its key signs (RS256; iss, scope, aud = " +
        "token_uri, exp ≤ iat + 1 h), kept, and got again only once less than Renew is left"
    ) {
      withTokens { fake =>
        val clock = new Hands(start = Start)
        val authorizer = opened(fake.key())
        authorizer.bearer(clock).map(_.value) ==> Right("token-1")
        val (header, claims) = fake.assertions match {
          case Vector(one) => one
          case other => throw new java.lang.AssertionError(s"${other.size} assertions")
        }
        header("alg").str ==> "RS256"
        (
          claims("iss").str,
          claims("scope").str,
          claims("aud").str,
          claims("iat").num.toLong
        ) ==> (FakeTokenServer.Email, Scopes.head, fake.uri, Start / 1000)
        assert(claims("exp").num.toLong - claims("iat").num.toLong <= 3600L)
        // The token lasts 3600 s: kept while Renew or more of it is left.
        clock.at = Start + 3600_000L - Credential.Renew.toMillis
        authorizer.bearer(clock).map(_.value) ==> Right("token-1")
        fake.assertions.size ==> 1
        clock.at += 1000
        authorizer.bearer(clock).map(_.value) ==> Right("token-2")
        fake.assertions.lift(1).map(_._2("iat").num.toLong) ==> Some(clock.at / 1000)
      }
    }

    test("a token endpoint's refusal is NoToken naming its URL; the next bearer asks again") {
      withTokens { fake =>
        val clock = new Hands(start = Start)
        val authorizer = opened(fake.key())
        fake.refuses()
        authorizer.bearer(clock) ==>
          Left(
            McpError.NoToken(
              s"${fake.uri} answered HTTP 400: invalid_grant (Invalid JWT Signature.)"
            )
          )
        fake.refuses(on = false)
        authorizer.bearer(clock).map(_.value) ==> Right("token-1")
      }
    }

    test("a malformed key is refused naming the variable, never its value") {
      withTokens { fake =>
        val good = ujson.read(fake.key()).obj.toVector
        def without(field: String) = Some(ujson.write(ujson.Obj.from(good.filter(_._1 != field))))
        def changed(field: String, value: String) =
          Some(
            ujson.write(ujson.Obj.from(good.filter(_._1 != field) :+ (field -> ujson.Str(value))))
          )
        def malformed(why: String) = Some(EdgeRefusal.Malformed(Key, why))
        refusal(None) ==> Some(EdgeRefusal.Missing(Key))
        refusal(Some("  ")) ==> malformed("it is blank")
        refusal(Some("not json")) ==> malformed("it is not a service account's JSON key")
        refusal(without("client_email")) ==> malformed("its JSON names no client_email")
        refusal(without("private_key")) ==> malformed("its JSON names no private_key")
        refusal(without("token_uri")) ==> malformed("its JSON names no token_uri")
        refusal(
          changed("private_key", "-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----")
        ) ==> malformed("its private_key is not an RSA key in PKCS#8 PEM")
        refusal(changed("token_uri", "http://oauth2.example.com/token")) ==>
          malformed("its token_uri is not an absolute https URL (http only on a loopback host)")
      }
    }

    test("a static credential's bearer is its variable's value, as Bearer.of reads it") {
      val token = Variable("GITHUB_MCP_TOKEN")
      Credential.static(token).needs ==> Vector(token)
      Credential.serviceAccount(Key, Scopes).needs ==> Vector(Key)
      Credential
        .open(Credential.static(token), Map("GITHUB_MCP_TOKEN" -> "github_pat_x1"))
        .map(_.bearer(new Hands()).map(_.value)) ==> Right(Right("github_pat_x1"))
      Credential.open(Credential.static(token), Map.empty).map(_ => ()) ==>
        Left(EdgeRefusal.Missing(token))
    }
  }
}
