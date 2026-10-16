package grit.mcp.client

import grit.core.edge.{EdgeRefusal, Variable}
import grit.core.identity.Email
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

  /** The service account whose key file is at `path` opens as. */
  private def opened(path: String): Authorizer =
    Credential.open(
      Credential.serviceAccount(Key, Scopes),
      Map("DRIVE_SA_KEY" -> path),
      KeyFiles.Local
    ) match {
      case Right(a) => a
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  /** Why the service account does not open with its variable set to `value`, if it does not. */
  private def refusal(value: Option[String]): Option[EdgeRefusal] =
    Credential
      .open(
        Credential.serviceAccount(Key, Scopes),
        value.map("DRIVE_SA_KEY" -> _).toMap,
        KeyFiles.Local
      )
      .swap
      .toOption

  val tests = Tests {
    test(
      "a service account's token is got by a JWT its key signs (RS256; iss, scope, aud = " +
        "token_uri, exp ≤ iat + 1 h), kept, and got again only once less than Renew is left"
    ) {
      withTokens { fake =>
        val clock = new Hands(start = Start)
        val authorizer = opened(fake.keyFile())
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
        val authorizer = opened(fake.keyFile())
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

    test(
      "a key file unset, not at an absolute path, missing, unreadable or malformed is refused " +
        "naming the variable, never the file's contents"
    ) {
      withTokens { fake =>
        val good = ujson.read(fake.key()).obj.toVector
        def file(json: String) = Some(FakeTokenServer.written(json))
        def without(field: String) = file(ujson.write(ujson.Obj.from(good.filter(_._1 != field))))
        def changed(field: String, value: String) =
          file(
            ujson.write(ujson.Obj.from(good.filter(_._1 != field) :+ (field -> ujson.Str(value))))
          )
        def malformed(why: String) = Some(EdgeRefusal.Malformed(Key, why))
        val missing = java.nio.file.Files.createTempDirectory("grit-key-").resolve("key.json")
        val directory = missing.getParent.toString
        refusal(None) ==> Some(EdgeRefusal.Missing(Key))
        refusal(Some("  ")) ==> malformed("it is blank")
        refusal(Some("key.json")) ==> malformed("it is not an absolute path (~ is not expanded)")
        refusal(Some("~/key.json")) ==> malformed("it is not an absolute path (~ is not expanded)")
        refusal(Some(missing.toString)) ==> malformed("no file is at its path")
        refusal(Some(directory)) ==> malformed("its file cannot be read (IOException)")
        // A key's JSON in the variable itself is no path.
        refusal(Some(fake.key())) ==> malformed("it is not an absolute path (~ is not expanded)")
        refusal(file("not json")) ==> malformed("its file is not a service account's JSON key")
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
        .open(Credential.static(token), Map("GITHUB_MCP_TOKEN" -> "github_pat_x1"), KeyFiles.Local)
        .map(_.bearer(new Hands()).map(_.value)) ==> Right(Right("github_pat_x1"))
      Credential.open(Credential.static(token), Map.empty, KeyFiles.Local).map(_ => ()) ==>
        Left(EdgeRefusal.Missing(token))
    }

    test(
      "a service account's address is its key's client_email, lowercased; a static token has none"
    ) {
      withTokens { fake =>
        val token = Variable("GITHUB_MCP_TOKEN")
        (
          Credential.account(
            Credential.serviceAccount(Key, Scopes),
            Map("DRIVE_SA_KEY" -> fake.keyFile("Reader@Proj.iam.example")),
            KeyFiles.Local
          ),
          Credential.account(
            Credential.static(token),
            Map("GITHUB_MCP_TOKEN" -> "github_pat_x1"),
            KeyFiles.Local
          ),
          Credential.account(Credential.serviceAccount(Key, Scopes), Map.empty, KeyFiles.Local)
        ) ==> (
          Email.of("reader@proj.iam.example").map(Some(_)),
          Right(None),
          Left(EdgeRefusal.Missing(Key))
        )
      }
    }
  }
}
