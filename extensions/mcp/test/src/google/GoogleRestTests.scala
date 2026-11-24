package grit.mcp.google

import java.net.{ServerSocket, URI}

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Credential, FakeTokenServer, KeyFiles}

import utest.*

/** [[GoogleRest]] against [[FakeGoogleServer]]: what it sends, and the status table it reads. */
object GoogleRestTests extends TestSuite {

  /** A failure of the test API's own, read before the shared table. */
  private enum Own derives CanEqual {
    case Refined
  }

  private def withFake[A](body: (FakeGoogleServer, GoogleRest[Nothing]^) => A): A = {
    val fake = FakeGoogleServer.start()
    try body(fake, GoogleRest(FakeGoogleServer.authorizer(), Clock.system()))
    finally fake.stop()
  }

  private def error(status: Int, key: String, reason: String): String =
    ujson.write(
      ujson.Obj(
        "error" -> ujson.Obj("code" -> status, key -> ujson.Arr(ujson.Obj("reason" -> reason)))
      )
    )

  private def at(
      fake: FakeGoogleServer,
      path: String,
      query: Vector[(String, String)] = Vector.empty
  ) =
    GoogleRequest("Test", fake.base, path, query)

  val tests = Tests {
    test("a GET carries the bearer and its query, a space as %20, and no body") {
      withFake { (fake, rest) =>
        fake.answering("files", 200, "found")
        rest.text(at(fake, "files", Vector("q" -> "a b+c", "n" -> "1"))) ==> Right("found")
        fake.requests ==> Vector(
          FakeGoogleServer.Received(
            method = "GET",
            path = "files",
            query = Some("q=a%20b%2Bc&n=1"),
            authorization = Some(s"Bearer ${FakeGoogleServer.Token}"),
            contentType = None,
            body = ""
          )
        )
      }
    }

    test("a request with JSON is a POST of it, typed as JSON, with no query when none is given") {
      withFake { (fake, rest) =>
        fake.answering("sites/x/query", 200, """{"rows":[]}""")
        val asked = ujson.Obj("rowLimit" -> 5)
        rest.json(at(fake, "sites/x/query").copy(json = Some(asked))) ==>
          Right(ujson.Obj("rows" -> ujson.Arr()))
        fake.requests.map(r => (r.method, r.query, r.contentType, r.body)) ==>
          Vector(("POST", None, Some("application/json"), """{"rowLimit":5}"""))
      }
    }

    test("an answer read as JSON that is not JSON is malformed") {
      withFake { (fake, rest) =>
        fake.answering("files", 200, "not json")
        rest.json(at(fake, "files")) ==> Left(GoogleFailure.Malformed("an answer that is not JSON"))
      }
    }

    test(
      "401 is no token; 403 for a disabled API is off; a rate limit, 429 and 5xx busy; " +
        "any other 403 and 404 hidden; any other status malformed, naming the API"
    ) {
      withFake { (fake, rest) =>
        def answered(status: Int, body: String) = {
          fake.answering("files", status, body)
          rest.text(at(fake, "files"))
        }
        Vector(
          answered(401, error(401, "errors", "authError")),
          answered(403, error(403, "details", "SERVICE_DISABLED")),
          answered(403, error(403, "errors", "accessNotConfigured")),
          answered(403, error(403, "errors", "userRateLimitExceeded")),
          answered(403, error(403, "errors", "rateLimitExceeded")),
          answered(403, error(403, "errors", "dailyLimitExceeded")),
          answered(403, error(403, "errors", "quotaExceeded")),
          answered(429, error(429, "errors", "rateLimitExceeded")),
          answered(503, error(503, "errors", "backendError")),
          answered(403, error(403, "errors", "forbidden")),
          answered(404, error(404, "errors", "notFound")),
          answered(418, ""),
          answered(204, "")
        ) ==> Vector(
          Left(GoogleFailure.NoToken("Test refused the token it was given")),
          Left(GoogleFailure.Off),
          Left(GoogleFailure.Off),
          Left(GoogleFailure.Busy(403)),
          Left(GoogleFailure.Busy(403)),
          Left(GoogleFailure.Busy(403)),
          Left(GoogleFailure.Busy(403)),
          Left(GoogleFailure.Busy(429)),
          Left(GoogleFailure.Busy(503)),
          Left(GoogleFailure.Hidden),
          Left(GoogleFailure.Hidden),
          Left(GoogleFailure.Malformed("Test answered 418")),
          Left(GoogleFailure.Malformed("Test answered 204"))
        )
      }
    }

    test("a refinement reads a status and its reasons before the shared table does") {
      val fake = FakeGoogleServer.start()
      try {
        val rest = new GoogleRest[Own](
          FakeGoogleServer.authorizer(),
          Clock.system(),
          (status, reasons) =>
            if (status == 403 && reasons.contains("rateLimitExceeded")) Some(Own.Refined) else None
        )
        fake.answering("limited", 403, error(403, "errors", "rateLimitExceeded"))
        fake.answering("gone", 404, error(404, "errors", "notFound"))
        (rest.text(at(fake, "limited")), rest.text(at(fake, "gone"))) ==>
          (Left(Own.Refined), Left(GoogleFailure.Hidden))
      } finally fake.stop()
    }

    test("a streamed answer is read whole past MaxRead") {
      withFake { (fake, rest) =>
        val line = "0123456789abcdef\n"
        fake.answering("media", 200, line * (GoogleRest.MaxRead / line.length + 10))
        rest.streamed(at(fake, "media"))(_.readAllBytes().length) ==>
          Right((GoogleRest.MaxRead / line.length + 10) * line.length)
      }
    }

    test("no answer within the timeout is unreachable, as is a port nothing listens on") {
      val silent = new ServerSocket(0)
      val closed = new ServerSocket(0)
      val closedPort = closed.getLocalPort
      closed.close()
      try {
        val rest = new GoogleRest[Nothing](
          FakeGoogleServer.authorizer(),
          Clock.system(),
          (_, _) => None,
          timeout = 200.millis
        )
        def on(port: Int) =
          rest.text(GoogleRequest("Test", URI.create(s"http://127.0.0.1:$port/api/"), "files"))
        (on(silent.getLocalPort), on(closedPort)) ==>
          (
            Left(GoogleFailure.Unreachable("no answer within 200 milliseconds")),
            Left(GoogleFailure.Unreachable("ConnectException"))
          )
      } finally silent.close()
    }

    test("a token not got is no token, with the authorizer's why") {
      val tokens = FakeTokenServer.start()
      val fake = FakeGoogleServer.start()
      try {
        val key = Variable("FAKE_GOOGLE_KEY")
        val authorizer = Credential
          .open(
            Credential.serviceAccount(key, Set("scope")),
            Map(Variable.value(key) -> tokens.keyFile()),
            KeyFiles.Local
          )
          .fold(r => throw new java.lang.AssertionError(r.message), identity)
        tokens.refuses()
        GoogleRest(authorizer, Clock.system()).text(at(fake, "files")) ==>
          Left(
            GoogleFailure.NoToken(
              s"${tokens.uri} answered HTTP 400: invalid_grant (Invalid JWT Signature.)"
            )
          )
        fake.requests ==> Vector()
      } finally {
        fake.stop()
        tokens.stop()
      }
    }
  }
}
