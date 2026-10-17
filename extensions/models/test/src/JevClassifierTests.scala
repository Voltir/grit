package grit.models

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Duration

import grit.core.classify.{Ask, ClassifierError, StateJson}

import com.sun.net.httpserver.HttpServer
import utest.*

/** What a call to Jev that gets no answer comes back as, over a loopback HTTP server. */
object JevClassifierTests extends TestSuite {

  private given StateJson[String] = StateJson.instance(t => ujson.Obj("text" -> t))

  private val urgent = Ask.yesNo[String]("Does this convey urgency?", None, None)

  private def jev(endpoint: URI): JevClassifier =
    new JevClassifier(JevConfig("test-key", "jev-test", endpoint, Duration.ofSeconds(5)))

  /** `body` with each path's status and response body served on a loopback port. */
  private def serving[A](routes: Map[String, (Int, String)])(body: Int => A): A = {
    val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    routes.foreach { case (path, (status, text)) =>
      http.createContext(
        path,
        exchange => {
          val bytes = text.getBytes(UTF_8)
          exchange.sendResponseHeaders(status, bytes.length.toLong)
          exchange.getResponseBody.write(bytes)
          exchange.close()
        }
      )
    }
    http.start()
    try body(http.getAddress.getPort)
    finally http.stop(0)
  }

  val tests = Tests {
    test("a 401 is Refused and a 503 Unavailable, each with its status and detail") {
      serving(
        Map(
          "/refuses" -> (401, """{"detail": "bad key"}"""),
          "/down" -> (503, """{"detail": "overloaded"}""")
        )
      ) { port =>
        def at(path: String) = jev(URI.create(s"http://127.0.0.1:$port$path"))
        (
          at("/refuses").ask("the build is on fire", urgent),
          at("/down").ask("the build is on fire", urgent)
        ) ==> (
          Left(ClassifierError.Refused("HTTP 401: bad key")),
          Left(ClassifierError.Unavailable("HTTP 503: overloaded"))
        )
      }
    }

    test("a connection to a closed port is Unavailable, naming the failure") {
      val closed = serving(Map.empty)(identity)
      jev(URI.create(s"http://127.0.0.1:$closed/v1/systemone"))
        .ask("the build is on fire", urgent)
        .left
        .map {
          case ClassifierError.Unavailable(cause) => Some(cause.takeWhile(_ != ':'))
          case _ => None
        } ==> Left(Some("ConnectException"))
    }
  }
}
