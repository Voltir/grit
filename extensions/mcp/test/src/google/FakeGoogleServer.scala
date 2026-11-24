package grit.mcp.google

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets.UTF_8

import grit.core.edge.Variable
import grit.mcp.client.{Authorizer, Credential, KeyFiles}

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** A Google REST API in this JVM, on a loopback port, below `/api/`: each path answered as
  * [[answering]] set it, 404 with no body until then, every request kept as [[Received]].
  */
final class FakeGoogleServer private () {

  // Test scaffolding: set by the test's thread and read by the server's, each under `lock`,
  // and no capability is reached through any of it.
  private val lock = new Object

  // Each var is read and written only inside `lock.synchronized`.
  @caps.unsafe.untrackedCaptures
  private var kept = Vector.empty[FakeGoogleServer.Received]

  @caps.unsafe.untrackedCaptures
  private var answers = Map.empty[String, (Int, Vector[Byte])]

  private val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.createContext("/api/", exchange => answer(exchange))
  http.start()

  /** The API's base, as a [[GoogleRequest]] takes it. */
  def base: URI = URI.create(s"http://127.0.0.1:${http.getAddress.getPort}/api/")

  /** Every request received, in order. */
  def requests: Vector[FakeGoogleServer.Received] = lock.synchronized(kept)

  /** Requests for `path` below the base are answered `status` with `body` from now. */
  def answering(path: String, status: Int, body: String): Unit =
    lock.synchronized {
      answers = answers.updated(path, (status, body.getBytes(UTF_8).toVector))
    }

  /** Stops answering. */
  def stop(): Unit = http.stop(0)

  private def answer(exchange: HttpExchange): Unit = {
    val path = exchange.getRequestURI.getRawPath.stripPrefix("/api/")
    val received = FakeGoogleServer.Received(
      method = exchange.getRequestMethod,
      path = path,
      query = Option(exchange.getRequestURI.getRawQuery),
      authorization = Option(exchange.getRequestHeaders.getFirst("Authorization")),
      contentType = Option(exchange.getRequestHeaders.getFirst("Content-Type")),
      body = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
    )
    val (status, bytes) = lock.synchronized {
      kept = kept :+ received
      answers.getOrElse(path, (404, Vector.empty[Byte]))
    }
    exchange.sendResponseHeaders(status, if (bytes.isEmpty) -1L else bytes.length.toLong)
    val out = exchange.getResponseBody
    bytes.foreach(b => out.write(b.toInt))
    exchange.close()
  }
}

object FakeGoogleServer {

  /** One request as received: its raw path below the base and raw query, as sent. */
  final case class Received(
      method: String,
      path: String,
      query: Option[String],
      authorization: Option[String],
      contentType: Option[String],
      body: String
  )

  /** The bearer [[authorizer]] carries. */
  val Token: String = "google-token"

  /** An authorizer carrying [[Token]]. */
  def authorizer(): Authorizer = {
    val token = Variable("FAKE_GOOGLE_TOKEN")
    Credential
      .open(Credential.static(token), Map(Variable.value(token) -> Token), KeyFiles.Local)
      .fold(r => throw new java.lang.AssertionError(r.message), identity)
  }

  def start(): FakeGoogleServer = new FakeGoogleServer()
}
