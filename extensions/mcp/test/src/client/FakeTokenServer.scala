package grit.mcp.client

import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyPair, KeyPairGenerator, Signature}
import java.util.Base64

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** A service account's token endpoint in this JVM, on a loopback port, checking what Google's
  * documents of it (developers.google.com/identity/protocols/oauth2/service-account, "Making
  * the access token request"): a form POST whose `grant_type` is
  * `urn:ietf:params:oauth:grant-type:jwt-bearer` and whose `assertion` is a JWT signed RS256
  * by `keys`' private key. It answers a good one with `{access_token, expires_in, token_type}`,
  * the token `token-<n>` for the n-th it issues, lasting [[lasting]] seconds; anything else,
  * or every request after [[refuses]], with `400 {"error": "invalid_grant", ...}`. Each good
  * assertion's header and claims are kept, in [[assertions]].
  */
final class FakeTokenServer private (val keys: KeyPair) {

  // Test scaffolding: set by the test's thread and read by the server's, each under `lock`,
  // and no capability is reached through any of it.
  private val lock = new Object

  // Each var is read and written only inside `lock.synchronized`.
  @caps.unsafe.untrackedCaptures
  private var kept = Vector.empty[(ujson.Obj, ujson.Obj)]

  @caps.unsafe.untrackedCaptures
  private var seconds = 3600L

  @caps.unsafe.untrackedCaptures
  private var refusing = false

  private val http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.createContext("/token", exchange => answer(exchange))
  http.start()

  /** The URL it answers at. */
  def uri: String = s"http://127.0.0.1:${http.getAddress.getPort}/token"

  /** Each token it issues from now lasts `s` seconds. */
  def lasting(s: Long): Unit = lock.synchronized { seconds = s }

  /** Every request from now is refused, or, with `on` false, answered again. */
  def refuses(on: Boolean = true): Unit = lock.synchronized { refusing = on }

  /** Each good assertion's header and claims, in the order sent. */
  def assertions: Vector[(ujson.Obj, ujson.Obj)] = lock.synchronized(kept)

  /** Stops answering. */
  def stop(): Unit = http.stop(0)

  /** A service account's JSON key for `keys`, its token URI this server's. */
  def key(email: String = FakeTokenServer.Email): String = {
    val pem =
      Base64.getMimeEncoder(64, "\n".getBytes(UTF_8)).encodeToString(keys.getPrivate.getEncoded)
    ujson.write(
      ujson.Obj(
        "type" -> "service_account",
        "client_email" -> email,
        "private_key" -> s"-----BEGIN PRIVATE KEY-----\n$pem\n-----END PRIVATE KEY-----\n",
        "token_uri" -> uri
      )
    )
  }

  private def answer(exchange: HttpExchange): Unit = {
    val body = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
    val form = body
      .split('&')
      .toVector
      .map(_.split("=", 2).toVector)
      .collect { case Vector(k, v) => k -> URLDecoder.decode(v, UTF_8) }
      .toMap
    val formed = exchange.getRequestMethod == "POST" &&
      Option(exchange.getRequestHeaders.getFirst("Content-Type"))
        .exists(_.startsWith("application/x-www-form-urlencoded")) &&
      form.get("grant_type").contains("urn:ietf:params:oauth:grant-type:jwt-bearer")
    val read = form.get("assertion").filter(_ => formed).flatMap(verified)
    lock.synchronized {
      read match {
        case Some(parts) if !refusing =>
          kept = kept :+ parts
          val reply = ujson.Obj(
            "access_token" -> s"token-${kept.size}",
            "expires_in" -> ujson.Num(seconds.toDouble),
            "token_type" -> "Bearer"
          )
          send(exchange, 200, ujson.write(reply))
        case _ =>
          send(
            exchange,
            400,
            """{"error":"invalid_grant","error_description":"Invalid JWT Signature."}"""
          )
      }
    }
  }

  /** `jwt`'s header and claims, when `keys` signed it RS256. */
  private def verified(jwt: String): Option[(ujson.Obj, ujson.Obj)] =
    jwt.split('.').toVector match {
      case Vector(h, c, s) =>
        val check = Signature.getInstance("SHA256withRSA")
        check.initVerify(keys.getPublic)
        check.update(s"$h.$c".getBytes(UTF_8))
        val decode = Base64.getUrlDecoder
        scala.util
          .Try(check.verify(decode.decode(s)))
          .toOption
          .filter(identity)
          .flatMap { _ =>
            scala.util.Try {
              (
                ujson.read(decode.decode(h)).obj,
                ujson.read(decode.decode(c)).obj
              )
            }.toOption
          }
          .map((header, claims) => (ujson.Obj.from(header), ujson.Obj.from(claims)))
      case _ => None
    }

  private def send(exchange: HttpExchange, status: Int, body: String): Unit = {
    val bytes = body.getBytes(UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "application/json")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    exchange.getResponseBody.write(bytes)
    exchange.close()
  }
}

object FakeTokenServer {

  /** The service account's address [[FakeTokenServer.key]] names by default. */
  val Email: String = "reader@example.iam.gserviceaccount.com"

  /** A fake holding a fresh 2048-bit RSA key pair. */
  def start(): FakeTokenServer = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(2048)
    new FakeTokenServer(generator.generateKeyPair())
  }
}
