package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.interfaces.RSAPrivateKey
import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, Signature}
import java.time.Instant
import java.util.Base64
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, Variable}
import grit.mcp.wire.McpError

/** How a server's requests are authorised: where the secret is read, never the secret. */
sealed trait Credential {

  /** The environment variables [[Credential.open]] reads. */
  def needs: Vector[Variable]
}

object Credential {

  private final case class Static(token: Variable) extends Credential {
    def needs: Vector[Variable] = Vector(token)
  }

  private final case class ServiceAccount(key: Variable, scopes: Set[String]) extends Credential {
    def needs: Vector[Variable] = Vector(key)
  }

  /** `token`'s value, as it is ([[Bearer.of]]'s refusals). */
  def static(token: Variable): Credential = Static(token)

  /** A service account's access tokens for `scopes`: `key`'s value is its JSON key as Google
    * issues it (`client_email`, `private_key` in PKCS#8 PEM, `token_uri`, an `https` URL);
    * each token got at `token_uri` by a JWT the key signs (RS256, lasting [[Lifetime]]), kept,
    * and got again once less than [[Renew]] of it is left.
    */
  def serviceAccount(key: Variable, scopes: Set[String]): Credential = ServiceAccount(key, scopes)

  /** 5 minutes: how long before a token expires it is got again. */
  val Renew: FiniteDuration = 5.minutes

  /** 1 hour, the most Google allows: how long the JWT asking for a token lasts. */
  val Lifetime: FiniteDuration = 1.hour

  /** Opened over `env`, or why not: a variable unset or blank, a key not JSON naming those
    * fields, a `token_uri` not `https` (`http` only on a loopback host), or a PEM not an RSA
    * key; each naming the variable, never its value. A static token is refused as
    * [[Bearer.of]] says.
    */
  def open(credential: Credential, env: Map[String, String]): Either[EdgeRefusal, Authorizer] =
    credential match {
      case Static(token) => Bearer.of(env, token).map(b => new Authorizer(Right(b)))
      case ServiceAccount(key, scopes) =>
        env.get(Variable.value(key)) match {
          case None => Left(EdgeRefusal.Missing(key))
          case Some(v) if v.trim.isEmpty => Left(EdgeRefusal.Malformed(key, "it is blank"))
          case Some(v) =>
            Account
              .read(v, scopes.toVector.sorted.mkString(" "))
              .left
              .map(EdgeRefusal.Malformed(key, _))
              .map(a => new Authorizer(Left(a)))
        }
    }
}

/** A service account read from its key: `scope`, the scopes asked for, space-separated. */
private[client] final case class Account(
    email: String,
    key: RSAPrivateKey,
    tokenUri: URI,
    scope: String
)

private[client] object Account {

  /** `json` read as a service account's key, or why not, never quoting it. */
  def read(json: String, scope: String): Either[String, Account] = {
    def field(o: ujson.Obj, name: String): Either[String, String] =
      o.value
        .get(name)
        .flatMap(_.strOpt)
        .filter(_.trim.nonEmpty)
        .toRight(s"its JSON names no $name")
    for {
      o <- Try(ujson.read(json)).toOption
        .flatMap(_.objOpt)
        .map(ujson.Obj.from(_))
        .toRight("it is not a service account's JSON key")
      email <- field(o, "client_email")
      pem <- field(o, "private_key")
      uri <- field(o, "token_uri")
      tokenUri <- Try(new URI(uri)).toOption
        .filter(McpServer.secure)
        .toRight("its token_uri is not an absolute https URL (http only on a loopback host)")
      key <- rsa(pem).toRight("its private_key is not an RSA key in PKCS#8 PEM")
    } yield Account(email, key, tokenUri, scope)
  }

  private def rsa(pem: String): Option[RSAPrivateKey] = {
    val body = pem.linesIterator
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("-----"))
      .mkString
    Try(Base64.getDecoder.decode(body)).toOption.flatMap { der =>
      Try(KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(der))).toOption
        .collect { case k: RSAPrivateKey => k }
    }
  }
}

/** The bearer each request of one server carries, from the [[Credential]] it was opened by;
  * never shown.
  */
final class Authorizer private[client] (source: Either[Account, Bearer]) {

  private val http = HttpClient.newBuilder().connectTimeout(McpClient.Timeout.toJava).build()

  // The token last got, and the instant it expires. Replaced whole through the reference's own
  // atomic operations, so a reader sees one token or the other, each with more than Renew left
  // when it was kept; two callers that both find it near expiry both get one, and either is
  // valid, at the cost of one more exchange.
  @caps.unsafe.untrackedCaptures
  private val kept = new AtomicReference[Option[(Bearer, Instant)]](None)

  /** The bearer for the next request, at `clock`'s time: a kept token, or a new one when none
    * is kept or less than [[Credential.Renew]] of it is left; [[McpError.NoToken]] naming the
    * token URL when getting one fails, and the next call asks again. Blocks up to
    * [[McpClient.Timeout]].
    */
  def bearer(clock: Clock): Either[McpError, Bearer] = source match {
    case Right(fixed) => Right(fixed)
    case Left(account) =>
      val now = clock.now()
      kept.get() match {
        case Some((b, expires)) if !now.plus(Credential.Renew.toJava).isAfter(expires) =>
          Right(b)
        case _ =>
          exchange(account, now).map { (b, expires) =>
            kept.set(Some((b, expires)))
            b
          }
      }
  }

  /** A token for `account`, asked for at `now`, and when it expires. */
  private def exchange(account: Account, now: Instant): Either[McpError, (Bearer, Instant)] = {
    val url = account.tokenUri.toString
    val encode = Base64.getUrlEncoder.withoutPadding
    def part(o: ujson.Obj) = encode.encodeToString(ujson.write(o).getBytes(UTF_8))
    val iat = now.getEpochSecond
    val unsigned = part(ujson.Obj("alg" -> "RS256", "typ" -> "JWT")) + "." + part(
      ujson.Obj(
        "iss" -> account.email,
        "scope" -> account.scope,
        "aud" -> url,
        // ujson writes a Long as a string; NumericDate is a JSON number (RFC 7519 §2).
        "iat" -> ujson.Num(iat.toDouble),
        "exp" -> ujson.Num((iat + Credential.Lifetime.toSeconds).toDouble)
      )
    )
    val signer = Signature.getInstance("SHA256withRSA")
    signer.initSign(account.key)
    signer.update(unsigned.getBytes(UTF_8))
    val jwt = unsigned + "." + encode.encodeToString(signer.sign())
    val form = "grant_type=" + URLEncoder.encode(Authorizer.Grant, UTF_8) +
      "&assertion=" + URLEncoder.encode(jwt, UTF_8)
    val request = HttpRequest
      .newBuilder(account.tokenUri)
      .timeout(McpClient.Timeout.toJava)
      .header("Content-Type", "application/x-www-form-urlencoded")
      .POST(HttpRequest.BodyPublishers.ofString(form))
      .build()
    val answered =
      try Right(http.send(request, HttpResponse.BodyHandlers.ofString()))
      catch {
        case _: InterruptedException =>
          Thread.currentThread().interrupt()
          Left(s"$url was interrupted")
        // The message never includes the request, so never the assertion.
        case NonFatal(e) => Left(s"$url could not be reached: ${e.getClass.getSimpleName}")
      }
    answered
      .flatMap { response =>
        val body = Try(ujson.read(response.body())).toOption.flatMap(_.objOpt)
        def str(name: String) = body.flatMap(_.get(name)).flatMap(_.strOpt)
        if (response.statusCode != 200) {
          val said =
            str("error").fold("")(e => ": " + e + str("error_description").fold("")(d => s" ($d)"))
          Left(s"$url answered HTTP ${response.statusCode}$said")
        } else
          (for {
            token <- str("access_token").flatMap(Bearer.read(_).toOption)
            seconds <- body.flatMap(_.get("expires_in")).flatMap(_.numOpt).filter(_ > 0)
          } yield (token, now.plusSeconds(seconds.toLong)))
            .toRight(s"$url answered no access_token and expires_in")
      }
      .left
      .map(McpError.NoToken(_))
  }

  override def toString: String = "Authorizer(<redacted>)"
}

private object Authorizer {

  /** The OAuth grant a JWT bearer assertion is exchanged by. */
  val Grant = "urn:ietf:params:oauth:grant-type:jwt-bearer"
}
