package grit.mcp.google

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*

import grit.core.clock.Clock
import grit.mcp.client.Authorizer

/** One request to a Google REST API: a GET of `path` below `base` with `query`, or, with `json`,
  * a POST of it as the body; `api` names the API in a failure's words ("Drive").
  */
final case class GoogleRequest(
    api: String,
    base: URI,
    path: String,
    query: Vector[(String, String)] = Vector.empty,
    json: Option[ujson.Value] = None
)

/** Why a Google REST API answered nothing a reader reads: the status table its APIs share. */
enum GoogleFailure derives CanEqual {

  /** No token was got (`why`, the authorizer's), or the API refused the one sent (401). */
  case NoToken(why: String)

  /** 404, or 403 for a reason no other case names: the two are not told apart. */
  case Hidden

  /** 403 `SERVICE_DISABLED` or `accessNotConfigured`: the API is not enabled for the account's
    * project.
    */
  case Off

  /** 429, a 5xx, or 403 for a rate or quota limit: try again later. */
  case Busy(status: Int)

  /** No answer within the sender's timeout, or an I/O failure: `why`, never the request. */
  case Unreachable(why: String)

  /** Any other status, or a body not JSON where JSON was asked for: `why`, naming the API. */
  case Malformed(why: String)
}

/** Requests to Google's REST APIs with `authorizer`'s token at `clock`'s time, each timing out
  * after `timeout`. A status other than 200 is first given to `refine`, with the reasons its body
  * gives (`error.errors[].reason`, then `error.details[].reason`); when `refine` reads neither as
  * its API's own failure, [[GoogleFailure]]'s table does.
  */
final class GoogleRest[E](
    authorizer: Authorizer,
    clock: Clock,
    refine: (Int, Vector[String]) -> Option[E],
    timeout: FiniteDuration = GoogleRest.Timeout
) {

  private val http = HttpClient.newBuilder().connectTimeout(timeout.toJava).build()

  /** `read` over the answer's body when it is 200, open only within it; an I/O failure of
    * `read`'s is [[GoogleFailure.Unreachable]].
    */
  def streamed[A](request: GoogleRequest)(
      read: java.io.InputStream^ => A
  ): Either[GoogleFailure | E, A] =
    authorizer
      .bearer(clock)
      .left
      .map(miss => GoogleFailure.NoToken(miss.why))
      .flatMap { bearer =>
        val query = request.query
          .map((k, v) => s"$k=${URLEncoder.encode(v, UTF_8).replace("+", "%20")}")
          .mkString("&")
        val builder = HttpRequest
          .newBuilder(
            request.base.resolve(if (query.isEmpty) request.path else s"${request.path}?$query")
          )
          .timeout(timeout.toJava)
          .header("Authorization", s"Bearer ${bearer.value}")
        val sent = request.json match {
          case Some(body) =>
            builder
              .header("Content-Type", "application/json")
              .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
          case None => builder.GET()
        }
        try {
          val response = http.send(sent.build(), HttpResponse.BodyHandlers.ofInputStream())
          val in = response.body()
          try
            if (response.statusCode == 200) Right(read(in))
            else Left(failure(request.api, response.statusCode, GoogleRest.capped(in)))
          finally in.close()
        } catch {
          case _: HttpTimeoutException =>
            Left(GoogleFailure.Unreachable(s"no answer within $timeout"))
          // Its class alone: an I/O error's message may name the request.
          case e: IOException => Left(GoogleFailure.Unreachable(e.getClass.getSimpleName))
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            Left(GoogleFailure.Unreachable("interrupted"))
        }
      }

  /** The answer's first [[GoogleRest.MaxRead]] bytes as text; when cut there, its whole lines. */
  def text(request: GoogleRequest): Either[GoogleFailure | E, String] =
    streamed(request)(GoogleRest.capped)

  /** The answer as JSON. */
  def json(request: GoogleRequest): Either[GoogleFailure | E, ujson.Value] =
    text(request).flatMap(body =>
      scala.util
        .Try(ujson.read(body))
        .toOption
        .toRight(GoogleFailure.Malformed("an answer that is not JSON"))
    )

  private def failure(api: String, status: Int, body: String): GoogleFailure | E = {
    val reasons = scala.util.Try(ujson.read(body)).toOption.toVector.flatMap(GoogleRest.reasons)
    refine(status, reasons).getOrElse(status match {
      case 401 => GoogleFailure.NoToken(s"$api refused the token it was given")
      case 403 if reasons.exists(GoogleRest.Disabled) => GoogleFailure.Off
      case 403 if reasons.exists(GoogleRest.Limits) => GoogleFailure.Busy(status)
      case 403 | 404 => GoogleFailure.Hidden
      case s if s == 429 || s >= 500 => GoogleFailure.Busy(status)
      case s => GoogleFailure.Malformed(s"$api answered $s")
    })
  }
}

object GoogleRest {

  /** A sender for an API with no failures of its own. */
  def apply(authorizer: Authorizer, clock: Clock): GoogleRest[Nothing]^{clock} =
    new GoogleRest[Nothing](authorizer, clock, (_, _) => None)

  /** `url` as an API's base, or why not, naming `what`: not an absolute URL. */
  def base(what: String, url: String): Either[String, URI] =
    scala.util
      .Try(URI.create(url))
      .toOption
      .filter(_.isAbsolute)
      .toRight(s"$what $url is not an absolute URL")

  /** How long one request may take: 60 seconds. */
  val Timeout: FiniteDuration = 60.seconds

  /** The most bytes of an answer read as text, an error's included: 1 MiB. */
  val MaxRead: Int = 1024 * 1024

  /** The reasons an error answer gives, in order: the older form's (`error.errors[].reason`),
    * then the newer form's (`error.details[].reason`); none when it names none.
    */
  private[mcp] def reasons(answer: ujson.Value): Vector[String] = {
    val error = answer.objOpt.flatMap(_.get("error")).flatMap(_.objOpt)
    Vector("errors", "details").flatMap(key =>
      error
        .flatMap(_.get(key))
        .flatMap(_.arrOpt)
        .toVector
        .flatten
        .flatMap(_.objOpt.flatMap(_.get("reason")).flatMap(_.strOpt))
    )
  }

  /** The 403 reasons Google gives for a rate or quota limit. */
  private val Limits: Set[String] =
    Set("userRateLimitExceeded", "rateLimitExceeded", "dailyLimitExceeded", "quotaExceeded")

  /** The reasons Google gives for a 403 when the API asked is not enabled for the project: the
    * newer form's, and the older one's.
    */
  private val Disabled: Set[String] = Set("SERVICE_DISABLED", "accessNotConfigured")

  /** `in`'s first [[MaxRead]] bytes as text; when cut there, its whole lines. */
  private def capped(in: java.io.InputStream^): String = {
    val bytes = in.readNBytes(MaxRead)
    val text = new String(bytes, UTF_8)
    // A body cut at MaxRead ends partway through a line: keep its whole lines.
    if (bytes.length < MaxRead) text else text.take(text.lastIndexOf('\n') + 1)
  }
}
