package grit.models

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.core.message.Message
import grit.core.provider.ProviderError

/** A model call over HTTP, as every [[grit.core.provider.Provider]] here makes it: one JSON
  * POST, no retries, its response read whole or as server-sent events. A failure to connect, a
  * timeout or a connection lost is [[ProviderError.Unavailable]]; anything else thrown while
  * sending or reading is [[ProviderError.Refused]], its message never holding the request.
  */
private[models] object ProviderHttp {

  /** Whether a request answered with HTTP `status` may succeed if sent again: 408, 429 and
    * every 5xx.
    */
  def transient(status: Int): Boolean = status == 408 || status == 429 || status / 100 == 5

  /** A non-200 response's error, `HTTP <status>: ` then its body's `error` object as `describe`
    * writes it, else the body trimmed to 200 characters; `Unavailable` when [[transient]].
    */
  def httpError(
      status: Int,
      body: String,
      describe: collection.Map[String, ujson.Value] => String
  ): ProviderError = {
    val detail = scala.util
      .Try(ujson.read(body))
      .toOption
      .flatMap(_.objOpt)
      .flatMap(_.get("error"))
      .flatMap(_.objOpt)
      .map(describe)
      .getOrElse(body.trim.take(200))
    val cause = s"HTTP $status: $detail"
    if (transient(status)) ProviderError.Unavailable(cause) else ProviderError.Refused(cause)
  }

  /** `unreadable response: <why>`. */
  def unreadable(why: String): ProviderError = ProviderError.Refused(s"unreadable response: $why")

  /** `body` POSTed to `endpoint` as JSON with `headers` besides its content type; the response
    * must begin within `timeout`.
    */
  def post(
      endpoint: URI,
      timeout: Duration,
      headers: Seq[(String, String)],
      body: ujson.Value
  ): HttpRequest =
    headers
      .foldLeft(HttpRequest.newBuilder(endpoint).timeout(timeout)) { case (b, (name, value)) =>
        b.header(name, value)
      }
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
      .build()

  /** `request` sent on `http`: a 200's body as `read` reads it, any other status as `error`
    * classes it with the body's text.
    */
  def complete(
      http: HttpClient,
      request: HttpRequest,
      read: ujson.Value => Either[ProviderError, Message.Assistant],
      error: (Int, String) => ProviderError
  ): Either[ProviderError, Message.Assistant] =
    guarded {
      val response = http.send(request, HttpResponse.BodyHandlers.ofString())
      if (response.statusCode == 200) read(ujson.read(response.body))
      else Left(error(response.statusCode, response.body))
    }

  /** `request` sent on `http`, its 200 response's lines folded by `fold` as they arrive into the
    * body `read` reads; any other status as `error` classes it with the body's text.
    */
  def stream(
      http: HttpClient,
      request: HttpRequest,
      fold: Iterator[String] => Either[ProviderError, ujson.Value],
      read: ujson.Value => Either[ProviderError, Message.Assistant],
      error: (Int, String) => ProviderError
  ): Either[ProviderError, Message.Assistant] =
    guarded {
      val response = http.send(request, HttpResponse.BodyHandlers.ofLines())
      val lines = response.body
      try {
        if (response.statusCode == 200) fold(lines.iterator().asScala).flatMap(read)
        else Left(error(response.statusCode, lines.iterator().asScala.mkString("\n")))
      } finally lines.close()
    }

  private def guarded(
      call: => Either[ProviderError, Message.Assistant]
  ): Either[ProviderError, Message.Assistant] =
    try call
    catch {
      // The message never includes the request, so never the key.
      case e: java.io.IOException =>
        Left(ProviderError.Unavailable(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
      case NonFatal(e) =>
        Left(ProviderError.Refused(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }
}
