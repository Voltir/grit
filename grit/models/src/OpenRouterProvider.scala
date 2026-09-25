package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.core.message.Message
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}

/** [[Provider]] over OpenRouter's chat completions, one request per call, streamed or
  * not, no retries (the durable turn records the outcome either way).
  */
final class OpenRouterProvider(config: OpenRouterConfig) extends Provider {

  private val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
    guarded {
      val response = http.send(
        post(OpenRouterJson.request(config.model, config.maxTokens, config.routing, request)),
        HttpResponse.BodyHandlers.ofString()
      )
      if (response.statusCode == 200) OpenRouterJson.response(ujson.read(response.body))
      else Left(OpenRouterJson.error(response.statusCode, response.body))
    }

  /** The response streamed ([[OpenRouterStream]]), read line by line as it arrives. The
    * timeout bounds the wait for the response to begin, not the whole of it.
    */
  override def stream(
      request: ModelRequest,
      onDelta: Delta => Unit
  ): Either[ProviderError, Message.Assistant] =
    guarded {
      val body = OpenRouterJson.request(config.model, config.maxTokens, config.routing, request)
      body("stream") = true
      val response = http.send(post(body), HttpResponse.BodyHandlers.ofLines())
      val lines = response.body
      try {
        if (response.statusCode == 200)
          OpenRouterStream.fold(lines.iterator().asScala, onDelta).flatMap(OpenRouterJson.response)
        else
          Left(OpenRouterJson.error(response.statusCode, lines.iterator().asScala.mkString("\n")))
      } finally lines.close()
    }

  private def post(body: ujson.Value): HttpRequest =
    HttpRequest
      .newBuilder(config.endpoint)
      .timeout(config.timeout)
      .header("Authorization", s"Bearer ${config.apiKey}")
      .header("Content-Type", "application/json")
      .header("X-OpenRouter-Title", "grit")
      .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
      .build()

  private def guarded(
      call: => Either[ProviderError, Message.Assistant]
  ): Either[ProviderError, Message.Assistant] =
    try call
    catch {
      // The message never includes the request, so never the key.
      case NonFatal(e) =>
        Left(ProviderError.Unavailable(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }
}
