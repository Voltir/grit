package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.util.control.NonFatal

import grit.core.message.Message
import grit.core.provider.{ModelRequest, Provider, ProviderError}

/** [[Provider]] over OpenRouter's chat completions, one blocking request per call, no
  * retries (the durable turn records the outcome either way).
  */
final class OpenRouterProvider(config: OpenRouterConfig) extends Provider {

  private val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
    try {
      val body = ujson.write(OpenRouterJson.request(config.model, config.maxTokens, request))
      val response = http.send(
        HttpRequest
          .newBuilder(config.endpoint)
          .timeout(config.timeout)
          .header("Authorization", s"Bearer ${config.apiKey}")
          .header("Content-Type", "application/json")
          .header("X-OpenRouter-Title", "grit")
          .POST(HttpRequest.BodyPublishers.ofString(body))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      if (response.statusCode == 200) OpenRouterJson.response(ujson.read(response.body))
      else Left(OpenRouterJson.error(response.statusCode, response.body))
    } catch {
      // The message never includes the request, so never the key.
      case NonFatal(e) =>
        Left(ProviderError.Unavailable(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }
}
