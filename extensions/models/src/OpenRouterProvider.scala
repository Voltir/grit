package grit.models

import java.net.http.{HttpClient, HttpRequest}

import grit.core.message.Message
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
import grit.core.secret.{Reveal, Secret}

/** [[Provider]] over OpenRouter's chat completions, called with `key` under `config`, one
  * request per call, streamed or not, no retries. A failure to connect, a timeout or a connection lost is
  * [[ProviderError.Unavailable]] ([[ProviderHttp]]); how an HTTP error or a model error is
  * classed is [[OpenRouterJson.error]]'s and [[OpenRouterJson.modelError]]'s.
  */
final class OpenRouterProvider(key: Secret, config: OpenRouterConfig) extends Provider {

  private val authorization = s"Bearer ${Reveal.text(key)}"

  private val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
    ProviderHttp.complete(http, post(body(request)), OpenRouterJson.response, OpenRouterJson.error)

  /** The response streamed ([[OpenRouterStream]]), read line by line as it arrives. The
    * timeout bounds the wait for the response to begin, not the whole of it.
    */
  override def stream(
      request: ModelRequest,
      onDelta: Delta => Unit
  ): Either[ProviderError, Message.Assistant] = {
    val streamed = body(request)
    streamed("stream") = true
    ProviderHttp.stream(
      http,
      post(streamed),
      OpenRouterStream.fold(_, onDelta),
      OpenRouterJson.response,
      OpenRouterJson.error
    )
  }

  private def body(request: ModelRequest): ujson.Value =
    OpenRouterJson.request(
      config.model,
      config.maxTokens,
      config.upstream,
      request,
      config.effort,
      config.replay
    )

  private def post(body: ujson.Value): HttpRequest =
    ProviderHttp.post(
      config.endpoint,
      config.timeout,
      Seq("Authorization" -> authorization, "X-OpenRouter-Title" -> "grit"),
      body
    )
}
