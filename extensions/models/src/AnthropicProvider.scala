package grit.models

import java.net.URI
import java.net.http.{HttpClient, HttpRequest}
import java.time.Duration

import grit.core.message.Message
import grit.core.model.{ModelId, Pinned}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
import grit.core.secret.{Reveal, Secret, Variable}

/** [[Provider]] over Anthropic's Messages API, called with `key`, one request per call,
  * streamed or not, no retries, under `pinned`'s model (its id after `anthropic/`), budget and
  * effort, thinking adaptive and summarised, earlier thinking sent back as its settings'
  * replay says. Caches `tools` and `system`, and a request's first `stable` messages, with a
  * breakpoint after each ([[AnthropicJson.request]]). Each reply records
  * [[grit.core.model.Upstream.AnthropicApi]] as the upstream that served it, and its cost from
  * [[AnthropicRates]].
  *
  * A failure to connect, a timeout or a connection lost is [[ProviderError.Unavailable]]; an
  * HTTP error is classed as [[AnthropicJson.error]] says, an error event in a stream as
  * [[AnthropicJson.modelError]] says; a decline by the model's safeguards is
  * [[ProviderError.Refused]] naming its category; a model not under `anthropic/` is
  * [[ProviderError.Refused]] with no call made.
  */
final class AnthropicProvider(key: Secret, pinned: Pinned) extends Provider {

  private val revealed = Reveal.text(key)

  private val timeout = Duration.ofMinutes(5)

  private val http = HttpClient.newBuilder().connectTimeout(timeout).build()

  private val model = AnthropicProvider.modelOf(pinned)

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
    model.flatMap { m =>
      ProviderHttp.complete(
        http,
        post(m, request, streamed = false),
        AnthropicJson.response,
        AnthropicJson.error
      )
    }

  /** The response streamed ([[AnthropicStream]]), read line by line as it arrives. The timeout
    * bounds the wait for the response to begin, not the whole of it.
    */
  override def stream(
      request: ModelRequest,
      onDelta: Delta => Unit
  ): Either[ProviderError, Message.Assistant] =
    model.flatMap { m =>
      ProviderHttp.stream(
        http,
        post(m, request, streamed = true),
        AnthropicStream.fold(_, onDelta),
        AnthropicJson.response,
        AnthropicJson.error
      )
    }

  private def post(model: String, request: ModelRequest, streamed: Boolean): HttpRequest = {
    val (headers, body) = AnthropicProvider.wire(revealed, model, pinned, request)
    if (streamed) body("stream") = true
    ProviderHttp.post(AnthropicProvider.Endpoint, timeout, headers, body)
  }
}

object AnthropicProvider {

  val Endpoint: URI = URI.create("https://api.anthropic.com/v1/messages")

  /** The variable Anthropic's key is read from ([[Keys.of]]). */
  val KeyVar: Variable = Variable("ANTHROPIC_API_KEY")

  /** The headers a request is sent with under `key`, besides its content type: under
    * [[AnthropicJson.BindingBeta]] too when it asks [[AnthropicJson.request]]'s `dropBlock`.
    */
  private[models] def headers(key: String, dropBlock: Boolean): Seq[(String, String)] =
    Seq("x-api-key" -> key, "anthropic-version" -> AnthropicJson.Version) ++
      Option.when(dropBlock)("anthropic-beta" -> AnthropicJson.BindingBeta)

  /** The headers and the body a plain call of `request` is sent with under `key`, on `model`
    * (Anthropic's own id) and `pinned`'s budget, effort and replay; asking `dropBlock` for a
    * model in [[DropsMismatchedThinking]].
    */
  private[models] def wire(
      key: String,
      model: String,
      pinned: Pinned,
      request: ModelRequest
  ): (Seq[(String, String)], ujson.Value) = {
    val dropBlock = DropsMismatchedThinking.exists(AnthropicJson.isOrSnapshotOf(model, _))
    val body = AnthropicJson.request(
      model,
      pinned.assignment.maxTokens,
      pinned.assignment.effort,
      pinned.settings.replay,
      dropBlock,
      request
    )
    (headers(key, dropBlock), body)
  }

  /** How a pair served by Anthropic directly names its model: `anthropic/` and Anthropic's own
    * id.
    */
  val Vendor = "anthropic/"

  /** The models (Anthropic's ids) whose requests ask the API to drop a thinking block an
    * earlier edit invalidated, rather than refuse the request ([[AnthropicJson.request]]'s
    * `dropBlock`): those a live call (the test sources' `AnthropicLiveProbe`) showed accepting
    * it. Anthropic documents it for adaptive thinking on Claude Haiku 5.5 alone. A dated
    * snapshot of a model here asks it too.
    */
  val DropsMismatchedThinking: Set[String] = Set("claude-haiku-5-5")

  /** Anthropic's own id for `pinned`'s model, the part after [[Vendor]]; `Refused` when it is
    * not under [[Vendor]].
    */
  private[models] def modelOf(pinned: Pinned): Either[ProviderError, String] = {
    val named = ModelId.value(pinned.assignment.ref.model)
    Option
      .when(named.startsWith(Vendor))(named.drop(Vendor.length))
      .toRight(ProviderError.Refused(s"$named is not one of Anthropic's models (anthropic/...)"))
  }
}
