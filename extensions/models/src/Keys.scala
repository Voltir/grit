package grit.models

import grit.core.message.Message
import grit.core.model.{ModelRef, Pinned, Policy, Upstream}
import grit.core.provider.{ModelRequest, Provider, ProviderError}

/** The keys of the gateways grit calls models through, at least one of them set: OpenRouter's
  * ([[OpenRouterConfig.KeyVar]]) and Anthropic's ([[AnthropicProvider.KeyVar]]). `toString`
  * shows neither.
  */
final class Keys private (openRouter: Option[String], anthropic: Option[String]) {

  /** A provider whose every call is made under `pinned`, through the gateway that serves its
    * pair: [[AnthropicProvider]] for a pair served by
    * [[grit.core.model.Upstream.AnthropicApi]], [[OpenRouterProvider]] for any other. When that
    * gateway's key is not set here, every call is [[grit.core.provider.ProviderError.Refused]]
    * with [[Keys.Unkeyed.message]], and none is made.
    */
  def provider(pinned: Pinned): Provider^ = {
    val ref = pinned.assignment.ref
    keyed(ref) match {
      case Left(unkeyed) =>
        new Provider {
          def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
            Left(ProviderError.Refused(unkeyed.message))
        }
      case Right(key) =>
        if (direct(ref)) new AnthropicProvider(key, pinned)
        else new OpenRouterProvider(OpenRouterConfig.of(key, pinned))
    }
  }

  /** The first pair `policy` assigns, role by role and rung by rung, whose gateway's key is not
    * set here; `None` when every pair's is.
    */
  def missing(policy: Policy): Option[Keys.Unkeyed] =
    policy.ladders
      .flatMap(_.rungs)
      .map(rung => keyed(rung.assignment.ref))
      .collectFirst { case Left(unkeyed) => unkeyed }

  private def direct(ref: ModelRef): Boolean = ref.upstream.contains(Upstream.AnthropicApi)

  /** The key `ref`'s gateway is called with, or the variable it is missing from. */
  private def keyed(ref: ModelRef): Either[Keys.Unkeyed, String] =
    if (direct(ref)) anthropic.toRight(Keys.Unkeyed(AnthropicProvider.KeyVar, ref))
    else openRouter.toRight(Keys.Unkeyed(OpenRouterConfig.KeyVar, ref))

  override def toString: String =
    s"Keys(openRouter: ${openRouter.fold("unset")(_ => "set")}, " +
      s"anthropic: ${anthropic.fold("unset")(_ => "set")})"
}

object Keys {

  /** `ref` is called through a gateway whose key, read from `variable`, is not set. */
  final case class Unkeyed(variable: String, ref: ModelRef) {
    def message: String = s"$variable is not set, and $ref is called with it"
  }

  /** `env`'s keys, each variable optional but not blank when set; `None` when neither is set:
    * the stub model, so no key, no spend.
    */
  def of(env: Map[String, String]): Either[OpenRouterConfig.Invalid, Option[Keys]] =
    for {
      openRouter <- read(env, OpenRouterConfig.KeyVar)
      anthropic <- read(env, AnthropicProvider.KeyVar)
    } yield Option.when(openRouter.isDefined || anthropic.isDefined)(
      new Keys(openRouter, anthropic)
    )

  private def read(
      env: Map[String, String],
      variable: String
  ): Either[OpenRouterConfig.Invalid, Option[String]] =
    env.get(variable) match {
      case None => Right(None)
      case Some(key) =>
        Either.cond(key.trim.nonEmpty, Some(key), OpenRouterConfig.Invalid.Empty(variable))
    }
}
