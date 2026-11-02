package grit.models

import grit.core.message.Message
import grit.core.model.{ModelRef, Pinned, Policy, Upstream}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.secret.{Secret, Variable}

/** The keys of the gateways grit calls models through, at least one of them set: OpenRouter's
  * ([[OpenRouterConfig.KeyVar]]) and Anthropic's ([[AnthropicProvider.KeyVar]]).
  */
final class Keys private (openRouter: Option[Secret], anthropic: Option[Secret]) {

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
        else new OpenRouterProvider(key, OpenRouterConfig.of(pinned))
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
  private def keyed(ref: ModelRef): Either[Keys.Unkeyed, Secret] =
    if (direct(ref)) anthropic.toRight(Keys.Unkeyed(AnthropicProvider.KeyVar, ref))
    else openRouter.toRight(Keys.Unkeyed(OpenRouterConfig.KeyVar, ref))

  override def toString: String =
    s"Keys(openRouter: ${openRouter.fold("unset")(_ => "set")}, " +
      s"anthropic: ${anthropic.fold("unset")(_ => "set")})"
}

object Keys {

  /** `ref` is called through a gateway whose key, read from `variable`, is not set. */
  final case class Unkeyed(variable: Variable, ref: ModelRef) {
    def message: String =
      s"${Variable.value(variable)} is not set, but the model catalog calls $ref, which needs it"
  }

  /** `env`'s keys, each read as [[Secret.read]] reads one, but optional: an unset variable is no
    * key, and one set but blank is refused. `None` when neither is set: the stub model, so no
    * key, no spend.
    */
  def of(env: Map[String, String]): Either[Secret.Refusal, Option[Keys]] =
    for {
      openRouter <- read(env, OpenRouterConfig.KeyVar)
      anthropic <- read(env, AnthropicProvider.KeyVar)
    } yield Option.when(openRouter.isDefined || anthropic.isDefined)(
      new Keys(openRouter, anthropic)
    )

  private def read(
      env: Map[String, String],
      variable: Variable
  ): Either[Secret.Refusal, Option[Secret]] =
    Secret.read(env, variable) match {
      case Right(key) => Right(Some(key))
      case Left(Secret.Refusal.Unset(_)) => Right(None)
      case Left(refused) => Left(refused)
    }
}
