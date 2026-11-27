package grit.core.model

import grit.core.message.{Message, Tokens}

/** Whether an upstream holds a model's tool arguments to a `strict` schema. */
enum StrictSchemas {

  /** Held under `tool_choice: auto`: grit sends `strict`. */
  case Enforced

  /** Held only under `tool_choice: required`, which grit does not send. */
  case WhenRequired

  /** Accepted and not held. */
  case Ignored

  /** Checked after generation, and the call refused with an error. */
  case Rejected
}

/** How the model's reasoning from an earlier call of a turn is sent back with the next. */
enum ReasoningReplay {

  /** The provider's own form ([[grit.core.message.AssistantBlock.Reasoning]]'s `replay`),
    * verbatim: OpenRouter's `reasoning_details`, Anthropic's thinking blocks.
    */
  case Details

  /** Not sent. */
  case Dropped

  /** As [[Details]] for the last assistant reply the request sends; not sent for any earlier
    * one. What that costs a cache is the bench README's (`eval/bench/README.md`).
    */
  case Latest

  /** For each of a request's `messages`, whether its reasoning is sent under this replay;
    * `sent` says whether the wire sends a reply at all when its reasoning goes with it.
    */
  def sends(messages: Vector[Message], sent: Message.Assistant => Boolean): Vector[Boolean] = {
    val last = messages.lastIndexWhere {
      case a: Message.Assistant => sent(a)
      case _ => false
    }
    messages.indices.toVector.map(i =>
      this match {
        case Details => true
        case Dropped => false
        case Latest => i == last
      }
    )
  }
}

/** A repair made to a tool call's arguments before they are read. */
enum ArgRepair {

  /** A whole number sent as a string of its digits is read as the number. */
  case QuotedNumber

  /** A list sent as a string holding a JSON list is read as the list. */
  case QuotedList

  /** A string field holding its own close tag and then other fields as
    * `<parameter name="…">` markup is cut at the close, and each carried field sent null or
    * absent is read from the markup.
    */
  case CarriedParameter
}

/** What a pair's upstream needs in a request beyond its gateway's shape. */
enum WireQuirk {

  /** Z.AI's `thinking: {type: "enabled", clear_thinking: …}` is sent, `clear_thinking` false
    * unless the replay is [[ReasoningReplay.Dropped]]: the upstream keeps the reasoning the
    * replay sends, and clears it when none is.
    */
  case ClearThinking

  /** An assistant reply sent with tool calls and no `reasoning_details` carries
    * `"reasoning_content": ""`, which DeepSeek asks of every replayed call.
    */
  case EmptyReasoningContent
}

/** A repair made to a tool call's name before it is looked up. */
enum NameRepair {

  /** The name is used as sent. */
  case AsSent

  /** The name is cut at its first `<|` (gpt-oss's harmony tokens: `read<|channel|>…`). */
  case HarmonyCut
}

/** Where a note to the model goes when the last messages are tool results. */
enum AfterToolResult {

  /** In a user message after them. */
  case UserMessage

  /** Appended to the last tool result: an upstream refuses a user message there. */
  case InLastResult
}

/** How the model is told what each offered tool is for. */
enum ToolGuidance {

  /** Only in each tool's schema. */
  case SchemaOnly

  /** In each tool's schema, and a line per offered tool in the system prompt. */
  case SystemLines
}

/** What a pair's first price rise does to a turn (`TurnLoop.inputLimit`). */
enum PriceRise {

  /** The turn ends before its input reaches the rise. */
  case EndsTurn

  /** The turn grows past the rise, its calls billed at the higher rates. */
  case Billed
}

/** Everything grit knows about one pair: data, read and stored as is. A call is made under
  * the [[Settings]] a profile resolves to, never the profile itself. `contextLength` is the
  * most tokens, input and output together, the pair accepts in one request;
  * `effectiveContext`, the longest prompt (a request's input) it does its best work within;
  * `pricing`, what it bills a call; `density`, the characters of text a token its tokenizer
  * averages, fitted from its usage-ledger rows; `priceRise`, whether a turn may grow past
  * where its price first rises.
  */
final case class Profile(
    ref: ModelRef,
    strict: Known[StrictSchemas] = Known.Unmeasured,
    replay: Known[ReasoningReplay] = Known.Unmeasured,
    names: Known[NameRepair] = Known.Unmeasured,
    repairs: Known[Set[ArgRepair]] = Known.Unmeasured,
    afterResult: Known[AfterToolResult] = Known.Unmeasured,
    guidance: Known[ToolGuidance] = Known.Unmeasured,
    contextLength: Known[Tokens] = Known.Unmeasured,
    effectiveContext: Known[Tokens] = Known.Unmeasured,
    pricing: Known[Pricing] = Known.Unmeasured,
    density: Known[Density] = Known.Unmeasured,
    quirks: Known[Set[WireQuirk]] = Known.Unmeasured,
    priceRise: Known[PriceRise] = Known.Unmeasured
) {

  /** Each of `other`'s settings over this one's where its source ranks higher
    * ([[Known.orOver]]); `other` names the same pair. A set (`repairs`, `quirks`) is replaced
    * whole, never merged.
    */
  def overlaid(other: Profile): Profile =
    Profile(
      ref,
      strict.orOver(other.strict),
      replay.orOver(other.replay),
      names.orOver(other.names),
      repairs.orOver(other.repairs),
      afterResult.orOver(other.afterResult),
      guidance.orOver(other.guidance),
      contextLength.orOver(other.contextLength),
      effectiveContext.orOver(other.effectiveContext),
      pricing.orOver(other.pricing),
      density.orOver(other.density),
      quirks.orOver(other.quirks),
      priceRise.orOver(other.priceRise)
    )
}

/** What a call is made under. `profiled` is false when no profile named the pair;
  * `contextLength` is `None` when no profile gives it, and a turn's requests are then bounded
  * by its window's limit, not a context (`TurnLoop.inputLimit`). `effectiveContext` and
  * `pricing` are `None` when no profile gives them: a call is then unpriced (`Cost.AtLeast`),
  * and a turn is bounded by neither; a turn ends before its price first rises unless
  * `priceRise` is [[PriceRise.Billed]]. `density` is `None` when no profile gives it: a call's
  * request is then costed by the deployment's estimator unchanged
  * (`TokenEstimator.withSettings`). `quirks` are empty when no profile gives them.
  */
final case class Settings(
    strict: StrictSchemas,
    replay: ReasoningReplay,
    names: NameRepair,
    repairs: Set[ArgRepair],
    afterResult: AfterToolResult,
    guidance: ToolGuidance,
    profiled: Boolean,
    contextLength: Option[Tokens] = None,
    effectiveContext: Option[Tokens] = None,
    pricing: Option[Pricing] = None,
    density: Option[Density] = None,
    quirks: Set[WireQuirk] = Set.empty,
    priceRise: PriceRise = PriceRise.EndsTurn
)

object Settings {

  /** `profile` with each unmeasured setting as grit behaved before profiles: strict
    * [[StrictSchemas.Ignored]] (not sent), reasoning [[ReasoningReplay.Details]], names
    * [[NameRepair.HarmonyCut]], every [[ArgRepair]], notes as a [[AfterToolResult.UserMessage]],
    * guidance [[ToolGuidance.SchemaOnly]], a price rise [[PriceRise.EndsTurn]]. No profile:
    * all of those, and not `profiled`.
    */
  def of(profile: Option[Profile]): Settings = {
    def known[A](setting: Profile => Known[A]): Known[A] = profile.fold(Known.Unmeasured)(setting)
    Settings(
      known(_.strict).getOr(StrictSchemas.Ignored),
      known(_.replay).getOr(ReasoningReplay.Details),
      known(_.names).getOr(NameRepair.HarmonyCut),
      known(_.repairs).getOr(ArgRepair.values.toSet),
      known(_.afterResult).getOr(AfterToolResult.UserMessage),
      known(_.guidance).getOr(ToolGuidance.SchemaOnly),
      profile.isDefined,
      known(_.contextLength).toOption,
      known(_.effectiveContext).toOption,
      known(_.pricing).toOption,
      known(_.density).toOption,
      known(_.quirks).getOr(Set.empty),
      known(_.priceRise).getOr(PriceRise.EndsTurn)
    )
  }
}
