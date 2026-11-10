package grit.core.model

import grit.core.message.Tokens

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
}

/** A repair made to a tool call's arguments before they are read. */
enum ArgRepair {

  /** A whole number sent as a string of its digits is read as the number. */
  case QuotedNumber

  /** A list sent as a string holding a JSON list is read as the list. */
  case QuotedList
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

/** Everything grit knows about one pair: data, read and stored as is. A call is made under
  * the [[Settings]] a profile resolves to, never the profile itself. `contextLength` is the
  * most tokens, input and output together, the pair accepts in one request;
  * `effectiveContext`, the longest prompt (a request's input) it does its best work within;
  * `pricing`, what it bills a call.
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
    pricing: Known[Pricing] = Known.Unmeasured
) {

  /** Each of `other`'s settings over this one's where its source ranks higher
    * ([[Known.orOver]]); `other` names the same pair.
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
      pricing.orOver(other.pricing)
    )
}

/** What a call is made under. `profiled` is false when no profile named the pair;
  * `contextLength` is `None` when no profile gives it, and a turn's requests are then bounded
  * by its window's limit, not a context (`TurnLoop.inputLimit`). `effectiveContext` and
  * `pricing` are `None` when no profile gives them: a call is then unpriced (`Cost.AtLeast`),
  * and a turn is bounded by neither.
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
    pricing: Option[Pricing] = None
)

object Settings {

  /** `profile` with each unmeasured setting as grit behaved before profiles: strict
    * [[StrictSchemas.Ignored]] (not sent), reasoning [[ReasoningReplay.Details]], names
    * [[NameRepair.HarmonyCut]], every [[ArgRepair]], notes as a [[AfterToolResult.UserMessage]],
    * guidance [[ToolGuidance.SchemaOnly]]. No profile: all of those, and not `profiled`.
    */
  def of(profile: Option[Profile]): Settings = {
    def known[A](setting: Profile => Known[A]): Known[A] = profile.fold(Known.Unmeasured)(setting)
    def value[A](k: Known[A]): Option[A] = k match {
      case Known.Of(a, _) => Some(a)
      case Known.Unmeasured => None
    }
    Settings(
      known(_.strict).getOr(StrictSchemas.Ignored),
      known(_.replay).getOr(ReasoningReplay.Details),
      known(_.names).getOr(NameRepair.HarmonyCut),
      known(_.repairs).getOr(ArgRepair.values.toSet),
      known(_.afterResult).getOr(AfterToolResult.UserMessage),
      known(_.guidance).getOr(ToolGuidance.SchemaOnly),
      profile.isDefined,
      value(known(_.contextLength)),
      value(known(_.effectiveContext)),
      value(known(_.pricing))
    )
  }
}
