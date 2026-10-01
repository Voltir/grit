package grit.core.model

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

  /** OpenRouter's `reasoning_details`, verbatim. */
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
  * the [[Settings]] a profile resolves to, never the profile itself.
  */
final case class Profile(
    ref: ModelRef,
    strict: Known[StrictSchemas] = Known.Unmeasured,
    replay: Known[ReasoningReplay] = Known.Unmeasured,
    names: Known[NameRepair] = Known.Unmeasured,
    repairs: Known[Set[ArgRepair]] = Known.Unmeasured,
    afterResult: Known[AfterToolResult] = Known.Unmeasured,
    guidance: Known[ToolGuidance] = Known.Unmeasured
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
      guidance.orOver(other.guidance)
    )
}

/** What a call is made under. `profiled` is false when no profile named the pair. */
final case class Settings(
    strict: StrictSchemas,
    replay: ReasoningReplay,
    names: NameRepair,
    repairs: Set[ArgRepair],
    afterResult: AfterToolResult,
    guidance: ToolGuidance,
    profiled: Boolean
)

object Settings {

  /** `profile` with each unmeasured setting as grit behaved before profiles: strict
    * [[StrictSchemas.Ignored]] (not sent), reasoning [[ReasoningReplay.Details]], names
    * [[NameRepair.HarmonyCut]], every [[ArgRepair]], notes as a [[AfterToolResult.UserMessage]],
    * guidance [[ToolGuidance.SchemaOnly]]. No profile: all of those, and not `profiled`.
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
      profile.isDefined
    )
  }
}
