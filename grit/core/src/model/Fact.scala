package grit.core.model

import java.time.LocalDate

/** One setting of a pair, with the value a probe or a person found. */
enum Setting {
  case Strict(value: StrictSchemas)
  case Replay(value: ReasoningReplay)
  case Names(value: NameRepair)
  case Repairs(value: Set[ArgRepair])
  case AfterResult(value: AfterToolResult)
  case Guidance(value: ToolGuidance)
}

/** A measured fact about `ref`: `setting` held in `held` of `runs` runs of `probe`
  * (`held <= runs`, both checked where a fact is read).
  */
final case class Fact(ref: ModelRef, setting: Setting, probe: String, runs: Int, held: Int) {

  /** The fact as a profile of `ref` that knows only `setting`, measured on `on`. */
  def profile(on: LocalDate): Profile = {
    val source = Source.Measured(probe, on, runs, held)
    val none = Profile(ref)
    setting match {
      case Setting.Strict(v) => none.copy(strict = Known.Of(v, source))
      case Setting.Replay(v) => none.copy(replay = Known.Of(v, source))
      case Setting.Names(v) => none.copy(names = Known.Of(v, source))
      case Setting.Repairs(v) => none.copy(repairs = Known.Of(v, source))
      case Setting.AfterResult(v) => none.copy(afterResult = Known.Of(v, source))
      case Setting.Guidance(v) => none.copy(guidance = Known.Of(v, source))
    }
  }
}

/** Capability to keep a fact a person approved: kept, the next turn's catalog lays it over
  * the seed. The book dates it and names who approved it.
  */
trait FactBook extends caps.SharedCapability {

  /** Keeps `fact`; `Left` says why it was not kept. */
  def keep(fact: Fact): Either[String, Unit]
}
