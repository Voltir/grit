package grit.kit.run

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.ModelRef

/** How grit writes one model call's consumption for a person to read: in the cache report, and
  * after each call a run with arguments or `grit work` makes.
  */
object CallLine {

  /** `in <input> · read <cache read> · write <cache write> · out <output> · <cost>`, the cost in
    * cents ([[Cost.cents]]) or `cost unknown` when the call was not priced.
    */
  def spent(u: Usage): String =
    s"in ${Tokens.value(u.input)} · read ${Tokens.value(u.cacheRead)} · write ${Tokens
        .value(u.cacheWrite)} · out ${Tokens.value(u.output)} · ${u.costUsd
        .fold("cost unknown")(Cost.cents)}"

  /** `[provider] <pair> answered · ` and [[spent]]: the line printed after a call `ref`'s model
    * answered.
    */
  def answered(ref: ModelRef, u: Usage): String = s"[provider] $ref answered · ${spent(u)}"
}
