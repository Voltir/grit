package grit.core.tool

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.provider.ToolSchema

/** Whether a person approves each call of a tool before it runs. */
enum Gate[-A] {

  /** It runs without asking: a tool that only reads. */
  case Free

  /** Each call waits up to `within` for a person's answer, and at most its loop's
    * ([[grit.core.tool.Gate.AnswerWithin]] unless the deployment shortens it); `describe` is
    * what they are shown of a call's arguments. Pure, so the text approved is fixed before
    * anything runs.
    */
  case Ask(describe: A -> String, within: FiniteDuration = Gate.AnswerWithin)
}

object Gate {

  /** How long a call that asks first waits by default: a day, so a local turn left overnight is
    * still waiting in the morning.
    */
  val AnswerWithin: FiniteDuration = 24.hours

  /** What a tool that asks first says after its description: that calling it is how the
    * person is asked to approve the call, so the model calls it rather than asking in its
    * reply, and that a declined call runs nothing.
    */
  val AsksFirst: String =
    "Calling this tool asks the person to approve the call; do not ask in your reply. If " +
      "they decline, nothing runs."

  /** `schema`, its description followed by [[AsksFirst]] when `asks`. */
  private[tool] def described(schema: ToolSchema, asks: Boolean): ToolSchema =
    if (asks) schema.copy(description = s"${schema.description} $AsksFirst") else schema
}
