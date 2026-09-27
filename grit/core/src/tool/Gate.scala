package grit.core.tool

import grit.core.provider.ToolSchema

/** Whether a person approves each call of a tool before it runs. */
enum Gate[-A] {

  /** It runs without asking: a tool that only reads. */
  case Free

  /** Each call waits for a person's answer; `describe` is what they are shown of a call's
    * arguments. Pure, so the text approved is fixed before anything runs.
    */
  case Ask(describe: A -> String)
}

object Gate {

  /** What a tool that asks first says after its description: that calling it is how the
    * person is asked to approve the call, so the model calls it rather than asking in its
    * reply, and what a declined call returns.
    */
  val AsksFirst: String =
    "Calling this tool asks the person to approve the call, and it runs only once they do: " +
      "call it rather than asking for permission in your reply. If they decline, nothing " +
      "runs and their reason, if any, is returned."

  /** `schema`, its description followed by [[AsksFirst]] when `asks`. */
  private[tool] def described(schema: ToolSchema, asks: Boolean): ToolSchema =
    if (asks) schema.copy(description = s"${schema.description} $AsksFirst") else schema
}
