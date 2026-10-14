package grit.core.tool

import grit.core.provider.ToolSchema

/** A tool the model may call, reading its arguments into an `A`: `does` is what the model is
  * told it is for, and when to call it (never that a person approves it: its [[Gate]] says
  * that, [[Gate.AsksFirst]]); `retry` is what happens to a call its edge was cut short
  * running; `action` is what a person can no longer have done when its calls are withheld,
  * in words that follow "you can't" ("post to channels"), `None` for a tool said as "use" and
  * its name ([[ToolSet.Entry.plainly]]).
  */
final case class ToolSpec[A](
    name: ToolName,
    does: String,
    args: Args[A],
    retry: Retry = Retry.Interrupt,
    action: Option[String] = None
) {

  /** What a request shows the model of it; `strict` asks the provider to hold the model's
    * arguments to the schema.
    */
  def schema(strict: Boolean): ToolSchema =
    ToolSchema(ToolName.value(name), does, args.schema(strict), strict)
}
