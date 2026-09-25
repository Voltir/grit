package grit.core.tool

import grit.core.provider.ToolSchema

/** A tool the model may call, reading its arguments into an `A`: `does` is what the model is
  * told it is for, and when to call it.
  */
final case class ToolSpec[A](name: ToolName, does: String, args: Args[A]) {

  /** What a request shows the model of it; `strict` asks the provider to hold the model's
    * arguments to the schema.
    */
  def schema(strict: Boolean): ToolSchema =
    ToolSchema(ToolName.value(name), does, args.schema(strict), strict)
}
