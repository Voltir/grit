package grit.models

/** A tool call's arguments as a provider sent them, read into the form
  * [[grit.core.message.AssistantBlock.ToolCall]] holds.
  */
private[models] object ToolArguments {

  /** `sent` read as JSON when it is a string, taken at its word when it is already an object;
    * text that does not read as JSON is kept whole as a `ujson.Str`, and absent or blank
    * arguments, or any other value, are `{}`.
    */
  def read(sent: Option[ujson.Value]): ujson.Value = sent match {
    case Some(ujson.Str(raw)) if raw.trim.nonEmpty =>
      // Not JSON: kept whole, for the loop's reader to refuse and echo.
      scala.util.Try(ujson.read(raw)).getOrElse(ujson.Str(raw))
    case Some(o: ujson.Obj) => o
    case _ => ujson.Obj()
  }
}
