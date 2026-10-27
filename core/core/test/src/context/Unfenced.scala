package grit.core.context

import grit.core.message.Message

/** What a tool wrote, read back from the model's copy of its result ([[Shown.turn]]): the
  * content inside its `<tool_output>` fence, a closing tag in it still escaped; content shown
  * unfenced as it is.
  */
object Unfenced {

  private val Fence = """(?s)<tool_output source="[^"]*">\n(.*)\n</tool_output>""".r

  def apply(content: String): String = content match {
    case Fence(inner) => inner
    case other => other
  }

  /** `r` with its content read as [[apply]] reads it. */
  def apply(r: Message.ToolResult): Message.ToolResult = r.copy(content = apply(r.content))
}
