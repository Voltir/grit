package grit.core

/** The provider's id for a tool call, which its result must quote back. */
opaque type ToolCallId = String

object ToolCallId {
  def apply(value: String): ToolCallId = value
  def value(id: ToolCallId): String = id

}
