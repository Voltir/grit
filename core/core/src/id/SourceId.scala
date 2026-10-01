package grit.core.id

/** The id an edge's source gave an inbound message: a Slack event id, a schedule slot, a
  * TUI submission id. Unique within one [[Origin]], so a redelivery is recognised.
  */
opaque type SourceId = String

object SourceId {
  def apply(value: String): SourceId = value
  def value(id: SourceId): String = id
}
