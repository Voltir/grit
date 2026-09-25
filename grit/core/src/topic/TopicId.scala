package grit.core.topic

import grit.core.id.{TurnRef, WorkflowId}

/** A topic of one conversation. Named for the turn that opened it, so a turn replayed opens
  * the same topic, and no two turns open the same one.
  */
opaque type TopicId = String

object TopicId {
  def apply(value: String): TopicId = value
  def value(id: TopicId): String = id

  /** The topic `turn` opens, if it opens one. */
  def openedBy(turn: TurnRef): TopicId = s"topic:${WorkflowId.value(turn.workflowId)}"
}
