package grit.core.id

/** Identifier for a durable workflow run. A turn's is `{conversationId}:{turnSeq}`; a
  * repeated start with the same id resumes or returns that run instead of starting another.
  */
opaque type WorkflowId = String

object WorkflowId {
  def apply(value: String): WorkflowId = value
  def value(id: WorkflowId): String = id
}
