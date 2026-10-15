package grit.core.id

/** Identifier for a durable workflow run. A turn's is `{conversationId}:{turnSeq}`; a
  * repeated start with the same id resumes or returns that run instead of starting another.
  */
opaque type WorkflowId = String

object WorkflowId {
  def apply(value: String): WorkflowId = value
  def value(id: WorkflowId): String = id

  /** What `from` reads `id` as, or, when it reads none, why: `not a <kind>: <id>`. */
  def as[A](kind: String, from: WorkflowId -> Option[A])(id: WorkflowId): Either[String, A] =
    from(id).toRight(s"not a $kind: $id")
}
