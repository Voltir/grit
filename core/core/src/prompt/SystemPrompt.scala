package grit.core.prompt

/** A system prompt: its fragments by [[Layer]], and within a layer in the order given. */
final case class SystemPrompt private (fragments: Vector[Fragment]) {

  /** The fragments' texts in order, a blank line between: equal prompts render equal bytes. */
  def render: String = fragments.map(_.text).mkString("\n\n")

  /** The fragments' ids, in order. */
  def ids: Vector[FragmentId] = fragments.map(_.id)
}

object SystemPrompt {

  /** `fragments`, sorted stably by layer. */
  def of(fragments: Vector[Fragment]): SystemPrompt =
    SystemPrompt(fragments.sortBy(_.layer.ordinal))
}
