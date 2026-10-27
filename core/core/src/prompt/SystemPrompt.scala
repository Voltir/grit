package grit.core.prompt

/** A system prompt: its fragments by [[Layer]], and within a layer in the order given. */
final case class SystemPrompt private (fragments: Vector[Fragment]) {

  /** The fragments' texts in order, a blank line between, each [[Layer.Place]] fragment's
    * fenced in `<place_instructions source="{its source}">` ([[Fenced]]), every other as
    * written: equal prompts render equal bytes.
    */
  def render: String = fragments
    .map(f =>
      if (f.layer == Layer.Place)
        Fenced.in(Tag.PlaceInstructions, f.text, "source" -> f.source).rendered
      else f.text
    )
    .mkString("\n\n")

  /** The fragments' ids, in order. */
  def ids: Vector[FragmentId] = fragments.map(_.id)
}

object SystemPrompt {

  /** `fragments`, sorted stably by layer. */
  def of(fragments: Vector[Fragment]): SystemPrompt =
    SystemPrompt(fragments.sortBy(_.layer.ordinal))
}
