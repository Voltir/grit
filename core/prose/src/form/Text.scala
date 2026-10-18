package grit.prose.form

/** What a run of text is marked as. Meanings, never looks: how strong text or a link is
  * shown is each edge's to say ([[Renderer]]).
  */
enum Mark {

  /** Strong importance: markdown's `**`. */
  case Strong

  /** Stress: markdown's `*`. */
  case Emphasis

  /** A fragment of code, to be read literally: markdown's backticks. */
  case Code

  /** Part of a link's text, the link going to `href`. */
  case Link(href: String)
}

/** A run of text under one set of marks, carrying `notes`: what its writer says of it, never
  * shown by a renderer. A `'\n'` in `text` is a line break the author wrote, which an edge
  * keeps.
  */
final case class Span[+A <: caps.Pure](text: String, marks: Set[Mark], notes: Vector[A])

object Span {

  /** `text` under `marks`, with no notes. */
  def apply(text: String, marks: Set[Mark]): Span[Nothing] = Span(text, marks, Vector.empty)

  /** `text` with no marks. */
  def plain(text: String): Span[Nothing] = Span(text, Set.empty)
}

/** Inline text: the spans of a paragraph, a heading, a table cell. Adjacent spans never
  * share both their marks and their notes, and none is empty, when the parser or [[Text.of]]
  * made them.
  */
final case class Text[+A <: caps.Pure](spans: Vector[Span[A]]) {

  /** The characters alone, marks dropped. */
  def plain: String = spans.map(_.text).mkString

  def isEmpty: Boolean = spans.forall(_.text.isEmpty)
}

object Text {

  val empty: Text[Nothing] = Text(Vector.empty)

  /** `s` with no marks. */
  def plain(s: String): Text[Nothing] = if (s.isEmpty) empty else Text(Vector(Span.plain(s)))
}
