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

/** A run of text under one set of marks. A `'\n'` in `text` is a line break the author
  * wrote, which an edge keeps.
  */
final case class Span(text: String, marks: Set[Mark])

object Span {

  /** `text` with no marks. */
  def plain(text: String): Span = Span(text, Set.empty)
}

/** Inline text: the spans of a paragraph, a heading, a table cell. Adjacent spans never
  * share a set of marks, and none is empty, when the parser made them.
  */
final case class Text(spans: Vector[Span]) {

  /** The characters alone, marks dropped. */
  def plain: String = spans.map(_.text).mkString

  def isEmpty: Boolean = spans.forall(_.text.isEmpty)
}

object Text {

  val empty: Text = Text(Vector.empty)

  /** `s` with no marks. */
  def plain(s: String): Text = if (s.isEmpty) empty else Text(Vector(Span.plain(s)))
}
