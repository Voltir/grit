package grit.outline.model

/** A test call in a suite's source: its name, the lines it spans, and their verbatim text. */
final case class TestCase(name: String, lines: Lines, text: String)
