package grit.outline.model

/** One resolved reference to `target`: `enclosing` is the full name of the definition holding it, `text` is that source line trimmed. */
final case class Use(target: String, file: String, line: Int, enclosing: String, text: String)
