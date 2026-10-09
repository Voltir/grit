package grit.outline.model

/** One resolved reference to `target`, defined at `targetLine` (1-based; 0 when it has no position): `enclosing` is the full name of the definition holding it, `text` is that source line trimmed. */
final case class Use(
    target: String,
    targetLine: Int,
    file: String,
    line: Int,
    enclosing: String,
    text: String
)
