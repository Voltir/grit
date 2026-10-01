package grit.tui.model.text

/** One wrapped row: the display text plus the offset into the logical, unwrapped text
  * it came from. The offset is the provenance that lets a screen position be mapped
  * back to a document position and a selection be copied as the author wrote it.
  */
final case class Row(startOffset: Int, text: String)

/** Greedy wrapping at spaces, measured in display columns.
  *
  * Existing newlines are honoured; a word too long to fit hard-breaks at the column
  * boundary. Rows carry their [[Row.startOffset]] so the logical text is always
  * recoverable. No trailing spacer is added: inter-entry gaps are the caller's layout
  * concern, not a property of wrapping.
  */
object Wrap {

  /** `text` broken to `width` display columns; `width` below one is clamped to one. */
  def wrap(text: String, width: Int): Vector[Row] = wrap(text, width, width)

  /** `text` broken to `first` display columns on its first row and `rest` on every row
    * after it -- the room a block leaves its text beside a lead and a hanging indent.
    * Either width below one is clamped to one.
    */
  def wrap(text: String, first: Int, rest: Int): Vector[Row] = {
    val w0 = math.max(1, first)
    val w1 = math.max(1, rest)
    val out = Vector.newBuilder[Row]
    var n = 0
    var base = 0
    val logical = text.split("\n", -1)
    var li = 0
    while (li < logical.length) {
      val line = logical(li)
      if (line.isEmpty) { out += Row(base, ""); n += 1 }
      else {
        var pos = 0
        while (pos < line.length) {
          val w = if (n == 0) w0 else w1
          val rest = line.substring(pos)
          if (Width.of(rest) <= w) { out += Row(base + pos, rest); pos = line.length }
          else {
            val hard = pos + Width.offsetAtColumn(rest, w)
            // A space at index `hard` itself breaks a row that fills exactly `w` columns.
            val space = line.lastIndexOf(' ', hard)
            val soft = space > pos
            val brk = if (soft) space else math.max(hard, pos + 1)
            out += Row(base + pos, line.substring(pos, brk))
            pos = if (soft) brk + 1 else brk
          }
          n += 1
        }
      }
      base += line.length + 1 // the '\n' that split consumed
      li += 1
    }
    out.result()
  }

  /** `text` cut to `width` display columns, one row per logical line: what a block that
    * does not wrap shows. The cut never splits a glyph, and each row keeps its
    * [[Row.startOffset]], so the logical text -- and so a copy -- stays whole.
    */
  def truncate(text: String, width: Int): Vector[Row] = truncate(text, width, width)

  /** As [[truncate]], cut to `first` columns on the first row and `rest` on the others. */
  def truncate(text: String, first: Int, rest: Int): Vector[Row] = {
    val out = Vector.newBuilder[Row]
    var base = 0
    val logical = text.split("\n", -1)
    var li = 0
    while (li < logical.length) {
      val line = logical(li)
      val w = math.max(1, if (li == 0) first else rest)
      out += Row(base, line.substring(0, Width.offsetAtColumn(line, w)))
      base += line.length + 1
      li += 1
    }
    out.result()
  }
}
