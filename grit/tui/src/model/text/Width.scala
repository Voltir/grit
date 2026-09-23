package grit.tui.model.text

/** Display width of text in terminal cells.
  *
  * The terminal advances the cursor by cells, not by chars, so everything that maps a
  * display column onto a character offset -- wrapping, hit-testing, selection -- has to
  * agree on this object or selection drifts on any line containing a wide glyph.
  * Measured per code point: counting UTF-16 chars measures an astral narrow character
  * as two cells.
  */
object Width {

  /** Cells occupied by one code point: 0 for combining marks, 2 for East Asian wide and
    * emoji, 1 otherwise.
    */
  def ofCodePoint(cp: Int): Int =
    if (cp == 0) 0
    else if (isCombining(cp)) 0
    else if (isWide(cp)) 2
    else 1

  /** Cells occupied by the whole string. */
  def of(s: String): Int = {
    var i = 0
    var total = 0
    while (i < s.length) {
      val cp = s.codePointAt(i)
      total += ofCodePoint(cp)
      i += Character.charCount(cp)
    }
    total
  }

  /** The char offset within `s` at display column `col`, clamped to `[0, s.length]`.
    *
    * A column landing on the second cell of a wide glyph resolves to that glyph's
    * offset, never inside it.
    */
  def offsetAtColumn(s: String, col: Int): Int = {
    if (col <= 0) 0
    else {
      var i = 0
      var used = 0
      var landed = -1
      while (i < s.length && landed < 0) {
        val cp = s.codePointAt(i)
        val w = ofCodePoint(cp)
        if (used + w > col) landed = i else { used += w; i += Character.charCount(cp) }
      }
      if (landed >= 0) landed else s.length
    }
  }

  /** The longest prefix of `s` that fits `cells` display columns.
    *
    * Chrome truncates and content wraps, so this is the cut every bordered thing makes
    * on its title and every one-row widget makes on its text. It cuts *between* glyphs:
    * a budget that lands on the second cell of a wide glyph drops that glyph rather
    * than half of it, because half of a wide glyph is not a character the terminal can
    * draw.
    */
  def fit(s: String, cells: Int): String =
    if (of(s) <= math.max(0, cells)) s
    else s.substring(0, offsetAtColumn(s, math.max(0, cells)))

  /** The display column at which the char offset `at` begins, clamped to
    * `[0, s.length]`.
    */
  def columnAtOffset(s: String, at: Int): Int = {
    val bound = math.max(0, math.min(at, s.length))
    var i = 0
    var used = 0
    while (i < bound) {
      val cp = s.codePointAt(i)
      used += ofCodePoint(cp)
      i += Character.charCount(cp)
    }
    used
  }

  private def isCombining(cp: Int): Boolean =
    (cp >= 0x0300 && cp <= 0x036f) || (cp >= 0x1ab0 && cp <= 0x1aff) ||
      (cp >= 0x20d0 && cp <= 0x20ff) || (cp >= 0xfe00 && cp <= 0xfe0f)

  private def isWide(cp: Int): Boolean =
    (cp >= 0x1100 && cp <= 0x115f) || (cp >= 0x2e80 && cp <= 0xa4cf) ||
      (cp >= 0xac00 && cp <= 0xd7a3) || (cp >= 0xf900 && cp <= 0xfaff) ||
      (cp >= 0xfe30 && cp <= 0xfe6f) || (cp >= 0xff00 && cp <= 0xff60) ||
      (cp >= 0xffe0 && cp <= 0xffe6) || (cp >= 0x1f300 && cp <= 0x1faff) ||
      (cp >= 0x20000 && cp <= 0x3fffd)
}
