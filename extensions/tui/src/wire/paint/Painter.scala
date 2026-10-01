package grit.tui.wire.paint

import grit.tui.model.surface.*

/** Turns a Frame into the bytes that paint it, diffed against the frame the terminal
  * already shows.
  *
  * The rules this exists to enforce (README, "rules the terminal taught us"): every
  * line addressed absolutely, one write per frame wrapped in `?2026`, SGR only when the
  * style actually changes, and the cursor stated only when the frame says something new
  * about it. Rule 2 -- never write into the terminal's last column -- is *not* enforced
  * here, on purpose: the frame a runtime hands over is already the paintable screen
  * ([[grit.tui.model.surface.Size.screen]] translated it at the boundary), so a frame's last
  * column is a column the terminal is owed back and the painter simply writes the frame
  * it is given. Clipping here as well would silently drop the last column of every
  * frame -- a guaranteed off-by-one, not a safety net.
  *
  * The column the frame leaves out still has to be *coloured*: left alone it keeps the
  * terminal's own background, a stripe down the right edge of any app with a ground of
  * its own, and whatever glyphs were there before a resize. So a row whose last cell is
  * painted ends with that cell's background and an erase to the end of the line, which
  * fills the owed column with no glyph, no cursor move and no deferred wrap (background
  * colour erase, which every terminal grit.tui targets performs).
  */
object Painter {

  /** The bytes that paint `frame` over `prev` (None: the terminal's contents are
    * unknown -- paint everything). The result is one write: a single string.
    */
  def paint(frame: Frame, prev: Option[Frame]): String = {
    val surface = frame.surface
    val size = surface.size
    val paintCols = size.cols
    val full = prev match {
      case None => true
      case Some(p) => p.surface.size != size
    }
    val prevSurface = prev.map(_.surface)

    def changed(row: Int, col: Int): Boolean = prevSurface match {
      case Some(p) if !full => p.at(row, col) != surface.at(row, col)
      case _ => true
    }

    val prevCursor = prev.flatMap(_.cursor)
    val cursorChanged = full || frame.cursor != prevCursor

    // The body is built before the wrapper so a frame with nothing to say can say
    // nothing at all: zero bytes, not an empty `?2026` pair (a widget tick that
    // changed nothing must write zero bytes).
    val body = new StringBuilder

    var current: Option[Style] = None
    var row = 0
    while (row < size.rows) {
      var col = 0
      while (col < paintCols) {
        if (changed(row, col)) {
          val style = surface.at(row, col).style
          // extend the run while the next cell is also changed and shares the style
          var end = col + 1
          while (end < paintCols && changed(row, end) && surface.at(row, end).style == style)
            end += 1
          body ++= Ansi.cup(row + 1, col + 1)
          Ansi.sgr(style, current) match {
            case Some(seq) =>
              body ++= seq
              current = Some(style)
            case None => ()
          }
          var i = col
          while (i < end) {
            body += surface.at(row, i).ch
            i += 1
          }
          if (end == paintCols) {
            val trailing = Painter.trailing(style)
            Ansi.sgr(trailing, current).foreach { seq =>
              body ++= seq
              current = Some(trailing)
            }
            body ++= Ansi.eraseLine
          }
          col = end
        } else col += 1
      }
      row += 1
    }

    if (cursorChanged) {
      frame.cursor match {
        case Some(p) =>
          body ++= Ansi.cursorShow
          body ++= Ansi.cup(p.row + 1, p.col + 1)
        case None =>
          body ++= Ansi.cursorHide
      }
    }

    if (body.isEmpty) ""
    else {
      val sb = new StringBuilder
      sb ++= Ansi.syncStart
      sb ++= body
      sb ++= Ansi.syncEnd
      sb.result()
    }
  }

  /** What the erase after a row's last cell fills the terminal's owed column with: the
    * background that cell shows, and nothing else (an erase draws no glyph, so a
    * foreground or a weight would mean nothing there).
    */
  private def trailing(last: Style): Style =
    Style(bg = if (last.reverse) last.fg else last.bg)
}
