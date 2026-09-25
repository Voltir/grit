package grit.prose.markdown

import scala.util.matching.Regex

import grit.prose.form.{Block, Item, Level, Text}

/** Markdown's block syntax, read line by line into blocks: ATX headings, fenced code,
  * thematic breaks, block quotes, bullet and numbered lists (nested by indentation),
  * pipe tables, and paragraphs. A line break inside a paragraph is kept. Not supported,
  * and so read as paragraphs: setext headings (they would change a line's meaning after
  * it had been shown), indented code blocks, HTML.
  */
private[markdown] object Blocks {

  /** `lines` as blocks. When `open`, they end where a reply still arriving has got to, and
    * the last block's inline text is read as unfinished ([[Inlines.parse]]).
    */
  def parse(lines: Vector[String], open: Boolean): Vector[Block] = {
    val n = lines.length
    val out = Vector.newBuilder[Block]

    /** Whether a leaf whose lines end before `end` is the last thing in `lines`. */
    def atEnd(end: Int): Boolean =
      open && n - end <= 1 && (end until n).forall(k => blank(lines(k)))

    var i = 0
    while (i < n) {
      val line = lines(i)
      if (blank(line)) { i += 1 }
      else
        fence(line) match {
          case Some((indent, marker, language)) =>
            var j = i + 1
            while (j < n && !closes(lines(j), marker)) j += 1
            out += Block.Code(language, lines.slice(i + 1, j).map(dropColumns(_, indent)))
            i = math.min(n, j + 1)
          case None =>
            if (Rule.matches(line)) {
              out += Block.Rule
              i += 1
            } else
              heading(line) match {
                case Some((level, text)) =>
                  out += Block.Heading(Level.of(level), Inlines.parse(text, atEnd(i + 1)))
                  i += 1
                case None =>
                  if (quoted(line).nonEmpty) {
                    var j = i
                    val inner = Vector.newBuilder[String]
                    var more = true
                    while (j < n && more) {
                      quoted(lines(j)) match {
                        case Some(rest) => inner += rest; j += 1
                        case None => more = false
                      }
                    }
                    out += Block.Quote(parse(inner.result(), atEnd(j)))
                    i = j
                  } else
                    marker(line) match {
                      case Some(first) =>
                        val (list, next) = items(lines, i, first, open)
                        out += list
                        i = next
                      case None =>
                        if (i + 1 < n && line.contains('|') && Delimiter.matches(lines(i + 1))) {
                          var j = i + 2
                          while (j < n && !blank(lines(j)) && lines(j).contains('|')) j += 1
                          out += Block.Table(
                            cells(line, open = false),
                            lines.slice(i + 2, j).map(cells(_, open = false))
                          )
                          i = j
                        } else {
                          var j = i + 1
                          while (j < n && !blank(lines(j)) && !interrupts(lines(j))) j += 1
                          val text = lines
                            .slice(i, j)
                            .map(l => dropColumns(l, indentOf(l)).stripTrailing().stripSuffix("\\"))
                            .mkString("\n")
                          out += Block.Paragraph(Inlines.parse(text, atEnd(j)))
                          i = j
                        }
                    }
              }
        }
    }
    out.result()
  }

  /** A list item's marker line: where the marker starts, whether it counts, its number,
    * and the text after it and the column that text starts at.
    */
  private final case class Marker(
      indent: Int,
      ordered: Boolean,
      number: Int,
      text: String,
      content: Int
  )

  /** The list starting at line `i`, whose first marker is `first`, and the line after it. */
  private def items(
      lines: Vector[String],
      i: Int,
      first: Marker,
      open: Boolean
  ): (Block, Int) = {
    val n = lines.length
    val out = Vector.newBuilder[Item]
    var k = i
    var current: Option[Marker] = Some(first)
    while (current.nonEmpty) {
      current match {
        case None => ()
        case Some(m) =>
          val body = Vector.newBuilder[String]
          body += m.text
          k += 1
          var going = true
          while (k < n && going) {
            val l = lines(k)
            if (blank(l)) {
              // A blank line stays in the item only when the item goes on after it.
              var q = k
              while (q < n && blank(lines(q))) q += 1
              if (q < n && indentOf(lines(q)) > m.indent) {
                (k until q).foreach(_ => body += "")
                k = q
              } else going = false
            } else if (indentOf(l) > m.indent) {
              body += dropColumns(l, math.min(indentOf(l), m.content))
              k += 1
            } else if (marker(l).isEmpty && !interrupts(l)) {
              // A lazy continuation of the item's paragraph.
              body += l.trim
              k += 1
            } else going = false
          }
          out += Item(
            parse(body.result(), open && (k until n).forall(q => blank(lines(q))) && n - k <= 1)
          )
          // The next item, if the list goes on: blank lines between items are allowed.
          var q = k
          while (q < n && blank(lines(q))) q += 1
          current = if (q < n) marker(lines(q)).filter(_.ordered == first.ordered) else None
          if (current.nonEmpty) k = q
      }
    }
    val list =
      if (first.ordered) Block.Numbered(first.number, out.result())
      else Block.Bullets(out.result())
    (list, k)
  }

  private val Fence: Regex = """^( {0,3})(`{3,}|~{3,})[ \t]*([^`\s]*)[^`]*$""".r
  private val Atx: Regex = """^ {0,3}(#{1,6})(?:[ \t]+(.*?))?(?:[ \t]+#+)?[ \t]*$""".r
  private val Rule: Regex = """^ {0,3}(?:(?:\*[ \t]*){3,}|(?:-[ \t]*){3,}|(?:_[ \t]*){3,})$""".r
  private val Quote: Regex = """^ {0,3}> ?(.*)$""".r
  private val ListItem: Regex = """^([ \t]*)([-*+]|\d{1,9}[.)])(?:([ \t]+)(.*))?$""".r

  /** A table's delimiter row, `|---|:--:|`. */
  val Delimiter: Regex = """^[ \t]*\|?[ \t]*:?-+:?[ \t]*(?:\|[ \t]*:?-+:?[ \t]*)*\|?[ \t]*$""".r

  /** An opening fence: its indent, its fence string, and the language tag after it. */
  private def fence(line: String): Option[(Int, String, Option[String])] =
    Fence.findFirstMatchIn(line).map { m =>
      (m.group(1).length, m.group(2), Option(m.group(3)).filter(_.nonEmpty))
    }

  /** Whether `line` closes a fence opened by `marker`. */
  private def closes(line: String, marker: String): Boolean = {
    val t = line.trim
    indentOf(line) <= 3 && t.length >= marker.length && marker.headOption.exists(c =>
      t.forall(_ == c)
    )
  }

  private def heading(line: String): Option[(Int, String)] =
    Atx.findFirstMatchIn(line).map(m => (m.group(1).length, Option(m.group(2)).getOrElse("")))

  private def quoted(line: String): Option[String] =
    Quote.findFirstMatchIn(line).map(_.group(1))

  private def marker(line: String): Option[Marker] =
    if (Rule.matches(line)) None
    else
      ListItem.findFirstMatchIn(line).map { m =>
        val indent = columns(m.group(1))
        val mark = m.group(2)
        val gap = Option(m.group(3)).map(columns).getOrElse(0)
        val text = Option(m.group(4)).getOrElse("")
        val ordered = mark.endsWith(".") || mark.endsWith(")")
        val number = if (ordered) mark.dropRight(1).toIntOption.getOrElse(1) else 0
        // A marker followed by more than four spaces starts its text one space in; the
        // rest belongs to the text.
        val content =
          if (text.isEmpty || gap > 4) indent + mark.length + 1 else indent + mark.length + gap
        val body = if (gap > 4) " " * (gap - 1) + text else text
        Marker(indent, ordered, number, body, content)
      }

  /** Whether `line` starts a block that ends a paragraph above it. A numbered item
    * interrupts only if it starts at 1, so a line that begins with a year does not.
    */
  private def interrupts(line: String): Boolean =
    fence(line).nonEmpty || heading(line).nonEmpty || Rule.matches(line) ||
      quoted(line).nonEmpty || marker(line).exists(m => !m.ordered || m.number == 1)

  /** A table row's cells, split at unescaped pipes, the outer pipes dropped. */
  private def cells(line: String, open: Boolean): Vector[Text] = {
    val t = line.trim.stripPrefix("|")
    val body = if (t.endsWith("|") && !t.endsWith("\\|")) t.dropRight(1) else t
    val out = Vector.newBuilder[String]
    val cur = new StringBuilder
    var k = 0
    while (k < body.length) {
      val c = body.charAt(k)
      if (c == '\\' && k + 1 < body.length && body.charAt(k + 1) == '|') {
        cur += '|'
        k += 2
      } else if (c == '|') {
        out += cur.toString
        cur.clear()
        k += 1
      } else {
        cur += c
        k += 1
      }
    }
    out += cur.toString
    out.result().map(c => Inlines.parse(c.trim, open))
  }

  private def blank(line: String): Boolean = line.forall(_.isWhitespace)

  /** The columns `ws` spans, a tab counting to the next multiple of four. */
  private def columns(ws: String): Int =
    ws.foldLeft(0)((col, c) => if (c == '\t') col + 4 - col % 4 else col + 1)

  /** The column the first non-blank character of `line` sits at. */
  private def indentOf(line: String): Int = columns(line.takeWhile(c => c == ' ' || c == '\t'))

  /** `line` with up to `cols` columns of leading whitespace removed. */
  private def dropColumns(line: String, cols: Int): String = {
    var col = 0
    var k = 0
    while (k < line.length && col < cols && (line.charAt(k) == ' ' || line.charAt(k) == '\t')) {
      col = if (line.charAt(k) == '\t') col + 4 - col % 4 else col + 1
      k += 1
    }
    line.substring(k)
  }
}
