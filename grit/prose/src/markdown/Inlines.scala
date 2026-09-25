package grit.prose.markdown

import grit.prose.form.{Mark, Span, Text}

/** Markdown's inline syntax read into marked text: code spans, `**strong**`/`__strong__`,
  * `*emphasis*`/`_emphasis_`, links, images (as links to them), `<url>` autolinks and
  * backslash escapes. Anything else is literal. Not CommonMark's full delimiter algorithm:
  * an opener takes the first closer that can close it.
  */
private[markdown] object Inlines {

  /** `s` as marked text. When `open`, `s` ends where a reply still arriving has got to:
    * an opener with no closer yet runs to the end, and a trailing delimiter that may be
    * about to open or close something is held back, so what is shown does not change
    * shape when the rest arrives. Otherwise an unclosed opener is literal.
    */
  def parse(s: String, open: Boolean): Text = Text(merged(spans(s, 0, s.length, Set.empty, open)))

  private def spans(
      s: String,
      from: Int,
      until: Int,
      marks: Set[Mark],
      open: Boolean
  ): Vector[Span] = {
    val out = Vector.newBuilder[Span]
    val lit = new StringBuilder
    def flush(): Unit =
      if (lit.nonEmpty) { out += Span(lit.toString, marks); lit.clear() }
    var i = from
    while (i < until) {
      val c = s.charAt(i)
      if (c == '\\' && i + 1 < until && isPunct(s.charAt(i + 1))) {
        lit += s.charAt(i + 1)
        i += 2
      } else if (c == '\\' && i + 1 == until && open) { i = until }
      else if (c == '`') {
        val n = run(s, i, until, c)
        ticksClosing(s, i + n, until, n) match {
          case Some(j) =>
            flush()
            out += Span(codeText(s.substring(i + n, j)), marks + Mark.Code)
            i = j + n
          case None if open =>
            flush()
            val rest = s.substring(i + n, until)
            if (rest.nonEmpty) out += Span(codeText(rest), marks + Mark.Code)
            i = until
          case None =>
            lit ++= s.substring(i, i + n)
            i += n
        }
      } else if (c == '*' || c == '_') {
        val n = run(s, i, until, c)
        if (open && i + n == until) { i = until }
        else if (canOpen(s, i, n, until, c)) {
          val want = math.min(n, 3)
          lit ++= c.toString * (n - want)
          val at = i + n - want
          closer(s, at + want, until, c, want) match {
            case Some(j) =>
              flush()
              out ++= spans(s, at + want, j, marks ++ markOf(want), open = false)
              i = j + want
            case None if open =>
              flush()
              out ++= spans(s, at + want, until, marks ++ markOf(want), open = true)
              i = until
            case None =>
              lit ++= c.toString * want
              i = at + want
          }
        } else {
          lit ++= s.substring(i, i + n)
          i += n
        }
      } else if (c == '[' || (c == '!' && i + 1 < until && s.charAt(i + 1) == '[')) {
        val bracket = if (c == '!') i + 1 else i
        link(s, bracket, until) match {
          case Link.Whole(textEnd, href, next) =>
            flush()
            out ++= spans(s, bracket + 1, textEnd, marks + Mark.Link(href), open = false)
            i = next
          case Link.Partial(textEnd) if open =>
            // The link is still arriving: its text so far, and no URL until it is whole.
            flush()
            out ++= spans(s, bracket + 1, textEnd, marks, open = true)
            i = until
          case Link.Partial(_) | Link.Not =>
            if (open && bracket + 1 <= until && !s.substring(bracket, until).contains("]")) {
              // An unclosed `[` at the tail may yet be a link: held, its text shown.
              i = bracket + 1
            } else {
              lit += c
              i += 1
            }
        }
      } else if (c == '<') {
        autolink(s, i, until) match {
          case Some((url, next)) =>
            flush()
            out += Span(url, marks + Mark.Link(url))
            i = next
          case None =>
            lit += c
            i += 1
        }
      } else {
        lit += c
        i += 1
      }
    }
    flush()
    out.result()
  }

  private enum Link {

    /** `[text](href)`: the text ends at `textEnd`, and the whole ends before `next`. */
    case Whole(textEnd: Int, href: String, next: Int)

    /** `[text]` or `[text](partial` running to the end: the text ends at `textEnd`. */
    case Partial(textEnd: Int)
    case Not
  }

  /** The link whose `[` is at `i`, if one is. */
  private def link(s: String, i: Int, until: Int): Link =
    matching(s, i, until, '[', ']') match {
      case None => Link.Not
      case Some(close) =>
        if (close + 1 == until) Link.Partial(close)
        else if (s.charAt(close + 1) != '(') Link.Not
        else
          matching(s, close + 1, until, '(', ')') match {
            case Some(end) =>
              val inner = s.substring(close + 2, end).trim
              val href = inner.takeWhile(ch => !ch.isWhitespace).stripPrefix("<").stripSuffix(">")
              Link.Whole(close, href, end + 1)
            case None => Link.Partial(close)
          }
    }

  /** The index of the `closeCh` that balances the `openCh` at `i`, skipping escapes. */
  private def matching(s: String, i: Int, until: Int, openCh: Char, closeCh: Char): Option[Int] = {
    var depth = 0
    var k = i
    var found: Option[Int] = None
    while (k < until && found.isEmpty) {
      val ch = s.charAt(k)
      if (ch == '\\') k += 1
      else if (ch == openCh) depth += 1
      else if (ch == closeCh) {
        depth -= 1
        if (depth == 0) found = Some(k)
      }
      k += 1
    }
    found
  }

  /** `<http://…>` at `i`: the URL, and where it ends. */
  private def autolink(s: String, i: Int, until: Int): Option[(String, Int)] = {
    val end = s.indexOf('>', i)
    if (end < 0 || end >= until) None
    else {
      val url = s.substring(i + 1, end)
      Option.when(
        (url.startsWith("http://") || url.startsWith("https://") || url.startsWith("mailto:")) &&
          !url.exists(_.isWhitespace)
      )((url, end + 1))
    }
  }

  /** Where the run of exactly `n` backticks closing a code span begins, if one does. */
  private def ticksClosing(s: String, from: Int, until: Int, n: Int): Option[Int] = {
    var k = from
    var found: Option[Int] = None
    while (k < until && found.isEmpty) {
      if (s.charAt(k) == '`') {
        val r = run(s, k, until, '`')
        if (r == n) found = Some(k) else k += r
      } else k += 1
    }
    found
  }

  /** Where the run of `want` `c`s that closes an opener begins, searching from `from`;
    * code spans are skipped, and so is a run that cannot close.
    */
  private def closer(s: String, from: Int, until: Int, c: Char, want: Int): Option[Int] = {
    var k = from
    var found: Option[Int] = None
    while (k < until && found.isEmpty) {
      val ch = s.charAt(k)
      if (ch == '\\') k += 2
      else if (ch == '`') {
        val r = run(s, k, until, '`')
        k = ticksClosing(s, k + r, until, r).fold(k + r)(_ + r)
      } else if (ch == c) {
        val r = run(s, k, until, c)
        val closes =
          k > from && !s.charAt(k - 1).isWhitespace &&
            (c != '_' || k + r >= until || !s.charAt(k + r).isLetterOrDigit)
        if (closes && r == want) found = Some(k)
        else if (closes && r == 3 && want < 3) found = Some(k + 3 - want)
        else k += r
      } else k += 1
    }
    found
  }

  /** Whether the run of `n` `c`s at `i` can open emphasis: followed by text, and for `_`
    * not inside a word.
    */
  private def canOpen(s: String, i: Int, n: Int, until: Int, c: Char): Boolean =
    i + n < until && !s.charAt(i + n).isWhitespace &&
      (c != '_' || i == 0 || !s.charAt(i - 1).isLetterOrDigit)

  private def markOf(n: Int): Set[Mark] = n match {
    case 1 => Set(Mark.Emphasis)
    case 2 => Set(Mark.Strong)
    case _ => Set(Mark.Strong, Mark.Emphasis)
  }

  /** How many `c`s run from `i`. */
  private def run(s: String, i: Int, until: Int, c: Char): Int = {
    var k = i
    while (k < until && s.charAt(k) == c) k += 1
    k - i
  }

  /** A code span's content: line breaks read as spaces, and one space trimmed from each
    * end when both have one (so a span can start or end with a backtick).
    */
  private def codeText(raw: String): String = {
    val t = raw.replace('\n', ' ')
    if (t.length >= 2 && t.startsWith(" ") && t.endsWith(" ") && t.exists(_ != ' '))
      t.substring(1, t.length - 1)
    else t
  }

  private def isPunct(c: Char): Boolean =
    c < 128 && "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".contains(c)

  /** Adjacent spans with the same marks joined, empty ones dropped. */
  private def merged(in: Vector[Span]): Vector[Span] =
    in.foldLeft(Vector.empty[Span]) { (acc, s) =>
      if (s.text.isEmpty) acc
      else
        acc.lastOption match {
          case Some(last) if last.marks == s.marks =>
            acc.dropRight(1) :+ Span(last.text + s.text, s.marks)
          case _ => acc :+ s
        }
    }
}
