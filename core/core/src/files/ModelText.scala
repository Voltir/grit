package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.host.Clipped
import grit.prose.form.{Block, Doc, Span}
import grit.prose.markdown.Markdown

/** A file's tree as the model reads it, a range at a time. */
object ModelText {

  /** The most bytes of UTF-8 a read's [[FileRead.shown]] takes: 1 KiB under
    * [[Clipped.MaxBytes]], room for the lines a tool puts before it naming the file.
    */
  val Room: Int = Clipped.MaxBytes - 1024

  /** The most bytes of UTF-8 an outline takes: 8 KiB. A longer one keeps its shallowest levels
    * that fit, and when its top level alone does not, its first headings, saying how many more
    * there are.
    */
  val OutlineMax: Int = 8 * 1024

  /** `range` of `tree`, its [[FileRead.shown]] within [[Room]]: whole blocks, one that does not
    * fit alone cut at a line (a line that does not fit alone, at a character), `next` reading
    * on from where it was cut; a page's first block after a `[page n]` line (`[pages n–m]` for
    * a block that crosses pages); pages past the end, the last page; pages of a file not
    * counted in pages, its outline, saying so; a section not found, its outline with
    * "no section '<heading>'"; near an anchor, the page its passage lies on now ([[Anchor.find]]),
    * or in a file not counted in pages from its block on, and when it is gone, the outline with
    * "That passage is gone from the file now.". `covered` names its pages ("pages 212–214 of 492") or, for a
    * file not counted in pages, its characters, and adds what `tree.covers` and `tree.missed`
    * say. A heading's path is its own text after its enclosing headings' each followed by
    * `" > "`, matched ignoring case and runs of spaces.
    */
  def of(tree: Understood, range: FileRange): FileRead = {
    val laid = Laid(tree.doc, tree.covers == Covers.Whole)
    val outline = laid.outline
    def told(covered: String): String = {
      val more = tree.covers.words.toVector ++
        Option.when(tree.missed.nonEmpty)(
          s"not read here: ${tree.missed.map(_.what).mkString(", ")}"
        )
      (covered +: more).mkString("; ")
    }
    def outlineOnly(said: String): FileRead =
      FileRead(said, told("its outline"), None, Some(outline.getOrElse("(no headings)")), false)
    def pages(a: Int, b: Int): FileRead = {
      val to = b.min(laid.pages)
      val from = a.min(to)
      val (first, end) = laid.pageBlocks(from, to)
      laid.fit(
        laid.start(first),
        end,
        None,
        told,
        stop = Option.when(to < laid.pages)((to + 1, (to + 1 + (to - from)).min(laid.pages)))
      )
    }
    range match {
      case FileRange.Section(heading) =>
        laid.section(heading) match {
          case None => outlineOnly(s"There is no section '$heading'.")
          case Some((from, to)) => laid.fit(laid.start(from), to, None, told, stop = None)
        }
      case FileRange.Pages(a, b) =>
        if (laid.pages == 0)
          outlineOnly("It is not counted in pages: ask for a section, or read on from an offset.")
        else pages(a, b)
      case FileRange.From(offset) =>
        laid.fit(offset.min(laid.total), laid.blocks.size, None, told, None)
      case FileRange.Near(anchor) =>
        Anchor.find(anchor, laid.whole) match {
          case None => outlineOnly("That passage is gone from the file now.")
          case Some((at, _)) =>
            val block = laid.blockAt(at)
            laid.spans.lift(block).flatten.map(_._1).orElse(laid.onPage.lift(block).flatten) match {
              case Some(page) => pages(page, page)
              case None => laid.fit(laid.start(block), laid.blocks.size, None, told, None)
            }
        }
      case FileRange.Start => laid.fit(0, laid.blocks.size, outline, told, None)
    }
  }

  /** All of `tree` as the model reads it, its blocks as markdown apart by a blank line, their
    * notes dropped: the text a [[FileRange.From]] offset and an [[Anchor]] count in.
    */
  def text(tree: Understood): String = Laid(tree.doc, readWhole = false).whole

  private def bytes(s: String): Int = s.getBytes(UTF_8).length

  /** `doc`'s blocks laid out as markdown, each at its character offset, with its pages. */
  private[files] final case class Laid(doc: Doc[Locus], readWhole: Boolean) {

    val blocks: Vector[Block[Locus]] = doc.blocks

    val texts: Vector[String] =
      blocks.map(b => Markdown.write(Doc(Vector(b)).mapSpans(s => Span(s.text, s.marks))))

    /** Each block's first character in the whole text, blocks apart by a blank line. */
    val starts: Vector[Int] = texts.scanLeft(0)((at, t) => at + t.length + 2).dropRight(1)

    /** The whole text. */
    lazy val whole: String = texts.mkString("\n\n")

    val total: Int = texts.lastOption.fold(0)(t => starts.lastOption.getOrElse(0) + t.length)

    /** Each block's first and last page, `None` for a block with none. */
    val spans: Vector[Option[(Int, Int)]] = blocks.map { b =>
      val ps = Doc(Vector(b)).notes.collect { case Locus.Page(n) => n }
      Option.when(ps.nonEmpty)((ps.min, ps.max))
    }

    val pages: Int = spans.flatten.map(_._2).maxOption.getOrElse(0)

    /** The page each block lies on or after: its own first, else the last before it. */
    val onPage: Vector[Option[Int]] =
      spans.scanLeft(Option.empty[Int])((last, s) => s.map(_._2).orElse(last)).tail

    /** Each heading: its block, level, text and path. */
    val headings: Vector[(Int, Int, String, String)] =
      blocks.zipWithIndex
        .collect { case (Block.Heading(level, text), i) => (i, level.value, text.plain.trim) }
        .foldLeft((Vector.empty[(Int, Int, String, String)], List.empty[(Int, String)])) {
          case ((done, stack), (i, level, text)) =>
            val above = stack.dropWhile(_._1 >= level)
            val path = (above.reverse.map(_._2) :+ text).mkString(" > ")
            (done :+ (i, level, text, path), (level, text) :: above)
        }
        ._1

    /** The block the character `offset` lies in, or the next one when it lies between. */
    def blockAt(offset: Int): Int = {
      val i = starts.lastIndexWhere(_ <= offset)
      if (i < 0) 0 else if (offset > starts(i) + texts(i).length) i + 1 else i
    }

    def start(block: Int): Int = starts.lift(block).getOrElse(total)

    def section(heading: String): Option[(Int, Int)] = {
      val want = norm(heading)
      headings.find(h => norm(h._4).endsWith(want)).map { case (i, level, _, _) =>
        val end = headings.collectFirst { case (j, l, _, _) if j > i && l <= level => j }
        (i, end.getOrElse(blocks.size))
      }
    }

    /** The blocks touching pages `from` to `to`: the first index, and the index past the last. */
    def pageBlocks(from: Int, to: Int): (Int, Int) = {
      val first = spans.indexWhere(_.exists(_._2 >= from))
      val last = spans.lastIndexWhere(_.exists(_._1 <= to))
      if (first < 0 || last < first) (blocks.size, blocks.size) else (first, last + 1)
    }

    lazy val outline: Option[String] =
      Option.when(headings.nonEmpty) {
        def line(h: (Int, Int, String, String)): String =
          "  " * (h._2 - 1).max(0) + "- " + h._3 + onPage(h._1).fold("")(p => s" (page $p)")
        val levels = headings.map(_._2).distinct.sorted
        levels.reverse
          .map(deepest => headings.filter(_._2 <= deepest).map(line).mkString("\n"))
          .find(bytes(_) <= OutlineMax)
          .getOrElse {
            val top = headings.filter(_._2 <= levels.headOption.getOrElse(1)).map(line)
            val kept = top.scanLeft(0)(_ + bytes(_) + 1).tail.takeWhile(_ <= OutlineMax - 64).size
            (top.take(kept) :+ s"… and ${top.size - kept} more headings").mkString("\n")
          }
      }

    /** The text from character `from` up to block `end`, as much as fits; `stop`, the pages to
      * read next when all of it fits.
      */
    def fit(
        from: Int,
        end: Int,
        outline: Option[String],
        told: String => String,
        stop: Option[(Int, Int)]
    ): FileRead = {
      val reserve = 512 + outline.fold(0)(o => bytes(o) + 16)
      val room = Room - reserve
      val first = blockAt(from)
      // What is shown, block by block: (its text, the character past it in the whole text).
      var shown = Vector.empty[String]
      var used = 0
      var at = from
      var page = if (first > 0) onPage(first - 1) else None
      var i = first
      var full = false
      while (!full && i < end.min(blocks.size)) {
        val skip = (at - starts(i)).max(0)
        val body = texts(i).drop(skip)
        val marker = spans(i) match {
          case Some((p, q)) if page.forall(_ < q) =>
            val from = page.fold(p)(seen => (seen + 1).max(p))
            Some(if (from == q) s"[page $q]" else s"[pages $from–$q]")
          case _ => None
        }
        val piece = (marker.toVector :+ body).mkString("\n")
        val size = bytes(piece) + 2
        if (used + size <= room) {
          shown :+= piece; used += size; at = starts(i) + texts(i).length + 2
          spans(i).foreach(s => page = Some(s._2)); i += 1
        } else {
          if (shown.isEmpty) {
            // The first block alone does not fit: its lines that do, or its first line's start.
            val lines = Clipped.lines(body)
            val keep = lines.scanLeft(0)(_ + bytes(_) + 1).tail.takeWhile(_ <= room - 64).size
            val kept =
              if (keep > 0) lines.take(keep).mkString("\n")
              else Clipped.prefix(lines.headOption.getOrElse(""), room - 64)
            shown :+= (marker.toVector :+ kept).mkString("\n")
            at = starts(i) + skip + kept.length + (if (keep > 0) 1 else 0)
          }
          full = true
        }
      }
      val text = shown.mkString("\n\n")
      val done = at.min(total)
      val shownPages = (first until i.max(first + 1)).flatMap(spans.lift(_).flatten)
      val covered =
        if (pages > 0 && shownPages.nonEmpty) {
          val (p, q) = (shownPages.map(_._1).min, shownPages.map(_._2).max)
          (if (p == q) s"page $p" else s"pages $p–$q") + s" of $pages"
        } else if (from == 0 && done >= total) "all of it"
        else s"characters $from–$done of $total"
      val next =
        if (full) FileRange.from(done) else stop.flatMap((a, b) => FileRange.pages(a, b))
      FileRead(
        text,
        told(covered),
        next,
        outline,
        readWhole && from == 0 && !full && i >= blocks.size
      )
    }
  }

  private def norm(s: String): String = s.trim.replaceAll("\\s+", " ").toLowerCase
}
