package grit.formats.ooxml

import javax.xml.stream.XMLStreamConstants.{END_ELEMENT, START_ELEMENT}
import javax.xml.stream.XMLStreamReader

import scala.collection.mutable

import grit.core.files.{
  Covers,
  Format,
  FormatName,
  Kind,
  Limit,
  Locus,
  Missed,
  OpenFile,
  ReadLimits,
  Understood,
  Unread
}
import grit.formats.limits.Guard
import grit.formats.ooxml.Package.{attr, isText}
import grit.formats.tree.Blocks.{list, table}
import grit.formats.tree.Levelled
import grit.prose.form.{Block, Cell, Doc, Level, Mark, Span, Text}

/** Word documents (`.docx`, `.docm`), transitional or strict, read over the JDK's zip and StAX:
  * paragraphs, headings by style (or a style based on one, or an outline level), lists with
  * the numbers Word shows, tables (as many columns as their widest row), links, text boxes
  * after their paragraph, footnotes and endnotes as an end section referenced `[n]`, and page
  * headers and footers in a section after it. Tracked deletions and a move's source are not
  * read; comments are [[Missed]]. Its tree is not counted in pages: a stopped read
  * [[Covers.Start]].
  */
object Docx extends Format {

  val name: FormatName = FormatName("docx")
  val version: Int = 1

  private val Main = "word/document.xml"

  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] = {
    val blocks = mutable.ArrayBuffer.empty[Block[Locus]]
    val missed = mutable.ArrayBuffer.empty[Missed]
    var opened: Either[Unread, Unit] = Right(())
    var read = false
    val stopped = Guard.until(file, limits) { count =>
      opened = Package.open(file, limits, Kind.Word, Main, count) { parts =>
        read = true
        val headings = parts.xml("word/styles.xml")(styles(_, count)).getOrElse(Map.empty)
        val lists = new WordLists
        parts.xml("word/numbering.xml")(lists.read(_, count))
        val links = parts.xml("word/_rels/document.xml.rels")(rels(_, count)).getOrElse(Map.empty)
        val notes = Vector("footnotes" -> "f", "endnotes" -> "e").flatMap { (part, kind) =>
          parts.xml(s"word/$part.xml")(notesOf(_, kind, count)).getOrElse(Vector.empty)
        }.toMap
        val pages = parts.names
          .filter(_.matches("word/(header|footer)\\d*\\.xml"))
          .sorted
          .flatMap(n => parts.xml(n)(notesOf(_, "page", count)).getOrElse(Vector.empty).map(_._2))
          .filter(_.nonEmpty)
          .distinct
        if (parts.xml("word/comments.xml")(notesOf(_, "c", count)).exists(_.nonEmpty))
          missed += Missed("comments")
        val referenced = mutable.LinkedHashMap.empty[String, Int]
        // The end sections are written whether or not the body was read whole.
        try parts.xml(Main)(body(_, count, headings, lists, links, referenced, blocks))
        finally {
          if (referenced.nonEmpty) {
            blocks += Block.Heading(Level.of(2), Text.plain("Notes"))
            referenced.foreach { (id, n) =>
              blocks += Block.Paragraph(Text.plain(s"[$n] ${notes.getOrElse(id, "")}".trim))
            }
          }
          if (pages.nonEmpty) {
            blocks += Block.Heading(Level.of(2), Text.plain("Page headers and footers"))
            pages.foreach(t => blocks += Block.Paragraph(Text.plain(t)))
          }
        }
      }
    }
    opened.flatMap { _ =>
      if (!read) Left(Unread.Damaged(Kind.Word))
      else if (blocks.isEmpty && stopped.contains(Limit.Time)) Left(Unread.TooSlow)
      else if (blocks.isEmpty && stopped.contains(Limit.Damaged)) Left(Unread.Damaged(Kind.Word))
      else
        Right(
          Understood(
            Doc(blocks.toVector),
            stopped.fold(Covers.Whole)(Covers.Start(_)),
            missed.toVector,
            name,
            version
          )
        )
    }
  }

  /** Each style's heading level: by name ("heading 1" to "heading 9", "title"), by outline
    * level, else by the style it is based on.
    */
  private def styles(r: XMLStreamReader, count: Guard.Count^): Map[String, Int] = {
    val level = mutable.Map.empty[String, Int]
    val basedOn = mutable.Map.empty[String, String]
    var id: Option[String] = None
    while (r.hasNext) {
      count.step()
      if (r.next() == START_ELEMENT) r.getLocalName match {
        case "style" => id = attr(r, "styleId")
        case "basedOn" => for { s <- id; b <- attr(r, "val") } basedOn(s) = b
        case "name" =>
          val n = attr(r, "val").getOrElse("").toLowerCase
          id.foreach { s =>
            if (n.matches("heading [1-9]")) level(s) = n.last - '0'
            else if (n == "title") level(s) = 1
          }
        case "outlineLvl" =>
          for { s <- id; l <- attr(r, "val").flatMap(_.toIntOption) if l < 9 }
            level.getOrElseUpdate(s, l + 1)
        case _ => ()
      }
    }
    basedOn.keys.foreach { s =>
      val inherited = Iterator
        .iterate(Option(s))(_.flatMap(basedOn.get))
        .take(20)
        .takeWhile(_.nonEmpty)
        .flatten
        .flatMap(level.get)
        .nextOption()
      inherited.foreach(l => level.getOrElseUpdate(s, l))
    }
    level.toMap
  }

  /** The external links by relationship id. */
  private def rels(r: XMLStreamReader, count: Guard.Count^): Map[String, String] = {
    val links = mutable.Map.empty[String, String]
    while (r.hasNext) {
      count.step()
      if (
        r.next() == START_ELEMENT && r.getLocalName == "Relationship" && attr(r, "TargetMode")
          .contains("External")
      )
        for { id <- attr(r, "Id"); to <- attr(r, "Target") } links(id) = to
    }
    links.toMap
  }

  /** Each note's (or comment's, or a page header's) text by `kind` and its id, its paragraphs
    * joined by a space; a separator note left out.
    */
  private def notesOf(
      r: XMLStreamReader,
      kind: String,
      count: Guard.Count^
  ): Vector[(String, String)] = {
    val out = mutable.ArrayBuffer.empty[(String, String)]
    val text = new StringBuilder
    var id = ""
    var skip = false
    var inT = false
    while (r.hasNext) {
      count.step()
      val e = r.next()
      if (e == START_ELEMENT) r.getLocalName match {
        case "footnote" | "endnote" | "comment" | "hdr" | "ftr" =>
          id = s"$kind${attr(r, "id").getOrElse("")}"
          skip = attr(r, "type").exists(_ != "normal")
          text.clear()
        case "t" => inT = true
        case "p" => if (text.nonEmpty) text.append(' ')
        case _ => ()
      }
      else if (e == END_ELEMENT) r.getLocalName match {
        case "t" => inT = false
        case "footnote" | "endnote" | "comment" | "hdr" | "ftr" =>
          if (!skip) out += id -> text.toString.trim
        case _ => ()
      }
      else if (inT && isText(e)) count.keep(r.getText, s => { text.append(s); () })
    }
    out.toVector
  }

  /** A paragraph being read: its spans, each a text and the link it is part of, and its
    * properties by local name (`pStyle`, `numId`, `ilvl`, `outlineLvl`).
    */
  private final class Para {
    val spans: mutable.ArrayBuffer[(StringBuilder, Option[String])] = mutable.ArrayBuffer.empty
    val props: mutable.Map[String, String] = mutable.Map.empty

    def add(s: String, link: Option[String]): Unit = {
      if (spans.lastOption.forall(_._2 != link)) spans += ((new StringBuilder, link))
      spans.lastOption.foreach(_._1.append(s))
    }

    /** Its text, leading and trailing spaces trimmed as Word shows them. */
    def text: Text[Nothing] = {
      val all = spans.toVector.map((t, l) =>
        Span(t.toString, l.fold(Set.empty[Mark])(h => Set[Mark](Mark.Link(h))))
      )
      val last = all.size - 1
      Text.of(all.zipWithIndex.map { (s, i) =>
        val start = if (i == 0) s.text.dropWhile(_.isWhitespace) else s.text
        Span(if (i == last) start.reverse.dropWhile(_.isWhitespace).reverse else start, s.marks)
      })
    }
  }

  /** The body of `word/document.xml`, its blocks added to `out` as they end; what a stopped
    * read holds when it stops is added too.
    */
  private def body(
      r: XMLStreamReader,
      count: Guard.Count^,
      headings: Map[String, Int],
      lists: WordLists,
      links: Map[String, String],
      referenced: mutable.LinkedHashMap[String, Int],
      out: mutable.ArrayBuffer[Block[Locus]]
  ): Unit = {
    val paras = mutable.Stack.empty[Para]
    val boxed = mutable.ArrayBuffer.empty[Text[Nothing]]
    val items = mutable.ArrayBuffer.empty[Levelled[Nothing]]
    val rows = mutable.ArrayBuffer.empty[Vector[Text[Nothing]]]
    val row = mutable.ArrayBuffer.empty[Text[Nothing]]
    val cell = mutable.ArrayBuffer.empty[Text[Nothing]]
    val instr = new StringBuilder
    var link: Option[String] = None
    var field: Option[String] = None
    var inT = false
    var inInstr = false
    var checked: Option[Boolean] = None
    var skipping = 0
    var skipped = ""
    var boxes = 0
    var tables = 0
    var span = 1
    var merged = false

    def put(s: String): Unit = paras.headOption.foreach(p => count.keep(s, t => p.add(t, link)))

    def flushList(): Unit =
      if (items.nonEmpty) { out += list(items.toList); items.clear() }

    def flushBoxes(): Unit = { boxed.foreach(t => out += Block.Paragraph(t)); boxed.clear() }

    def note(id: String): Unit = put(s"[${referenced.getOrElseUpdate(id, referenced.size + 1)}]")

    def endRow(): Unit = if (row.nonEmpty) { rows += row.toVector; row.clear() }

    def endTable(): Unit = {
      val cells = rows.toVector.map((r: Vector[Text[Nothing]]) => r.map(t => Cell.Words(t)))
      table[Nothing](cells).foreach(out += _)
      rows.clear()
      flushBoxes()
    }

    def paragraph(p: Para): Unit = {
      val text = p.text
      if (boxes > 0) { if (!text.isEmpty) boxed += text }
      else if (tables > 0) { if (!text.isEmpty) cell += text }
      else if (text.isEmpty) { flushList(); flushBoxes() }
      else {
        val level = p.props
          .get("outlineLvl")
          .flatMap(_.toIntOption)
          .filter(_ < 9)
          .map(_ + 1)
          .orElse(p.props.get("pStyle").flatMap(headings.get))
        val listed = if (level.isEmpty) p.props.get("numId").filter(_ != "0") else None
        listed match {
          case Some(numId) =>
            val ilvl = p.props.get("ilvl").flatMap(_.toIntOption).getOrElse(0)
            items += Levelled(
              ilvl,
              None,
              lists.next(numId, ilvl).fold(text)(l => Text.of(Span.plain(s"$l ") +: text.spans))
            )
          case None =>
            flushList()
            out += level.fold[Block[Locus]](Block.Paragraph(text))(l =>
              Block.Heading(Level.of(l), text)
            )
        }
        flushBoxes()
      }
    }

    def start(): Unit = r.getLocalName match {
      case "Fallback" | "moveFrom" => skipped = r.getLocalName; skipping = 1
      case "p" => paras.push(new Para)
      case prop @ ("pStyle" | "numId" | "ilvl" | "outlineLvl") =>
        for { p <- paras.headOption; v <- attr(r, "val") } p.props(prop) = v
      case "t" => inT = true
      case "tab" => put("\t")
      case "br" | "cr" => put("\n")
      case "instrText" => inInstr = true; instr.clear()
      case "fldChar" =>
        if (attr(r, "fldCharType").contains("end")) {
          field.foreach(u => put(s" ($u)")); field = None
        }
      case "checkBox" => checked = Some(false)
      case "default" | "checked" =>
        checked = checked.map(_ => !attr(r, "val").exists(v => v == "0" || v == "false"))
      case "footnoteReference" => attr(r, "id").foreach(i => note(s"f$i"))
      case "endnoteReference" => attr(r, "id").foreach(i => note(s"e$i"))
      case "hyperlink" => link = attr(r, "id").flatMap(links.get)
      case "txbxContent" => boxes += 1
      case "tbl" =>
        if (tables == 0) { flushList(); flushBoxes(); rows.clear() }
        tables += 1
      case "tr" => if (tables == 1) row.clear()
      case "tc" => if (tables == 1) { cell.clear(); span = 1; merged = false }
      case "gridSpan" =>
        if (tables == 1) span = attr(r, "val").flatMap(_.toIntOption).getOrElse(1).max(1)
      case "vMerge" => if (tables == 1) merged = !attr(r, "val").contains("restart")
      case _ => ()
    }

    def end(): Unit = r.getLocalName match {
      case "t" => inT = false
      case "instrText" =>
        inInstr = false
        """HYPERLINK\s+"([^"]+)"""".r.findFirstMatchIn(instr).foreach(m => field = Some(m.group(1)))
      case "checkBox" => checked.foreach(c => put(if (c) "☒" else "☐")); checked = None
      case "hyperlink" => link = None
      case "txbxContent" => boxes -= 1
      case "p" => if (paras.nonEmpty) paragraph(paras.pop())
      case "tc" =>
        if (tables == 1) {
          row += (if (merged) Text.empty
                  else Text.of(cell.toVector.flatMap(t => Span.plain(" ") +: t.spans).drop(1)))
          (1 until span).foreach(_ => row += Text.empty)
        }
      case "tr" => if (tables == 1) endRow()
      case "tbl" =>
        tables -= 1
        if (tables == 0) endTable()
      case _ => ()
    }

    try {
      while (r.hasNext) {
        count.step()
        val e = r.next()
        if (skipping > 0) {
          if (e == START_ELEMENT && r.getLocalName == skipped) skipping += 1
          else if (e == END_ELEMENT && r.getLocalName == skipped) skipping -= 1
        } else if (e == START_ELEMENT) start()
        else if (e == END_ELEMENT) end()
        else if (isText(e) && inT) put(r.getText)
        else if (isText(e) && inInstr) instr.append(r.getText)
      }
    } finally {
      while (paras.nonEmpty) {
        val t = paras.pop().text
        if (!t.isEmpty) { if (tables > 0) cell += t else out += Block.Paragraph(t) }
      }
      if (tables > 0) {
        if (cell.nonEmpty)
          row += Text.of(cell.toVector.flatMap(t => Span.plain(" ") +: t.spans).drop(1))
        endRow()
        endTable()
      }
      flushList()
      flushBoxes()
    }
  }
}
