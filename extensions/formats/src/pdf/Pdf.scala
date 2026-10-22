package grit.formats.pdf

import java.io.{IOException, Writer}
import java.nio.file.Files
import java.text.Normalizer
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import grit.core.files.{
  CoverUnit,
  Covers,
  Format,
  FormatName,
  Kind,
  Limit,
  Locus,
  OpenFile,
  ReadLimits,
  Understood,
  Unread
}
import grit.formats.limits.{Guard, Stopped}
import grit.prose.form.{Block, Doc, Level, Span, Text}

import org.apache.pdfbox.Loader
import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.encryption.InvalidPasswordException
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineNode
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}

/** PDFs, read with PDFBox in the order their pages draw text, never re-sorted by position (which
  * scrambles columns): each line's spans noted with its page, a paragraph running onto the next
  * page keeping both. A bookmark is a heading where its title is printed on its page (its
  * printed text kept, a section number included), or at its page's top when it is not found.
  * A line printed on a quarter of the pages or more, at least three, digits aside, is a running
  * header or footer and dropped. A run of at least three short lines (each at most
  * [[Pdf.ShortLine]] characters), as a table's rows are, keeps its line breaks. An owner's
  * restrictions on copying do not stop a read. Stopped by its glyphs ([[ReadLimits.glyphs]],
  * a page drawing its text many times over), its characters or its time, it covers the pages it
  * finished ([[Covers.Part]]).
  */
object Pdf extends Format {

  val name: FormatName = FormatName("pdf")
  val version: Int = 1

  /** The longest line a run that keeps its line breaks may hold: 40 characters. */
  val ShortLine: Int = 40

  private final case class Line(page: Int, para: Int, text: String)
  private final case class Mark(level: Int, page: Int, title: String)

  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] =
    if (!pdf(file)) Left(Unread.NotMine)
    else
      load(file).flatMap { doc =>
        try extract(file, limits, doc)
        finally doc.close()
      }

  /** Whether `%PDF-` is among its first bytes, where a PDF reader looks for it. */
  private def pdf(file: OpenFile^): Boolean =
    try {
      val in = Files.newInputStream(file.path)
      try new String(in.readNBytes(1024), "ISO-8859-1").contains("%PDF-")
      finally in.close()
    } catch { case _: IOException => false }

  private def load(file: OpenFile^): Either[Unread, PDDocument] =
    try Right(Loader.loadPDF(file.path.toFile))
    catch {
      case _: InvalidPasswordException => Left(Unread.Locked)
      case _: IOException | _: RuntimeException => Left(Unread.Damaged(Kind.Pdf))
    }

  private def extract(
      file: OpenFile^,
      limits: ReadLimits,
      doc: PDDocument
  ): Either[Unread, Understood] = {
    val pages = doc.getNumberOfPages
    val lines = mutable.ArrayBuffer.empty[Line]
    val at = new AtomicInteger(0)
    val stopped = Guard.until(file, limits) { count =>
      val glyphs = new AtomicInteger(0)
      val para = new AtomicInteger(0)
      val line = new StringBuilder
      val stripper = new PDFTextStripper {
        private def flush(): Unit = {
          val s = line.toString.strip
          line.clear()
          if (s.nonEmpty)
            count.keep(s, kept => { lines += Line(getCurrentPageNo, para.get, kept); () })
        }
        override protected def processTextPosition(text: TextPosition): Unit = {
          if (glyphs.incrementAndGet() > limits.glyphs) {
            at.set(getCurrentPageNo)
            throw new Stopped(Limit.Glyphs)
          }
          super.processTextPosition(text)
        }
        override protected def processOperator(
            operator: Operator,
            operands: java.util.List[COSBase]
        ): Unit = {
          at.set(getCurrentPageNo)
          count.step()
          super.processOperator(operator, operands)
        }
        override protected def writeString(
            text: String,
            positions: java.util.List[TextPosition]
        ): Unit = {
          line.append(text); ()
        }
        override protected def writeWordSeparator(): Unit = { line.append(' '); () }
        override protected def writeLineSeparator(): Unit = flush()
        override protected def writeParagraphStart(): Unit = { flush(); para.incrementAndGet(); () }
        override protected def writeParagraphEnd(): Unit = flush()
        override protected def writePageEnd(): Unit = { flush(); para.incrementAndGet(); () }
      }
      stripper.setSortByPosition(false)
      stripper.writeText(doc, Writer.nullWriter())
    }
    val kept = runningDropped(lines.toVector, pages)
    val marks = bookmarks(doc)
    val blocks = laid(kept, marks)
    val finished = (at.get - 1).max(0)
    stopped match {
      case Some(Limit.Time) if blocks.isEmpty => Left(Unread.TooSlow)
      case Some(Limit.Damaged) if blocks.isEmpty => Left(Unread.Damaged(Kind.Pdf))
      case None if kept.isEmpty && lines.isEmpty && pages > 0 => Left(Unread.Scanned(pages))
      case _ =>
        Right(
          Understood(
            Doc(blocks),
            stopped.fold(Covers.Whole)(Covers.Part(finished, pages, CoverUnit.Pages, _)),
            Vector.empty,
            name,
            version
          )
        )
    }
  }

  /** `s` as titles and lines are compared: NFKC, lower case, each run of anything but letters
    * and digits one space.
    */
  private def norm(s: String): String =
    Normalizer
      .normalize(s, Normalizer.Form.NFKC)
      .toLowerCase
      .replaceAll("[^\\p{L}\\p{N}]+", " ")
      .strip

  /** `lines` less those printed, digits aside, on a quarter of `pages` or more, at least three. */
  private def runningDropped(lines: Vector[Line], pages: Int): Vector[Line] = {
    def key(l: Line) = norm(l.text).replaceAll("\\d+", "#")
    val seen = lines.groupMapReduce(key)(l => Set(l.page))(_ ++ _)
    val most = (pages / 4).max(3)
    lines.filter(l => seen.getOrElse(key(l), Set.empty).size < most)
  }

  /** The document's bookmarks in order, each with its level and the page it goes to; the
    * first 10,000, so a looping outline ends.
    */
  private def bookmarks(doc: PDDocument): Vector[Mark] = {
    val out = mutable.ArrayBuffer.empty[Mark]
    def walk(node: PDOutlineNode, level: Int): Unit =
      node.children().asScala.foreach { item =>
        if (out.size < 10000) {
          val page =
            try
              Option(item.findDestinationPage(doc))
                .map(p => doc.getPages.indexOf(p) + 1)
                .filter(_ > 0)
            catch { case _: IOException => None }
          for { p <- page; t <- Option(item.getTitle) if t.strip.nonEmpty }
            out += Mark(level, p, t.strip)
          if (level < 32) walk(item, level + 1)
        }
      }
    try Option(doc.getDocumentCatalog.getDocumentOutline).foreach(walk(_, 1))
    catch { case _: RuntimeException => () }
    out.toVector
  }

  /** `lines` as blocks: each mark a heading where its title is printed on its page (over up to
    * three lines), else at its page's top; paragraphs between.
    */
  private def laid(lines: Vector[Line], marks: Vector[Mark]): Vector[Block[Locus]] = {
    val byPage = lines.indices.groupBy(i => lines(i).page)
    val used = mutable.Set.empty[Int]
    val placed = mutable.Map.empty[Int, (Int, Int)] // first line -> (level, last line)
    val lost = mutable.ArrayBuffer.empty[Mark]
    def stripped(s: String) = s.replaceFirst("^(\\d+ )+", "")
    marks.foreach { m =>
      val t = norm(m.title)
      val found = byPage.getOrElse(m.page, Vector.empty).sorted.find { i =>
        !used(i) && {
          val run =
            (i until (i + 3).min(lines.size)).takeWhile(j => lines(j).page == m.page && !used(j))
          val tt = stripped(t)
          run.indices.exists { k =>
            val a = stripped(norm(run.take(k + 1).map(j => lines(j).text).mkString(" ")))
            val hit =
              tt.nonEmpty && (a == tt || (a.startsWith(tt) && a.length <= tt.length * 1.3 + 6))
            if (hit) { placed(i) = (m.level, run(k)); (i to run(k)).foreach(used += _) }
            hit
          }
        }
      }
      if (t.nonEmpty && found.isEmpty) lost += m
    }
    val out = mutable.ArrayBuffer.empty[Block[Locus]]
    val para = mutable.ArrayBuffer.empty[Line]
    def flush(): Unit = if (para.nonEmpty) { out += paragraph(para.toVector); para.clear() }
    var page = 0
    var i = 0
    while (i < lines.size) {
      val l = lines(i)
      if (l.page != page) {
        lost.filter(m => m.page > page && m.page <= l.page).foreach { m =>
          flush()
          out += heading(m.level, m.page, m.title)
        }
        page = l.page
      }
      placed.get(i) match {
        case Some((level, last)) =>
          flush()
          out += heading(level, l.page, (i to last).map(lines(_).text).mkString(" "))
          i = last + 1
        case None =>
          val continues = para.lastOption.exists { p =>
            p.para == l.para || (p.page != l.page && !p.text.matches(".*[.:;!?)\"”]$") &&
              l.text.headOption.exists(_.isLower))
          }
          if (!continues) flush()
          para += l
          i += 1
      }
    }
    flush()
    out.toVector
  }

  private def heading(level: Int, page: Int, text: String): Block[Locus] =
    Block.Heading(Level.of(level), Text(Vector(Span(text, Set.empty, Vector(Locus.Page(page))))))

  /** A paragraph's lines, noted by page: a run of three or more short lines keeps its line
    * breaks; other lines are joined by a space, or by nothing after a hyphen.
    */
  private def paragraph(lines: Vector[Line]): Block[Locus] = {
    val short = lines.size >= 3 && lines.forall(_.text.length <= ShortLine)
    val spans = lines.indices.map { k =>
      val l = lines(k)
      val last = k == lines.size - 1
      val text =
        if (last) l.text
        else if (short) l.text + "\n"
        else if (l.text.endsWith("-")) l.text.dropRight(1)
        else l.text + " "
      Span(text, Set.empty, Vector(Locus.Page(l.page)))
    }
    Block.Paragraph(Text.of(spans.toVector))
  }
}
