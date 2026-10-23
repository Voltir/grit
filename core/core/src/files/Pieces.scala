package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8

import scala.util.matching.Regex

import grit.core.id.Sha256
import grit.core.message.Tokens
import grit.core.provider.TokenEstimator
import grit.prose.form.{Block, Cell, Column, Doc, Item, Span, Text}
import grit.prose.index.IndexText
import grit.prose.markdown.Markdown

/** A piece of a file: its tree under its heading path, the headings above it outermost first. */
final case class Piece(heading: Vector[String], doc: Doc[Locus]) {

  /** The first and last page its text lies on, when its file is counted in pages. */
  val pages: Option[(Int, Int)] = {
    val ps = doc.notes.collect { case Locus.Page(n) => n }
    Option.when(ps.nonEmpty)((ps.min, ps.max))
  }

  /** The hash of its heading path and words, case and spacing aside: equal for the same piece
    * in another revision of its file, wherever it lies there.
    */
  val fingerprint: Sha256 = {
    val words = (heading.mkString(" > ") + "\n" + IndexText.render(Piece.plain(doc)))
      .replaceAll("\\s+", " ")
      .trim
      .toLowerCase
    Sha256.digest(words.getBytes(UTF_8).toVector)
  }
}

object Piece {

  /** `doc` with its notes dropped. */
  private[files] def plain(doc: Doc[Locus]): Doc[Nothing] = doc.mapSpans(s => Span(s.text, s.marks))
}

object Pieces {

  /** About how many tokens a piece holds: what a window's judge reads of each candidate. */
  val Target: Tokens = Tokens(200)

  /** The most pieces one file is cut into; the rest are dropped, and counted. */
  val PerFile: Int = 2000

  /** `tree` cut into pieces: whole blocks packed greedily to at most [[Target]] tokens by
    * `estimate` under one heading path, a block over it cut (a paragraph at sentences, then
    * words; a list at its items; a code listing at lines); a table's rows packed with its
    * columns, a row over it cut at its columns; at most [[PerFile]], and how many were
    * dropped. A heading is never in a piece's doc but in its path; a rule is dropped.
    */
  def of(tree: Understood, estimate: TokenEstimator): (Vector[Piece], Int) = {
    val cutter = Cutter(estimate)
    // The heading path, each heading with its level; the pieces done; the blocks of the one open.
    var path = Vector.empty[(Int, String)]
    var done = Vector.empty[Piece]
    var open = Vector.empty[Block[Locus]]
    def close(): Unit = {
      if (open.nonEmpty) done :+= Piece(path.map(_._2), Doc(open))
      open = Vector.empty
    }
    tree.doc.blocks.foreach {
      case Block.Heading(level, text) =>
        close()
        path = path.takeWhile(_._1 < level.value) :+ (level.value -> oneLine(text.plain))
      case block =>
        cutter.cut(block).foreach { part =>
          val joined = cutter.join(open, part)
          if (open.isEmpty || cutter.fits(joined)) open = joined
          else { close(); open = Vector(part) }
        }
    }
    close()
    (done.take(PerFile), (done.size - PerFile).max(0))
  }

  private def oneLine(s: String): String = s.replace('\n', ' ').trim

  /** Cuts blocks and packs them to at most [[Target]] tokens by `estimate`. */
  private final case class Cutter(estimate: TokenEstimator) {

    def cost(blocks: Vector[Block[Locus]]): Long =
      Tokens.value(estimate.system(Markdown.write(Piece.plain(Doc(blocks)))))

    def fits(blocks: Vector[Block[Locus]]): Boolean = cost(blocks) <= Tokens.value(Target)

    def fits(block: Block[Locus]): Boolean = fits(Vector(block))

    /** `open` with `part` after it: a table's rows joined to the table before it of the same
      * columns.
      */
    def join(open: Vector[Block[Locus]], part: Block[Locus]): Vector[Block[Locus]] =
      (open.lastOption, part) match {
        case (Some(Block.Table(cs, rows)), Block.Table(cs2, more)) if cs == cs2 =>
          open.dropRight(1) :+ Block.table(cs, rows ++ more)
        case _ => open :+ part
      }

    /** `block` as blocks each within [[Target]], in order, nothing of its text lost; a rule and
      * an empty paragraph as none.
      */
    def cut(block: Block[Locus]): Vector[Block[Locus]] = block match {
      case Block.Rule => Vector.empty
      case Block.Paragraph(text) if text.plain.isBlank => Vector.empty
      case b if fits(b) => Vector(b)
      case Block.Paragraph(text) => texts(text, t => fits(Block.Paragraph(t))).map(Block.Paragraph(_))
      case Block.Heading(level, text) =>
        texts(text, t => fits(Block.Heading(level, t))).map(Block.Heading(level, _))
      case Block.Bullets(items) =>
        items.flatMap { item =>
          val one: Block[Locus] = Block.Bullets(Vector(item))
          if (fits(one)) Vector(one)
          else item.blocks.flatMap(cut).map(b => Block.Bullets(Vector(Item(Vector(b)))))
        }
      case Block.Numbered(start, items) =>
        items.zipWithIndex.flatMap { (item, i) =>
          val one: Block[Locus] = Block.Numbered(start + i, Vector(item))
          if (fits(one)) Vector(one)
          else item.blocks.flatMap(cut).map(b => Block.Numbered(start + i, Vector(Item(Vector(b)))))
        }
      case Block.Quote(blocks) => blocks.flatMap(cut).map(b => Block.Quote(Vector(b)))
      case Block.Code(language, lines) =>
        val parts = lines.flatMap { line =>
          if (fits(Block.Code(language, Vector(line)))) Vector(line)
          else texts(Text.plain(line), t => fits(Block.Code(language, Vector(t.plain)))).map(_.plain)
        }
        packed(parts)(ls => fits(Block.Code(language, ls))).map(Block.Code(language, _))
      case Block.Table(columns, rows) =>
        rows.flatMap { row =>
          val one = Block.table(columns, Vector(row))
          if (fits(one)) Vector(one) else cells(columns, row)
        }
    }

    /** A row too long for one piece, cut at its columns: as many columns a table as fit, a
      * cell's words too long alone cut as a paragraph is.
      */
    private def cells(columns: Vector[Column[Locus]], row: Vector[Cell[Locus]]): Vector[Block[Locus]] = {
      val single = columns.zip(row).flatMap { (c, cell) =>
        val one = Block.table(Vector(c), Vector(Vector(cell)))
        cell match {
          case _ if fits(one) => Vector((c, cell))
          case Cell.Words(text, formula) =>
            texts(text, t => fits(Block.table(Vector(c), Vector(Vector(Cell.Words(t, formula))))))
              .map(t => (c, Cell.Words(t, formula)))
          case other => Vector((c, other))
        }
      }
      packed(single)(cs => fits(Block.table(cs.map(_._1), Vector(cs.map(_._2)))))
        .map(cs => Block.table(cs.map(_._1), Vector(cs.map(_._2))))
    }

    /** `xs` in runs, each as long as `ok` holds, at least one. */
    private def packed[X](xs: Vector[X])(ok: Vector[X] => Boolean): Vector[Vector[X]] =
      xs.foldLeft(Vector.empty[Vector[X]]) { (runs: Vector[Vector[X]], x: X) =>
        runs.lastOption match {
          case Some(run) if ok(run :+ x) => runs.dropRight(1) :+ (run :+ x)
          case _ => runs :+ Vector(x)
        }
      }

    /** `text` cut where `ok` holds of each part: at sentences, a sentence too long at words, a
      * word too long at characters; each part trimmed of the spaces it was cut at.
      */
    private def texts(text: Text[Locus], ok: Text[Locus] => Boolean): Vector[Text[Locus]] = {
      val plain = text.plain
      def ends(level: Int, from: Int, to: Int): Vector[Int] = {
        val inside = level match {
          case 0 => Sentence.findAllMatchIn(plain.substring(from, to)).map(from + _.end).toVector
          case 1 => Space.findAllMatchIn(plain.substring(from, to)).map(from + _.end).toVector
          case _ => (from + 1 until to).toVector
        }
        inside.filter(e => e > from && e < to) :+ to
      }
      def part(from: Int, to: Int): Text[Locus] = trimmed(slice(text, from, to))
      def go(level: Int, from: Int, to: Int): Vector[(Int, Int)] = {
        var at = from
        var out = Vector.empty[(Int, Int)]
        while (at < to) {
          val next = ends(level, at, to)
          next.takeWhile(e => ok(part(at, e))).lastOption match {
            case Some(e) =>
              out :+= (at, e); at = e
            case None =>
              val e = next.headOption.getOrElse(to)
              out ++= (if (level >= 2) Vector((at, e)) else go(level + 1, at, e)); at = e
          }
        }
        out
      }
      go(0, 0, plain.length).map(part(_, _)).filterNot(_.isEmpty)
    }
  }

  /** The end of a sentence: its stop and the spaces after it. */
  private val Sentence = """[.!?]["')\]]*\s+""".r

  private val Space = """\s+""".r

  /** The characters `from` until `to` of `text`, each span's marks and notes kept. */
  private def slice(text: Text[Locus], from: Int, to: Int): Text[Locus] = {
    val starts = text.spans.scanLeft(0)(_ + _.text.length)
    Text.of(text.spans.zip(starts).flatMap { (s, at) =>
      val (a, b) = ((from - at).max(0), (to - at).min(s.text.length))
      Option.when(a < b)(s.copy(text = s.text.substring(a, b)))
    })
  }

  /** `text` without the spaces it starts or ends with. */
  private def trimmed(text: Text[Locus]): Text[Locus] = {
    val head = text.spans.headOption.map(s => s.copy(text = s.text.stripLeading))
    val spans = head.fold(text.spans)(text.spans.updated(0, _))
    val last = spans.lastOption.map(s => s.copy(text = s.text.stripTrailing))
    Text.of(last.fold(spans)(spans.updated(spans.size - 1, _)))
  }
}

/** The text a piece is searched by. */
object PieceIndex {

  /** The most characters of [[of]]'s text; past it, cut. */
  val Most: Int = 4000

  /** `file`'s name in words (its extension dropped), its heading path, then [[IndexText]] of its
    * doc, a line each, each identifier also as its alias ([[query]]); at most [[Most]]
    * characters.
    */
  def of(file: String, piece: Piece): String = {
    val name = file
      .replaceFirst("""\.[A-Za-z0-9]{1,5}$""", "")
      .replaceAll("""(?<!\d)-(?!\d)|_""", " ")
      .replaceAll("""\s+""", " ")
      .trim
    val path = piece.heading.mkString(" > ")
    val text = Vector(name, path, IndexText.render(Piece.plain(piece.doc))).filter(_.nonEmpty)
    query(text.mkString("\n")).take(Most)
  }

  /** `text` with each identifier followed by its alias: its letters and digits, an opening
    * bracket as `x` (`AC-2(3)` also `AC2x3`, `800-53` also `80053`). An identifier is letters
    * and digits joined by `-` or `.`, then any bracketed letters and digits, holding a digit and
    * a joint.
    */
  def query(text: String): String =
    Identifier.replaceAllIn(
      text,
      m => {
        val id = m.matched
        val alias = id.replace("(", "x").filter(_.isLetterOrDigit)
        val joined = id.exists(c => !c.isLetterOrDigit)
        Regex.quoteReplacement(if (joined && id.exists(_.isDigit)) s"$id $alias" else id)
      }
    )

  private val Identifier: Regex = """\b[A-Za-z0-9]+(?:[-.][A-Za-z0-9]+)*(?:\([A-Za-z0-9]+\))*""".r
}
