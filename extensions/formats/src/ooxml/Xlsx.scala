package grit.formats.ooxml

import java.time.{LocalDateTime, LocalTime}
import javax.xml.stream.XMLStreamConstants.{END_ELEMENT, START_ELEMENT}
import javax.xml.stream.XMLStreamReader

import scala.collection.mutable

import grit.core.files.{
  CoverUnit,
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
import grit.formats.tree.Blocks.table
import grit.prose.form.{Block, Cell, Doc, Level, Text}

/** Excel workbooks (`.xlsx`, `.xlsm`), transitional or strict, read over the JDK's zip and
  * StAX: each visible sheet, in the workbook's order, a heading of its name and a table as wide
  * as its widest row, its first row the columns' names. A cell keeps its value and the text it
  * shows: a number as stored, a date (in the workbook's 1900 or 1904 system) as an ISO date or
  * date and time, a percentage as one, a formula after its value, or alone as not calculated
  * when the workbook kept no value. Hidden sheets, cell comments, drawings and charts are
  * [[Missed]]. A stopped read covers the sheets it began ([[Covers.Part]]).
  */
object Xlsx extends Format {

  val name: FormatName = FormatName("xlsx")
  val version: Int = 1

  private val Main = "xl/workbook.xml"

  private final case class Sheet(name: String, part: String, hidden: Boolean)

  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] = {
    val blocks = mutable.ArrayBuffer.empty[Block[Locus]]
    val missed = mutable.ArrayBuffer.empty[Missed]
    var opened: Either[Unread, Unit] = Right(())
    var begun = 0
    var visible = 0
    val stopped = Guard.until(file, limits) { count =>
      opened = Package.open(file, limits, Kind.Excel, Main, count) { parts =>
        val (listed, d1904) = parts.xml(Main)(workbook(_, count)).getOrElse((Vector.empty, false))
        val targets = parts.xml("xl/_rels/workbook.xml.rels")(rels(_, count)).getOrElse(Map.empty)
        val strings = parts.xml("xl/sharedStrings.xml")(shared(_, count)).getOrElse(Vector.empty)
        val (xfs, codes) =
          parts.xml("xl/styles.xml")(styles(_, count)).getOrElse((Vector.empty, Map.empty))
        val sheets = listed.flatMap { (name, id, hidden) =>
          targets
            .get(id)
            .map(t => Sheet(name, if (t.startsWith("/")) t.drop(1) else s"xl/$t", hidden))
        }
        sheets.filter(_.hidden).foreach(s => missed += Missed(s"the hidden sheet '${s.name}'"))
        val names = parts.names
        if (names.exists(_.matches("xl/(comments\\d*\\.xml|threadedComments/.*)")))
          missed += Missed("cell comments")
        if (names.exists(_.matches("xl/drawings/drawing\\d*\\.xml")))
          missed += Missed("drawings and text boxes")
        if (names.exists(_.startsWith("xl/charts/"))) missed += Missed("charts")
        val shown = sheets.filterNot(_.hidden)
        visible = shown.size
        val cells = Cells(strings, xfs, codes, d1904)
        shown.foreach { s =>
          begun += 1
          blocks += Block.Heading(Level.of(2), Text.plain(s.name))
          val rows = mutable.ArrayBuffer.empty[Vector[Cell[Nothing]]]
          try parts.xml(s.part)(sheet(_, cells, count, rows))
          finally table(rows.toVector).foreach(blocks += _)
        }
      }
    }
    opened.map { _ =>
      Understood(
        Doc(blocks.toVector),
        stopped.fold(Covers.Whole)(Covers.Part(begun, visible, CoverUnit.Sheets, _)),
        missed.toVector,
        name,
        version
      )
    }
  }

  /** Each sheet's name, relationship id and whether it is hidden; and whether dates count from
    * 1904.
    */
  private def workbook(
      r: XMLStreamReader,
      count: Guard.Count^
  ): (Vector[(String, String, Boolean)], Boolean) = {
    val sheets = mutable.ArrayBuffer.empty[(String, String, Boolean)]
    var d1904 = false
    while (r.hasNext) {
      count.step()
      if (r.next() == START_ELEMENT) r.getLocalName match {
        case "sheet" =>
          for { n <- attr(r, "name"); id <- attr(r, "id") }
            sheets += ((n, id, attr(r, "state").exists(_ != "visible")))
        case "workbookPr" => d1904 = attr(r, "date1904").exists(v => v == "1" || v == "true")
        case _ => ()
      }
    }
    (sheets.toVector, d1904)
  }

  private def rels(r: XMLStreamReader, count: Guard.Count^): Map[String, String] = {
    val out = mutable.Map.empty[String, String]
    while (r.hasNext) {
      count.step()
      if (r.next() == START_ELEMENT && r.getLocalName == "Relationship")
        for { id <- attr(r, "Id"); to <- attr(r, "Target") } out(id) = to
    }
    out.toMap
  }

  /** The shared strings, phonetic runs left out, each counted against the characters read. */
  private def shared(r: XMLStreamReader, count: Guard.Count^): Vector[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    val cur = new StringBuilder
    var inT = false
    var phonetic = false
    while (r.hasNext) {
      count.step()
      val e = r.next()
      if (e == START_ELEMENT) r.getLocalName match {
        case "si" => cur.clear()
        case "t" => inT = true
        case "rPh" => phonetic = true
        case _ => ()
      }
      else if (e == END_ELEMENT) r.getLocalName match {
        case "si" => out += cur.toString
        case "t" => inT = false
        case "rPh" => phonetic = false
        case _ => ()
      }
      else if (inT && !phonetic && isText(e)) count.keep(r.getText, s => { cur.append(s); () })
    }
    out.toVector
  }

  /** Each cell style's number format id, and the workbook's own formats' codes by id. */
  private def styles(r: XMLStreamReader, count: Guard.Count^): (Vector[Int], Map[Int, String]) = {
    val xfs = mutable.ArrayBuffer.empty[Int]
    val codes = mutable.Map.empty[Int, String]
    var inXfs = false
    while (r.hasNext) {
      count.step()
      val e = r.next()
      if (e == START_ELEMENT) r.getLocalName match {
        case "numFmt" =>
          for { id <- attr(r, "numFmtId").flatMap(_.toIntOption); c <- attr(r, "formatCode") }
            codes(id) = c
        case "cellXfs" => inXfs = true
        case "xf" if inXfs => xfs += attr(r, "numFmtId").flatMap(_.toIntOption).getOrElse(0)
        case _ => ()
      }
      else if (e == END_ELEMENT && r.getLocalName == "cellXfs") inXfs = false
    }
    (xfs.toVector, codes.toMap)
  }

  /** A sheet's rows that hold anything, each cell at its column, added to `rows` as they end. */
  private def sheet(
      r: XMLStreamReader,
      cells: Cells,
      count: Guard.Count^,
      rows: mutable.ArrayBuffer[Vector[Cell[Nothing]]]
  ): Unit = {
    val row = mutable.ArrayBuffer.empty[Cell[Nothing]]
    val value = new StringBuilder
    val formula = new StringBuilder
    var inRow = false
    var kind: Option[String] = None
    var ref: Option[String] = None
    var style = 0
    var hasFormula = false
    var formulaKind: Option[String] = None
    var formulaRef: Option[String] = None
    var inV = false
    var inF = false
    var inline = false
    while (r.hasNext) {
      count.step()
      val e = r.next()
      if (e == START_ELEMENT) r.getLocalName match {
        case "row" => inRow = true; row.clear()
        case "c" =>
          kind = attr(r, "t"); ref = attr(r, "r")
          style = attr(r, "s").flatMap(_.toIntOption).getOrElse(0)
          value.clear(); formula.clear(); hasFormula = false; formulaKind = None; formulaRef = None
        case "v" => inV = true
        case "f" =>
          inF = true; hasFormula = true; formulaKind = attr(r, "t"); formulaRef = attr(r, "ref")
        case "is" => inline = true
        case "t" if inline => inV = true
        case _ => ()
      }
      else if (e == END_ELEMENT) r.getLocalName match {
        case "v" => inV = false
        case "f" => inF = false
        case "t" if inline => inV = false
        case "is" => inline = false
        case "c" if inRow =>
          val at = ref.map(Xlsx.column).getOrElse(row.size)
          if (at - row.size > 16384) throw new grit.formats.limits.Stopped(Limit.Damaged)
          while (row.size < at) row += Cell.Empty()
          val shown = cells.of(kind, value.toString, style)
          val sharedDependent = formulaKind.contains("shared") && formulaRef.isEmpty
          row += (
            if (!hasFormula) shown
            else if (sharedDependent)
              if (value.isEmpty) Cell.Words(Text.plain("(shared formula, not calculated)"))
              else shown
            else if (formula.isEmpty) shown
            else {
              val f = s"=$formula" + formulaRef
                .filter(_ => formulaKind.contains("shared"))
                .fold("")(r => s", filled to $r")
              Cells.withFormula(shown, f)
            }
          )
        case "row" =>
          inRow = false
          if (row.exists(!_.printed.isEmpty)) rows += row.toVector
        case _ => ()
      }
      else if (isText(e) && inV) count.keep(r.getText, s => { value.append(s); () })
      else if (isText(e) && inF) count.keep(r.getText, s => { formula.append(s); () })
    }
  }

  /** A column reference's index from 0: `A1` is 0, `AB7` is 27. */
  private def column(ref: String): Int =
    ref.takeWhile(_.isLetter).foldLeft(0)((c, ch) => c * 26 + (ch.toUpper - 'A' + 1)) - 1

  /** How a workbook's cells read: its shared strings, each style's number format, its own
    * formats' codes, and its date system.
    */
  private final case class Cells(
      strings: Vector[String],
      xfs: Vector[Int],
      codes: Map[Int, String],
      d1904: Boolean
  ) {

    /** The cell of type `kind` holding `raw` in style `style`. */
    def of(kind: Option[String], raw: String, style: Int): Cell[Nothing] = kind match {
      case Some("s") => raw.trim.toIntOption.flatMap(strings.lift).fold(Cell.Empty())(words)
      case Some("b") => Cell.Truth(raw.trim == "1")
      case Some("inlineStr" | "str" | "e") => words(raw)
      case _ if raw.isEmpty => Cell.Empty()
      case _ => number(raw.trim, xfs.lift(style).getOrElse(0))
    }

    private def words(s: String): Cell[Nothing] =
      if (s.isEmpty) Cell.Empty() else Cell.Words(Text.plain(s))

    private def number(raw: String, fmt: Int): Cell[Nothing] =
      Option.when(raw.matches("-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?"))(BigDecimal(raw)) match {
        case None => words(raw)
        case Some(v) if Cells.date(fmt, codes) => date(v, fmt)
        case Some(v) if fmt == 9 || fmt == 10 || codes.get(fmt).exists(_.endsWith("%")) =>
          Cell.Number(v, Text.plain((v * 100).bigDecimal.stripTrailingZeros.toPlainString + "%"))
        case Some(v) => Cell.Number(v, Text.plain(raw))
      }

    /** A date-formatted serial: a date, a date and time, a time of day for a format with no
      * date in it, or an elapsed time (`[h]:mm`), which is words.
      */
    private def date(serial: BigDecimal, fmt: Int): Cell[Nothing] = {
      val code = codes.getOrElse(fmt, Cells.BuiltinTimes.getOrElse(fmt, "yyyy-mm-dd"))
      val secs = (serial * 86400).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
      val plain = code.replaceAll("\"[^\"]*\"|\\[[^\\]]*\\]|\\\\.", "").toLowerCase
      val hasDate = plain.contains("y") || plain
        .contains("d") || (plain.contains("m") && !plain.contains("h") && !plain.contains("s"))
      if (code.toLowerCase.matches(".*\\[(h+|m+|s+)\\].*")) {
        val a = secs.abs
        words(f"${if (secs < 0) "-" else ""}${a / 3600}%d:${a / 60 % 60}%02d:${a % 60}%02d")
      } else if (!hasDate && serial >= 0 && serial < 1)
        words(LocalTime.ofSecondOfDay(secs % 86400).toString)
      else {
        val base =
          if (d1904) LocalDateTime.of(1904, 1, 1, 0, 0)
          // Excel's 1900 system counts a 29 February 1900 that never was.
          else if (serial < 61) LocalDateTime.of(1899, 12, 31, 0, 0)
          else LocalDateTime.of(1899, 12, 30, 0, 0)
        val at = base.plusSeconds(secs)
        if (secs % 86400 == 0) Cell.Date(at.toLocalDate, Text.plain(at.toLocalDate.toString))
        else Cell.Date(at, Text.plain(at.toString.replace('T', ' ')))
      }
    }
  }

  private object Cells {

    /** The built-in time formats (ECMA-376 18.8.30); the rest of 14 to 22 and 45 to 47 are
      * dates.
      */
    val BuiltinTimes: Map[Int, String] = Map(
      18 -> "h:mm AM/PM",
      19 -> "h:mm:ss AM/PM",
      20 -> "h:mm",
      21 -> "h:mm:ss",
      22 -> "m/d/yy h:mm",
      45 -> "mm:ss",
      46 -> "[h]:mm:ss",
      47 -> "mmss.0"
    )

    /** Whether format `id` shows a date or a time. */
    def date(id: Int, codes: Map[Int, String]): Boolean =
      (id >= 14 && id <= 22) || (id >= 45 && id <= 47) || codes.get(id).exists { code =>
        val c = code.replaceAll("\"[^\"]*\"|\\[[^\\]]*\\]|\\\\.", "").toLowerCase
        c.matches(".*[ymdhs].*") && !c.contains("#") && !c.contains("0.0")
      }

    /** `cell` computed by formula `f`; one with no value, as not calculated. */
    def withFormula(cell: Cell[Nothing], f: String): Cell[Nothing] = cell match {
      case c: Cell.Words[Nothing] => c.copy(formula = Some(f))
      case c: Cell.Number => c.copy(formula = Some(f))
      case c: Cell.Date => c.copy(formula = Some(f))
      case c: Cell.Truth => c.copy(formula = Some(f))
      case _: Cell.Empty => Cell.Empty(Some(f))
    }
  }
}
