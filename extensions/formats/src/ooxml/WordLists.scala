package grit.formats.ooxml

import java.util.Locale
import javax.xml.stream.XMLStreamConstants.{END_ELEMENT, START_ELEMENT}
import javax.xml.stream.XMLStreamReader

import scala.collection.mutable

import grit.formats.limits.Guard
import grit.formats.ooxml.Package.attr

/** Word's list numbers as Word shows them ("2.1.", "b)", "iv."), from `word/numbering.xml`:
  * counted per abstract list, so a list Word continues across its numbers is continued here,
  * a number's start override restarting its level the first time that number is used.
  */
private[ooxml] final class WordLists {

  /** One level: its start, format (`decimal`, `lowerLetter`, `bullet`, …) and text (`%1.%2.`). */
  private final case class Lvl(start: Option[Int], fmt: Option[String], text: Option[String]) {
    def over(o: Option[Lvl]): Lvl =
      o.fold(this)(o => Lvl(o.start.orElse(start), o.fmt.orElse(fmt), o.text.orElse(text)))
  }

  private final case class Num(abs: String, starts: Map[Int, Int], lvls: Map[Int, Lvl])

  private val abstracts: mutable.Map[String, Map[Int, Lvl]] = mutable.Map.empty
  private val nums: mutable.Map[String, Num] = mutable.Map.empty
  private val counters: mutable.Map[String, Vector[Int]] = mutable.Map.empty
  private val started: mutable.Set[String] = mutable.Set.empty

  /** Reads `word/numbering.xml`. */
  def read(r: XMLStreamReader, count: Guard.Count^): Unit = {
    var levels = Map.empty[Int, Lvl]
    var abs: Option[String] = None
    var num: Option[String] = None
    var absOfNum: Option[String] = None
    var starts = Map.empty[Int, Int]
    var lvls = Map.empty[Int, Lvl]
    var overLevel = 0
    var level = 0
    var lvl: Option[Lvl] = None
    def int(s: Option[String]): Option[Int] = s.flatMap(_.toIntOption)
    while (r.hasNext) {
      count.step()
      r.next() match {
        case START_ELEMENT =>
          r.getLocalName match {
            case "abstractNum" => abs = attr(r, "abstractNumId"); levels = Map.empty; num = None
            case "num" =>
              num = attr(r, "numId"); starts = Map.empty; lvls = Map.empty; absOfNum = None
            case "abstractNumId" if num.nonEmpty => absOfNum = attr(r, "val")
            case "lvlOverride" => overLevel = int(attr(r, "ilvl")).getOrElse(0)
            case "startOverride" => int(attr(r, "val")).foreach(v => starts += overLevel -> v)
            case "lvl" =>
              level = int(attr(r, "ilvl")).getOrElse(0); lvl = Some(Lvl(None, None, None))
            case "start" => lvl = lvl.map(_.copy(start = int(attr(r, "val"))))
            case "numFmt" => lvl = lvl.map(_.copy(fmt = attr(r, "val")))
            case "lvlText" => lvl = lvl.map(_.copy(text = attr(r, "val")))
            case _ => ()
          }
        case END_ELEMENT =>
          r.getLocalName match {
            case "lvl" =>
              lvl.foreach(l => if (num.nonEmpty) lvls += level -> l else levels += level -> l)
              lvl = None
            case "abstractNum" => abs.foreach(a => abstracts(a) = levels); abs = None
            case "num" =>
              for { n <- num; a <- absOfNum } nums(n) = Num(a, starts, lvls)
              num = None
            case _ => ()
          }
        case _ => ()
      }
    }
  }

  private def lvl(numId: String, ilvl: Int): Option[Lvl] =
    nums.get(numId).map { n =>
      val base = abstracts
        .getOrElse(n.abs, Map.empty)
        .getOrElse(ilvl, Lvl(Some(1), Some("decimal"), Some(s"%${ilvl + 1}.")))
      base.over(n.lvls.get(ilvl))
    }

  /** The label Word shows for the next paragraph of list `numId` at level `ilvl`: `None` for
    * a bullet, for number 0 and for a number not declared.
    */
  def next(numId: String, ilvl: Int): Option[String] =
    if (ilvl < 0 || ilvl > 8) None
    else
      for {
        n <- nums.get(numId)
        here <- lvl(numId, ilvl)
        if !here.fmt.contains("bullet") && !here.fmt.contains("none")
      } yield {
        // Level `l` set to `v`, the levels below it restarted.
        def set(c: Vector[Int], l: Int, v: Int): Vector[Int] =
          c.updated(l, v).zipWithIndex.map((x, i) => if (i > l) 0 else x)
        val before = counters.getOrElse(n.abs, Vector.fill(9)(0))
        val restarted =
          if (started.add(numId))
            n.starts.toVector.sorted.foldLeft(before) { case (c, (l, s)) =>
              if (l >= 0 && l < 9) set(c, l, s - 1) else c
            }
          else before
        val from = if (restarted(ilvl) == 0) here.start.fold(0)(_ - 1) else restarted(ilvl)
        val c = set(restarted, ilvl, from + 1)
        counters(n.abs) = c
        val text = here.text.getOrElse(s"%${ilvl + 1}.")
        """%([1-9])""".r.replaceAllIn(
          text,
          m => {
            val l = m.group(1).toInt - 1
            val at = lvl(numId, l)
            val v = if (l == ilvl) c(l) else c(l).max(at.flatMap(_.start).getOrElse(1))
            WordLists.format(v, at.flatMap(_.fmt))
          }
        )
      }
}

private[ooxml] object WordLists {

  /** `v` as a level of format `fmt` writes it. */
  def format(v: Int, fmt: Option[String]): String = fmt match {
    case Some("lowerLetter") => letters(v).toLowerCase(Locale.ROOT)
    case Some("upperLetter") => letters(v)
    case Some("lowerRoman") => roman(v).toLowerCase(Locale.ROOT)
    case Some("upperRoman") => roman(v)
    case Some("decimalZero") => (if (v < 10) "0" else "") + v
    case _ => v.toString
  }

  private def letters(v: Int): String =
    if (v <= 0) "0" else (('A' + (v - 1) % 26).toChar.toString) * ((v - 1) / 26 + 1)

  private def roman(v: Int): String =
    if (v <= 0 || v >= 4000) v.toString
    else
      Vector(
        1000 -> "M",
        900 -> "CM",
        500 -> "D",
        400 -> "CD",
        100 -> "C",
        90 -> "XC",
        50 -> "L",
        40 -> "XL",
        10 -> "X",
        9 -> "IX",
        5 -> "V",
        4 -> "IV",
        1 -> "I"
      )
        .foldLeft((v, "")) { case ((left, out), (n, s)) => (left % n, out + s * (left / n)) }
        ._2
}
