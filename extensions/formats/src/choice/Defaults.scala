package grit.formats.choice

import grit.core.files.{Format, Formats, Kind}
import grit.formats.google.Slides
import grit.formats.ooxml.{Docx, Xlsx}
import grit.formats.pdf.Pdf

/** The formats grit ships, as a deployment reads files by default: a PDF by [[Pdf]], a Word
  * document by [[Docx]], an Excel workbook by [[Xlsx]], a Google Slides deck by [[Slides]],
  * under the default limits. Every other
  * kind is read by none, so it is [[grit.core.files.Unread.Unsupported]].
  */
object Defaults {

  val formats: Formats = Formats(kind => of(kind))

  private def of(kind: Kind): Vector[Format] = kind match {
    case Kind.Pdf => Vector(Pdf)
    case Kind.Word => Vector(Docx)
    case Kind.Excel => Vector(Xlsx)
    case Kind.GoogleSlides => Vector(Slides)
    case Kind.OldOffice | Kind.ExcelBinary | Kind.OtherZip | Kind.Text | Kind.Image |
        Kind.Unknown =>
      Vector.empty
  }
}
