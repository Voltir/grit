package grit.formats.choice

import grit.core.files.{Format, Formats, Kind}
import grit.formats.ooxml.{Docx, Xlsx}

/** The formats grit ships, as a deployment reads files by default: a Word document by [[Docx]],
  * an Excel workbook by [[Xlsx]], under the default limits. Every other kind is read by none,
  * so it is [[grit.core.files.Unread.Unsupported]].
  */
object Defaults {

  val formats: Formats = Formats(kind => of(kind))

  private def of(kind: Kind): Vector[Format] = kind match {
    case Kind.Word => Vector(Docx)
    case Kind.Excel => Vector(Xlsx)
    case Kind.Pdf | Kind.OldOffice | Kind.ExcelBinary | Kind.OtherZip | Kind.Text | Kind.Image |
        Kind.Unknown =>
      Vector.empty
  }
}
