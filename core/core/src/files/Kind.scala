package grit.core.files

/** What a file's bytes are, as its first bytes show (and, for a zip, its entries' names); the
  * claimed type only breaks a tie, never overrides the bytes.
  */
enum Kind derives CanEqual {

  /** A PDF. */
  case Pdf

  /** A Word document: a `.docx` or `.docm`, transitional or strict. */
  case Word

  /** An Excel workbook: an `.xlsx` or `.xlsm`, transitional or strict. */
  case Excel

  /** An OLE2 container: a `.doc`, `.xls` or `.ppt`, or an Office file locked with a password. */
  case OldOffice

  /** An Excel binary workbook, `.xlsb`. */
  case ExcelBinary

  /** A zip holding neither a Word document nor an Excel workbook. */
  case OtherZip

  /** A Google Slides deck as the Slides API answers it: JSON, told from other text only by its
    * sender's claim (`application/vnd.google-apps.presentation`).
    */
  case GoogleSlides

  /** Text in UTF-8, with no NUL among its first bytes. */
  case Text

  /** A PNG, JPEG, GIF, WebP or TIFF image. */
  case Image

  /** None of these. */
  case Unknown

  /** What a refusal calls it: "a PDF", "a Word file", …. */
  def words: String = this match {
    case Pdf => "a PDF"
    case Word => "a Word file"
    case Excel => "an Excel file"
    case OldOffice => "an older Office file (.doc, .xls or .ppt) or a locked one"
    case ExcelBinary => "an Excel binary workbook (.xlsb)"
    case OtherZip => "a zip file"
    case GoogleSlides => "a Google Slides deck"
    case Text => "a text file"
    case Image => "an image"
    case Unknown => "a file of a type not known here"
  }
}

object Kind {

  /** How many of a file's first bytes [[sniff]] is handed: 1024. */
  val Head: Int = 1024

  /** The kind `head` (a file's first [[Head]] bytes, or all of a shorter one) shows; for a zip,
    * by `entries`, its entries' names, asked only of a zip. `claimed`, a media type its sender
    * gave, decides only a zip holding both a Word document and a workbook; an empty file (text
    * when it claims `text/`, else [[Unknown]]); and text that is a JSON object ([[GoogleSlides]]
    * when it claims `application/vnd.google-apps.presentation`, else [[Text]]).
    */
  def sniff(head: Vector[Byte], entries: => Vector[String], claimed: Option[String]): Kind = {
    def starts(magic: Int*): Boolean =
      head.length >= magic.length && magic.indices.forall(i => (head(i) & 0xff) == magic(i))
    val claimedText = claimed.exists(_.toLowerCase.startsWith("text/"))
    if (head.isEmpty) { if (claimedText) Text else Unknown }
    else if (pdf(head)) Pdf
    else if (starts(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1)) OldOffice
    else if (starts(0x50, 0x4b, 0x03, 0x04)) zip(entries, claimed)
    else if (
      starts(0x89, 0x50, 0x4e, 0x47) || starts(0xff, 0xd8, 0xff) || starts(
        0x47,
        0x49,
        0x46,
        0x38
      ) ||
      starts(0x49, 0x49, 0x2a, 0x00) || starts(0x4d, 0x4d, 0x00, 0x2a) ||
      (starts(0x52, 0x49, 0x46, 0x46) && head.length >= 12 &&
        head.slice(8, 12).map(_.toChar).mkString == "WEBP")
    ) Image
    else if (text(head))
      if (
        claimed.contains(SlidesType) && head
          .find(b => !Character.isWhitespace(b.toInt))
          .contains('{'.toByte)
      )
        GoogleSlides
      else Text
    else Unknown
  }

  /** The media type Drive gives a Google Slides deck. */
  val SlidesType: String = "application/vnd.google-apps.presentation"

  /** Whether `%PDF-` lies within `head` (a file's first [[Head]] bytes), where a PDF reader looks
    * for it.
    */
  def pdf(head: Vector[Byte]): Boolean =
    head.map(b => (b & 0xff).toChar).mkString.contains("%PDF-")

  private def zip(entries: Vector[String], claimed: Option[String]): Kind = {
    val word = entries.contains("word/document.xml")
    val excel = entries.contains("xl/workbook.xml")
    if (word && excel)
      if (claimed.exists(_.contains("spreadsheet"))) Excel else Word
    else if (word) Word
    else if (excel) Excel
    else if (entries.contains("xl/workbook.bin")) ExcelBinary
    else OtherZip
  }

  /** UTF-8 with no NUL; a character cut at the head's end is allowed. */
  private def text(head: Vector[Byte]): Boolean = {
    // Each character's lead byte says how many continuation bytes (10xxxxxx) follow it.
    def follows(lead: Int): Int =
      if (lead < 0x80) 0
      else if (lead >= 0xc2 && lead < 0xe0) 1
      else if (lead >= 0xe0 && lead < 0xf0) 2
      else if (lead >= 0xf0 && lead < 0xf5) 3
      else -1
    @annotation.tailrec
    def from(i: Int): Boolean =
      if (i >= head.length) true
      else {
        val b = head(i) & 0xff
        val n = follows(b)
        if (b == 0 || n < 0) false
        else {
          val more = (i + 1 to i + n).takeWhile(_ < head.length)
          more.forall(j => (head(j) & 0xc0) == 0x80) && from(i + 1 + n)
        }
      }
    from(0)
  }
}
