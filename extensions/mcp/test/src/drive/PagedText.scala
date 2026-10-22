package grit.mcp.drive

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import grit.core.files.{
  Covers,
  Format,
  FormatName,
  Kind,
  Locus,
  OpenFile,
  ReadLimits,
  Understood,
  Unread
}
import grit.prose.form.{Block, Doc, Span, Text}

/** A format for the edge's tests: a file of `%PDF-` and then pages of text apart by form feeds,
  * each page one paragraph noted with its page, and nothing of any other bytes. A stand-in for a
  * real reader, which grit.mcp may not name; what a format must do is core's
  * ([[grit.core.files.Format]]), and grit.formats' readers are held to it there.
  */
object PagedText extends Format {
  val name: FormatName = FormatName("paged-text")
  val version: Int = 1

  /** `pages` as such a file's bytes. */
  def bytes(pages: String*): Vector[Byte] =
    ("%PDF-" + pages.mkString("\f")).getBytes(UTF_8).toVector

  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] = {
    val text = new String(Files.readAllBytes(file.path), UTF_8)
    if (!text.startsWith("%PDF-")) Left(Unread.NotMine)
    else
      Right(
        Understood(
          Doc(
            text.drop(5).split('\f').toVector.zipWithIndex.map { (page, i) =>
              Block.Paragraph(
                Text(Vector(Span(page, Set.empty, Vector(Locus.Page(i + 1)))))
              ): Block[Locus]
            }
          ),
          Covers.Whole,
          Vector.empty,
          name,
          version
        )
      )
  }

  /** PDFs read by this format alone. */
  val formats: grit.core.files.Formats =
    grit.core.files.Formats(kind => if (kind == Kind.Pdf) Vector(PagedText) else Vector.empty)
}
