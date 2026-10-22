package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import grit.prose.form.{Block, Doc, Span, Text}

/** A format for tests above core: a file of `%PDF-` and then pages of text apart by form feeds,
  * each page one paragraph noted with its page, and nothing of any other bytes. A stand-in for a
  * real reader, which an edge's or a tool's module may not name; what a format must do is
  * core's ([[Format]]), and grit.formats' readers are held to it there.
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
  val formats: Formats =
    Formats(kind => if (kind == Kind.Pdf) Vector(PagedText) else Vector.empty)
}
