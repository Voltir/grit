package grit.core.files

import grit.core.document.Pointer
import grit.core.tool.ToolName
import grit.prose.form.{Block, Doc, Span, Text}

/** What a test's kept pieces are read again by and anchored at, when a test is about neither. */
object TestPieces {

  /** `drive_read` of the file `ref`. */
  def pointer(ref: String): Pointer =
    Pointer
      .of(ToolName("drive_read"), "file" -> ujson.Str(ref))
      .fold(why => throw new IllegalStateException(why), identity)

  /** The file `ref`, named `name`, at revision `r1`, a PDF read by `format` at version 1, read
    * again by [[pointer]] of `ref`, cut at [[Pieces.Target]].
    */
  def file(ref: String, name: String, format: FormatName): PiecedFile =
    PiecedFile(ref, name, "r1", Kind.Pdf, format, 1, pointer(ref), Pieces.Target)

  /** The anchor of all of `text`, as the whole of its file's text. */
  def anchor(text: String): Anchor = Anchor.of(text, 0, text.length)

  /** One piece under `heading` for each of `paragraphs`, in order, each a paragraph anchored at
    * its own text.
    */
  def paragraphs(heading: Vector[String], paragraphs: Vector[String]): Vector[Piece] =
    paragraphs.map(p =>
      Piece(
        heading,
        Doc(Vector(Block.Paragraph(Text(Vector(Span(p, Set.empty, Vector())))))),
        anchor(p)
      )
    )
}
