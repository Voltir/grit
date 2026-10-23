package grit.core.files

import grit.core.document.Pointer
import grit.core.tool.ToolName

/** What a test's kept pieces are read again by and anchored at, when a test is about neither. */
object TestPieces {

  /** `drive_read` of the file `ref`. */
  def pointer(ref: String): Pointer =
    Pointer
      .of(ToolName("drive_read"), "file" -> ujson.Str(ref))
      .fold(why => throw new IllegalStateException(why), identity)

  /** The anchor of all of `text`, as the whole of its file's text. */
  def anchor(text: String): Anchor = Anchor.of(text, 0, text.length)
}
