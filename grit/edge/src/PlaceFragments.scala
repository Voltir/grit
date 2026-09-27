package grit.edge

import grit.core.host.InstructionFile
import grit.core.prompt.{Fragment, Layer}
import grit.core.provider.TokenEstimator

/** A place's instruction files as the Place layer of a system prompt: data from the place,
  * never grit's words, bounded so a deep tree cannot crowd out the rest.
  */
object PlaceFragments {

  /** The most of an instruction file other than the nearest a prompt holds: 8 KiB. */
  val AncestorBytes: Int = 8 * 1024

  /** The most the whole layer holds, as `estimator` counts its fragments: 12,000 tokens. */
  val LayerTokens: Long = 12000

  /** `files`, the farthest first as [[grit.core.host.Instructions.around]] gives them, as
    * Place fragments in that order: the nearest file as read, each other cut at
    * [[AncestorBytes]] on a character boundary; a file cut says so on its last line; a file
    * whose text an earlier one already holds is left out; and while the layer is over
    * [[LayerTokens]], the farthest left is left out, but never the nearest.
    */
  def of(files: Vector[InstructionFile], estimator: TokenEstimator): Vector[Fragment] = {
    val nearest = files.size - 1
    val bounded = files.zipWithIndex.map { (f, i) =>
      if (i == nearest) f
      else {
        val kept = within(f.text, AncestorBytes)
        f.copy(text = kept, cut = f.cut || kept.length < f.text.length)
      }
    }
    // Equal texts once, the farthest kept: compared as read, before any cut.
    val distinct = files
      .zip(bounded)
      .foldLeft(Vector.empty[(String, InstructionFile)]) { case (kept, (read, f)) =>
        if (kept.exists(_._1 == read.text)) kept else kept :+ (read.text, f)
      }
      .map(_._2)
      .map(fragment)
    def tokens(fs: Vector[Fragment]): Long =
      fs.map(f => grit.core.message.Tokens.value(estimator.system(f.text))).sum
    // The nearest stays whatever its size: it is the one that speaks for this directory.
    Iterator
      .iterate(distinct)(_.drop(1))
      .find(fs => fs.size <= 1 || tokens(fs) <= LayerTokens)
      .getOrElse(Vector.empty)
  }

  /** The longest prefix of `text` whose UTF-8 is at most `max` bytes, never splitting a
    * character.
    */
  private def within(text: String, max: Int): String = {
    var bytes = 0
    var end = 0
    var over = false
    while (end < text.length && !over) {
      val cp = text.codePointAt(end)
      val size = if (cp < 0x80) 1 else if (cp < 0x800) 2 else if (cp < 0x10000) 3 else 4
      if (bytes + size > max) over = true
      else {
        bytes += size
        end += Character.charCount(cp)
      }
    }
    text.take(end)
  }

  /** `file` as its fragment: its text under a heading naming it. */
  private def fragment(file: InstructionFile): Fragment = {
    val note = if (file.cut) "\n\n[The rest of this file is not shown.]" else ""
    Fragment(Layer.Place, file.path, s"Instructions from ${file.path}:\n\n${file.text}$note")
  }
}
