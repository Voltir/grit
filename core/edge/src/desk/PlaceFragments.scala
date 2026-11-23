package grit.edge.desk

import grit.core.host.{Clipped, InstructionFile, RepositoryInstructions}
import grit.core.place.{Place, Repository}
import grit.core.prompt.{Fragment, Layer}
import grit.core.provider.TokenEstimator

/** A place's own instructions as the Place layer of a system prompt: its instruction files,
  * bounded so a deep tree cannot crowd out the rest, or the words a deployment declared for
  * it; data from the place, never grit's words.
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
        val kept = Clipped.prefix(f.text, AncestorBytes)
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

  /** `text`, words a deployment declared for `place`, as its Place fragment: from source
    * `place.written`, its text "Instructions for {place.written}:", a blank line, then `text`
    * trimmed. `Left` saying why when `text` is blank.
    */
  def declared(place: Place, text: String): Either[String, Fragment] = {
    val said = text.trim
    if (said.isEmpty) Left(s"the instructions declared for ${place.written} are blank")
    else Right(Fragment(Layer.Place, place.written, s"Instructions for ${place.written}:\n\n$said"))
  }

  /** `file`, at `repository`'s root, as its Place fragment: from source
    * `repository:{name}/{file.name}`, its text "Instructions from {file.name}:", a blank line,
    * then the file's text, a cut one saying so on its last line.
    */
  def repository(repository: Repository, file: RepositoryInstructions): Fragment =
    fragment(s"${repository.place.written}/${file.name}", file.name, file.text, file.cut)

  /** `file` as its fragment: its text under a heading naming it. */
  private def fragment(file: InstructionFile): Fragment =
    fragment(file.path, file.path, file.text, file.cut)

  /** `text` from `source`, under a heading naming it `named`, saying when it was `cut`. */
  private def fragment(source: String, named: String, text: String, cut: Boolean): Fragment = {
    val note = if (cut) "\n\n[The rest of this file is not shown.]" else ""
    Fragment(Layer.Place, source, s"Instructions from $named:\n\n$text$note")
  }
}
