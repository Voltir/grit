package grit.knowledge.card

import grit.core.document.{DocText, Pointer}
import grit.core.id.{CorpusName, DocKey}

/** What a unit of a source is. */
enum UnitKind(val word: String) {
  case File extends UnitKind("file")
  case Directory extends UnitKind("directory")
}

/** A unit's card: its key, its path in its source, its kind and version, its summary, and the
  * call that reads it; its document's key, text and data.
  */
final case class Card private (
    key: CardKey,
    path: String,
    kind: UnitKind,
    version: UnitVersion,
    summary: CardSummary,
    pointer: Pointer
) extends caps.Pure {

  /** Its document's key: [[key]]'s hex digits. */
  def docKey: DocKey =
    // 64 hex digits, one line within 200 characters: DocKey.of never refuses them.
    DocKey.of(CardKey.value(key)).fold(why => throw new IllegalStateException(why), identity)

  /** Its document's text: the summary, then, after a blank line, `Terms: ` and its terms
    * separated by `, `; the summary alone when it has no terms.
    */
  def text: DocText = {
    val s =
      if (summary.terms.isEmpty) summary.summary
      else s"${summary.summary}\n\nTerms: ${summary.terms.mkString(", ")}"
    // The summary is never blank (CardSummary.of), so neither is the text.
    DocText.of(s).fold(why => throw new IllegalStateException(why), identity)
  }

  /** Its document's data: `{"kind": "file" or "directory", "path", "terms", "version"}`. */
  def data: ujson.Obj =
    ujson.Obj(
      "kind" -> kind.word,
      "path" -> path,
      "terms" -> ujson.Arr.from(summary.terms.map(ujson.Str(_))),
      "version" -> UnitVersion.value(version)
    )
}

object Card {

  /** The card of `corpus`'s unit at `path`, keyed [[CardKey.of]] of the two. */
  def of(
      corpus: CorpusName,
      path: String,
      kind: UnitKind,
      version: UnitVersion,
      summary: CardSummary,
      pointer: Pointer
  ): Card = new Card(CardKey.of(corpus, path), path, kind, version, summary, pointer)
}
