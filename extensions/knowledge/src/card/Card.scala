package grit.knowledge.card

import grit.core.document.{DocText, Pointer}
import grit.core.id.{CorpusName, DocKey}

/** What a unit of a source is. */
enum UnitKind(val word: String) {
  case File extends UnitKind("file")
  case Directory extends UnitKind("directory")
}

/** A unit's card: its key, its path in its source, its kind and version, the scope a
  * directory's card was made under (none for a file's), its summary, and the call that reads it;
  * its document's key, text and data.
  */
final case class Card private (
    key: CardKey,
    path: String,
    kind: UnitKind,
    version: UnitVersion,
    scope: Option[Scope],
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

  /** Its document's data: `{"kind": "file" or "directory", "path", "terms", "version"}`, and a
    * directory's `"scope"`.
    */
  def data: ujson.Obj =
    ujson.Obj.from(
      Vector[(String, ujson.Value)](
        "kind" -> kind.word,
        "path" -> path,
        "terms" -> ujson.Arr.from(summary.terms.map(ujson.Str(_))),
        "version" -> UnitVersion.value(version)
      ) ++ scope.map(s => "scope" -> ujson.Str(Scope.value(s)))
    )
}

object Card {

  /** The card of `corpus`'s file at `path`, keyed [[CardKey.of]] of the two. */
  def file(
      corpus: CorpusName,
      path: String,
      version: UnitVersion,
      summary: CardSummary,
      pointer: Pointer
  ): Card =
    new Card(CardKey.of(corpus, path), path, UnitKind.File, version, None, summary, pointer)

  /** The card of `corpus`'s directory at `path`, made under `scope`, keyed [[CardKey.of]] of
    * `corpus` and `path`.
    */
  def directory(
      corpus: CorpusName,
      path: String,
      version: UnitVersion,
      scope: Scope,
      summary: CardSummary,
      pointer: Pointer
  ): Card = new Card(
    CardKey.of(corpus, path),
    path,
    UnitKind.Directory,
    version,
    Some(scope),
    summary,
    pointer
  )
}
