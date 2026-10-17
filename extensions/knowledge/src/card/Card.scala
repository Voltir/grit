package grit.knowledge.card

import java.time.ZoneOffset

import grit.core.document.{DocText, Pointer}
import grit.core.id.{CorpusName, DocKey}

/** What a unit of a source is. */
enum UnitKind(val word: String) {
  case File extends UnitKind("file")
  case Directory extends UnitKind("directory")
}

/** A unit's card: its key, its path in its source, its kind and version, the scope a
  * directory's card was made under (none for a file's), its summary, the call that reads it, and
  * its title, the unit's name as its source shows it, when it gave one; the path of the listing
  * that showed a file, when its source records it (`within`); its document's key, text and data.
  */
final case class Card private (
    key: CardKey,
    path: String,
    kind: UnitKind,
    version: UnitVersion,
    scope: Option[Scope],
    summary: CardSummary,
    pointer: Pointer,
    title: Option[String],
    within: Option[String]
) extends caps.Pure {

  /** Its document's key: [[key]]'s hex digits. */
  def docKey: DocKey = CardKey.docKey(key)

  /** Its document's text: the summary, then, after a blank line, `Terms: ` and its terms
    * separated by `, `; the summary alone when it has no terms. A card whose version is
    * [[UnitVersion.Changed]] begins with a line `{title, or its kind}, last changed {yyyy-MM-dd}.`
    * (the day in UTC) and a blank line.
    */
  def text: DocText = {
    val body =
      if (summary.terms.isEmpty) summary.summary
      else s"${summary.summary}\n\nTerms: ${summary.terms.mkString(", ")}"
    val s = changedDay.fold(body) { day =>
      s"${title.getOrElse(kind.word.capitalize)}, last changed $day.\n\n$body"
    }
    // The summary is never blank (CardSummary.of), so neither is the text.
    DocText.of(s).fold(why => throw new IllegalStateException(why), identity)
  }

  /** Its document's data: `{"kind": "file" or "directory", "path", "terms", "version"}`, a
    * directory's `"scope"`, its `"title"` and `"within"` when it has them, and when its version
    * is [[UnitVersion.Changed]], `"changed"`, the day (`yyyy-MM-dd`, UTC).
    */
  def data: ujson.Obj =
    ujson.Obj.from(
      Vector[(String, ujson.Value)](
        "kind" -> kind.word,
        "path" -> path,
        "terms" -> ujson.Arr.from(summary.terms.map(ujson.Str(_))),
        "version" -> UnitVersion.value(version)
      ) ++ scope.map(s => "scope" -> ujson.Str(Scope.value(s)))
        ++ changedDay.map(d => "changed" -> ujson.Str(d))
        ++ title.map(t => "title" -> ujson.Str(t))
        ++ within.map(w => "within" -> ujson.Str(w))
    )

  private def changedDay: Option[String] = version match {
    case UnitVersion.Changed(at) => Some(at.atOffset(ZoneOffset.UTC).toLocalDate.toString)
    case UnitVersion.Sha(_) => None
  }
}

object Card {

  /** The card of `corpus`'s file at `path`, listed `within` the listing at that path when given,
    * keyed [[CardKey.of]] of the two. `title` is kept as at most its first 80 characters, its
    * line breaks made spaces; a blank one is none.
    */
  def file(
      corpus: CorpusName,
      path: String,
      version: UnitVersion,
      summary: CardSummary,
      pointer: Pointer,
      title: Option[String] = None,
      within: Option[String] = None
  ): Card =
    new Card(
      CardKey.of(corpus, path),
      path,
      UnitKind.File,
      version,
      None,
      summary,
      pointer,
      title.map(_.replaceAll("\\R", " ").strip).filter(_.nonEmpty).map(cut),
      within
    )

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
    pointer,
    None,
    None
  )

  private val TitleMost = 80

  /** `title`'s first [[TitleMost]] code points, never splitting a character. */
  private def cut(title: String): String =
    if (title.codePointCount(0, title.length) <= TitleMost) title
    else title.substring(0, title.offsetByCodePoints(0, TitleMost))
}
