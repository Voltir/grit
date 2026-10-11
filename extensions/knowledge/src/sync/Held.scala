package grit.knowledge.sync

import grit.core.document.Document
import grit.core.id.{CorpusName, DocKey}
import grit.core.visibility.Label
import grit.knowledge.card.{CardKey, UnitVersion}

/** A card a run holds, as its `held` keep reads it: its key, its unit's path, the label it is
  * kept at, and its unit's version.
  */
final case class Held private (key: CardKey, path: String, label: Label, version: UnitVersion)
    extends caps.Pure

object Held {

  /** The card of `corpus`'s unit at `path`, kept at `label` at `version`, keyed
    * [[grit.knowledge.card.CardKey.of]] of `corpus` and `path`.
    */
  def of(corpus: CorpusName, path: String, label: Label, version: UnitVersion): Held =
    new Held(CardKey.of(corpus, path), path, label, version)

  /** `document`, one of the knowledge plugin's, as a card of `corpus` that a run holds; `None`
    * when it is another source's card (its key is not that of `corpus` and its data's path). Or
    * why not, naming its key: the key is no card's key, or its data holds no string `path`, or
    * no string `version` that is a git SHA.
    */
  def read(corpus: CorpusName, document: Document): Either[String, Option[Held]] = {
    val k = DocKey.value(document.key)
    def no(why: String) = s"the document $k is no card: $why"
    def str(field: String): Either[String, String] =
      document.data.objOpt
        .flatMap(_.get(field))
        .flatMap(_.strOpt)
        .toRight(no(s"its data holds no string $field"))
    for {
      key <- CardKey.parse(k).left.map(_ => no("its key is not 64 lowercase hex digits"))
      path <- str("path")
      text <- str("version")
      version <- UnitVersion.of(text).left.map(_ => no(s"its data's version, $text, is no git SHA"))
    } yield Option.when(CardKey.of(corpus, path) == key)(
      new Held(key, path, document.label, version)
    )
  }
}
