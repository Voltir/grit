package grit.knowledge.sync

import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.id.{CorpusName, DocKey}
import grit.core.visibility.Label
import grit.knowledge.card.{CardKey, Scope, UnitVersion}

/** A card a run holds, as its `held` keep reads it: its key, its unit's path, the label it is
  * kept at, its unit's version, the scope a directory's card was made under, and the path of
  * the listing a file's card records it was shown in ([[grit.knowledge.card.Card.within]]).
  */
final case class Held private (
    key: CardKey,
    path: String,
    label: Label,
    version: UnitVersion,
    scope: Option[Scope],
    within: Option[String]
) extends caps.Pure

object Held {

  /** The card of `corpus`'s unit at `path`, kept at `label` at `version`, made under `scope` if
    * a directory's, shown `within` the listing at that path if recorded, keyed
    * [[grit.knowledge.card.CardKey.of]] of `corpus` and `path`.
    */
  def of(
      corpus: CorpusName,
      path: String,
      label: Label,
      version: UnitVersion,
      scope: Option[Scope],
      within: Option[String] = None
  ): Held = new Held(CardKey.of(corpus, path), path, label, version, scope, within)

  /** The card as its run's record holds it: `key` is read back as it was written. */
  private[sync] def recorded(
      key: CardKey,
      path: String,
      label: Label,
      version: UnitVersion,
      scope: Option[Scope],
      within: Option[String]
  ): Held = new Held(key, path, label, version, scope, within)

  /** `document`, one of the knowledge plugin's, as a card of `corpus` that a run holds; `None`
    * when it is another source's card (its key is not that of `corpus` and its data's path). Or
    * why not, naming its key: the key is no card's key, or its data holds no string `path`, or
    * no string `version` that is a version ([[UnitVersion.of]]), or a `scope` that is no scope,
    * or a `within` that is no string.
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
      version <- UnitVersion
        .of(text)
        .left
        .map(_ => no(s"its data's version, $text, is no git SHA or instant"))
      scope <- document.data.objOpt.flatMap(_.get("scope")) match {
        case None => Right(None)
        case Some(value) =>
          value.strOpt
            .toRight(())
            .flatMap(Scope.parse(_).left.map(_ => ()))
            .map(Some(_))
            .left
            .map(_ => no(s"its data's scope, ${ujson.write(value)}, is no scope"))
      }
      within <- document.data.objOpt.flatMap(_.get("within")) match {
        case None => Right(None)
        case Some(value) =>
          value.strOpt
            .map(Some(_))
            .toRight(no(s"its data's within, ${ujson.write(value)}, is no string"))
      }
    } yield Option.when(CardKey.of(corpus, path) == key)(
      new Held(key, path, document.label, version, scope, within)
    )
  }
}

/** What a run's `held` keep read: the floor its run keeps cards at, and the cards it holds,
  * by key.
  */
private[sync] final case class Holding(floor: Label, held: Vector[Held]) extends caps.Pure

private[sync] object Holding {

  /** Its record: `{"floor", "held"}`, each held card `{"key", "path", "label", "version"}`, a
    * directory's `"scope"` and a file's `"within"` where it has them, each label in its written
    * form ([[Label.written]]), never taken apart.
    */
  given Journaled[Holding] = Journaled.json(
    h =>
      ujson.Obj(
        "floor" -> Label.written(h.floor),
        "held" -> ujson.Arr.from(h.held.map { c =>
          val card = ujson.Obj(
            "key" -> CardKey.value(c.key),
            "path" -> c.path,
            "label" -> Label.written(c.label),
            "version" -> UnitVersion.value(c.version)
          )
          c.scope.foreach(s => card("scope") = Scope.value(s))
          c.within.foreach(w => card("within") = w)
          card
        })
      ),
    json =>
      for {
        o <- json.objOpt.toRight(s"a held keep's record is an object: ${ujson.write(json)}")
        floor <- o.get("floor").flatMap(_.strOpt).toRight("a held keep's record names its floor")
        f <- Label.read(floor)
        cards <- o.get("held").flatMap(_.arrOpt).toRight("a held keep's record lists its cards")
        held <- cards.toVector.foldLeft[Either[String, Vector[Held]]](Right(Vector.empty)) {
          (acc, c) => acc.flatMap(hs => card(c).map(hs :+ _))
        }
      } yield Holding(f, held)
  )

  private def card(json: ujson.Value): Either[String, Held] = {
    def str(field: String): Either[String, String] =
      json.objOpt
        .flatMap(_.get(field))
        .flatMap(_.strOpt)
        .toRight(s"a held card's record holds no string $field: ${ujson.write(json)}")
    for {
      key <- str("key").flatMap(CardKey.parse)
      path <- str("path")
      label <- str("label").flatMap(Label.read)
      version <- str("version").flatMap(UnitVersion.of)
      scope <- json.objOpt.flatMap(_.get("scope")) match {
        case None => Right(None)
        case Some(_) => str("scope").flatMap(Scope.parse).map(Some(_))
      }
      within <- json.objOpt.flatMap(_.get("within")) match {
        case None => Right(None)
        case Some(_) => str("within").map(Some(_))
      }
    } yield Held.recorded(key, path, label, version, scope, within)
  }
}
