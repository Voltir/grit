package grit.knowledge.sync

import grit.core.document.Document
import grit.core.durable.Journaled
import grit.core.id.{CorpusName, DocKey}
import grit.core.json.Fields
import grit.core.place.Place
import grit.core.visibility.Label
import grit.knowledge.card.{Scope, UnitVersion}
import grit.knowledge.ref.CardKey

/** A card a run holds, as its `held` keep reads it: its key, its unit's path, the label it is
  * kept at, its unit's version, the scope a directory's card was made under, the path of the
  * listing a file's card records it was shown in ([[grit.knowledge.card.Card.within]]), and
  * whether it is kept `elsewhere` than the place its source declares ([[Source.at]]).
  */
final case class Held private (
    key: CardKey,
    path: String,
    label: Label,
    version: UnitVersion,
    scope: Option[Scope],
    within: Option[String],
    elsewhere: Boolean
) extends CardCopy

object Held {

  /** The card of `corpus`'s unit at `path`, kept at `label` at `version`, made under `scope` if
    * a directory's, shown `within` the listing at that path if recorded, kept `elsewhere` than
    * its source's place or not, keyed [[grit.knowledge.card.CardKey.of]] of `corpus` and `path`.
    */
  def of(
      corpus: CorpusName,
      path: String,
      label: Label,
      version: UnitVersion,
      scope: Option[Scope],
      within: Option[String] = None,
      elsewhere: Boolean = false
  ): Held = new Held(CardKey.of(corpus, path), path, label, version, scope, within, elsewhere)

  /** The card as its run's record holds it: `key` is read back as it was written. */
  private[sync] def recorded(
      key: CardKey,
      path: String,
      label: Label,
      version: UnitVersion,
      scope: Option[Scope],
      within: Option[String],
      elsewhere: Boolean
  ): Held = new Held(key, path, label, version, scope, within, elsewhere)

  /** `document` as `corpus`'s card: [[Read.Theirs]] when its key is no card key, its data holds
    * no string `path`, or its key is not `corpus`'s for that path (nothing else of it is read);
    * [[Read.Damaged]], saying why, when its data's `version` is not a string that is a version
    * ([[UnitVersion.of]]), its `scope` no scope, or its `within` no string; else [[Read.Ours]],
    * the card held, kept elsewhere when its place is not `at`.
    */
  def read(corpus: CorpusName, at: Place, document: Document): Read = {
    val data = document.data.objOpt
    val owned = for {
      key <- CardKey.parse(DocKey.value(document.key)).toOption
      path <- data.flatMap(_.get("path")).flatMap(_.strOpt)
      if CardKey.of(corpus, path) == key
    } yield (key, path)
    owned.fold(Read.Theirs) { (key, path) =>
      val fields = for {
        version <- data.flatMap(_.get("version")) match {
          case Some(value) =>
            value.strOpt
              .toRight(())
              .flatMap(UnitVersion.of(_).left.map(_ => ()))
              .left
              .map(_ => s"its version, ${shown(value)}, is no git SHA or instant")
          case None => Left("it holds no version")
        }
        scope <- data.flatMap(_.get("scope")) match {
          case None => Right(None)
          case Some(value) =>
            value.strOpt
              .toRight(())
              .flatMap(Scope.parse(_).left.map(_ => ()))
              .map(Some(_))
              .left
              .map(_ => s"its scope, ${shown(value)}, is no scope")
        }
        within <- data.flatMap(_.get("within")) match {
          case None => Right(None)
          case Some(value) =>
            value.strOpt.map(Some(_)).toRight(s"its within, ${shown(value)}, is no string")
        }
      } yield new Held(key, path, document.label, version, scope, within, document.place != at)
      fields.fold(Read.Damaged(key, path, document.label, _), Read.Ours(_))
    }
  }

  /** A field's value as a reply names it: a string as it is, anything else as JSON. */
  private def shown(value: ujson.Value): String = value.strOpt.getOrElse(ujson.write(value))

  /** A document as [[read]] reads it. */
  enum Read extends caps.Pure {

    /** Another source's document, or one no source wrote. */
    case Theirs

    /** Its source's card, held. */
    case Ours(held: Held)

    /** Its source's card, under `key` for the unit at `path`, kept at `label`, that does not
      * read, and why: a run holds it as no card, so writes the unit's card anew or withdraws it.
      */
    case Damaged(key: CardKey, path: String, label: Label, why: String) extends Read, CardCopy
  }
}

/** A copy of a card a run holds, whether or not it reads: its key, its unit's path, and the
  * label it is kept at.
  */
sealed trait CardCopy extends caps.Pure {
  def key: CardKey
  def path: String
  def label: Label
}

/** What a run's `held` keep read: the floor its run keeps cards at, and the cards it holds,
  * by key; the files, their cards current, whose pieces it reads them again for
  * (`repiece`): none kept at the floor, or kept by a format the area no longer reads; and the
  * copies of its source's cards that do not read (`damaged`).
  */
private[knowledge] final case class Holding(
    floor: Label,
    held: Vector[Held],
    repiece: Set[CardKey] = Set.empty,
    damaged: Vector[Held.Read.Damaged] = Vector.empty
) extends caps.Pure {
  private val byKey: Map[CardKey, Vector[Held]] = held.groupBy(_.key)

  /** The copy of `key` a walk goes by: the one at the floor, else the first held. */
  def best(key: CardKey): Option[Held] = {
    val copies = byKey.getOrElse(key, Vector.empty)
    copies.find(_.label == floor).orElse(copies.headOption)
  }

  /** The label of the copy a write or relabel of `key` replaces: its [[best]] copy's, else the
    * first damaged copy's; none when that is at the floor, where the write replaces it anyway.
    */
  def replacing(key: CardKey): Option[Label] =
    best(key)
      .map(_.label)
      .orElse(damaged.find(d => d.key == key && d.label != floor).map(_.label))
      .filter(_ != floor)

  /** The label a relabel of `key`, unchanged, copies from: its [[best]] copy's, when that is held
    * only at another label or kept elsewhere than its source's place.
    */
  def copying(key: CardKey): Option[Label] =
    best(key).filter(h => h.label != floor || h.elsewhere).map(_.label)

  /** Whether no copy of `key` is held, damaged or not. */
  def isNew(key: CardKey): Boolean = !byKey.contains(key) && !damaged.exists(_.key == key)

  /** How many units it holds a card of, damaged or not. */
  def units: Int = (byKey.keySet ++ damaged.map(_.key)).size
}

private[knowledge] object Holding {

  /** Its record: `{"floor", "held"}`, each held card `{"key", "path", "label", "version"}`, a
    * directory's `"scope"` and a file's `"within"` where it has them, and `"elsewhere": true`
    * only for a card kept elsewhere, so a record written before cards were held by place reads
    * back as every card at its source's; then, only when some are damaged, `"damaged"`, each
    * `{"key", "path", "label", "why"}`, so a record written before damaged cards were held reads
    * back as holding none; each label in its written form ([[Label.written]]), never taken apart.
    */
  given Journaled[Holding] = Journaled.json(
    h => {
      val record = ujson.Obj(
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
          if (c.elsewhere) card("elsewhere") = true
          card
        })
      )
      if (h.damaged.nonEmpty)
        record("damaged") = ujson.Arr.from(h.damaged.map { d =>
          ujson.Obj(
            "key" -> CardKey.value(d.key),
            "path" -> d.path,
            "label" -> Label.written(d.label),
            "why" -> d.why
          )
        })
      record
    },
    json =>
      for {
        o <- json.objOpt.toRight(s"a held keep's record is an object: ${ujson.write(json)}")
        floor <- o.get("floor").flatMap(_.strOpt).toRight("a held keep's record names its floor")
        f <- Label.read(floor)
        cards <- o.get("held").flatMap(_.arrOpt).toRight("a held keep's record lists its cards")
        held <- each(cards.toVector, card)
        damaged <- o.get("damaged") match {
          case None => Right(Vector.empty)
          case Some(value) =>
            value.arrOpt
              .toRight(s"a held keep's record lists its damaged cards: ${ujson.write(value)}")
              .flatMap(ds => each(ds.toVector, damagedCard))
        }
      } yield Holding(f, held, damaged = damaged)
  )

  /** Each of `values` as `read` reads it, or why the first does not. */
  private def each[A](
      values: Vector[ujson.Value],
      read: ujson.Value => Either[String, A]
  ): Either[String, Vector[A]] =
    values.foldLeft[Either[String, Vector[A]]](Right(Vector.empty)) { (acc, v) =>
      acc.flatMap(as => read(v).map(as :+ _))
    }

  private def damagedCard(json: ujson.Value): Either[String, Held.Read.Damaged] = {
    val f = Fields(s"a held card's record ${ujson.write(json)}", json)
    for {
      key <- f.str("key").flatMap(CardKey.parse)
      path <- f.str("path")
      label <- f.str("label").flatMap(Label.read)
      why <- f.str("why")
    } yield Held.Read.Damaged(key, path, label, why)
  }

  private def card(json: ujson.Value): Either[String, Held] = {
    val f = Fields(s"a held card's record ${ujson.write(json)}", json)
    for {
      key <- f.str("key").flatMap(CardKey.parse)
      path <- f.str("path")
      label <- f.str("label").flatMap(Label.read)
      version <- f.str("version").flatMap(UnitVersion.of)
      scope <- f.addedStr("scope").flatMap {
        case None => Right(None)
        case Some(s) => Scope.parse(s).map(Some(_))
      }
      within <- f.addedStr("within")
      elsewhere <- f
        .added("elsewhere")
        .flatMap(v => if (v.isEmpty) Right(false) else f.bool("elsewhere"))
    } yield Held.recorded(key, path, label, version, scope, within, elsewhere)
  }
}
