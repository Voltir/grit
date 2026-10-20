package grit.knowledge.card

import java.time.Instant

import grit.core.document.{Document, DocumentShelf}
import grit.core.id.{DocKey, DocumentVersion}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label
import grit.knowledge.ref.{CardKey, Ref}

/** Cards as another plugin reads them through [[grit.knowledge.plugin.Knowledge]]'s export. */
trait CardShelf extends caps.Pure {

  /** Every card its transaction reads written from `since` until `until`, both included, most
    * recent first, up to the `n`th of them `counts` holds to (so at most `n` when it holds to
    * all), each by its version, key, title and what its pointer reads ([[Ref.ofPointer]]):
    * never its summary, which a model reads only as a document shown to
    * [[grit.core.act.Dreaming.summarise]].
    */
  def changed(since: Instant, until: Instant, n: Int, counts: CardShelf.Seen -> Boolean)(using
      Tx^
  ): Either[StoreError, Vector[CardShelf.Seen]]

  /** Every card its transaction reads written by `until`, whatever its age, whose unit
    * ([[CardShelf.Seen.unit]]) is one of `units`, most recent first, at most `n`.
    */
  def linked(units: Set[Ref], until: Instant, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[CardShelf.Seen]]
}

object CardShelf {

  /** A card as [[CardShelf.changed]] sees it: its document's `version`, its `key`, its `title`
    * (its unit's name, or its path when its source gave none), the `unit` its pointer reads,
    * whether that is a file or a directory (`kind`), and the `label` it is kept at.
    */
  final case class Seen(
      version: DocumentVersion,
      key: CardKey,
      title: String,
      unit: Option[Ref],
      kind: UnitKind,
      label: Label
  ) extends caps.Pure

  /** The cards among `documents`, the knowledge plugin's own. */
  def over(documents: DocumentShelf): CardShelf = new CardShelf {
    def changed(since: Instant, until: Instant, n: Int, counts: Seen -> Boolean)(using
        Tx^
    ): Either[StoreError, Vector[Seen]] =
      documents.newestWhere(n, _.written.isBefore(since))(card(until), counts)

    def linked(units: Set[Ref], until: Instant, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[Seen]] =
      if (units.isEmpty) Right(Vector.empty)
      else
        documents.newestWhere(n, _ => false)(
          card(until)(_).filter(_.unit.exists(units.contains)),
          _ => true
        )
  }

  /** `d` as a card seen, or `None` when it is no card or written after `until`. */
  private def card(until: Instant)(d: Document): Option[Seen] =
    for {
      _ <- Option.unless(d.written.isAfter(until))(())
      key <- CardKey.parse(DocKey.value(d.key)).toOption
      o <- d.data.objOpt
      path <- o.get("path").flatMap(_.strOpt)
    } yield Seen(
      d.version,
      key,
      o.get("title").flatMap(_.strOpt).getOrElse(path),
      d.pointer.flatMap(Ref.ofPointer),
      UnitKind.of(o.get("kind").flatMap(_.strOpt)),
      d.label
    )
}
