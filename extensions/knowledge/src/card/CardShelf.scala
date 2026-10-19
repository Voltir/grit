package grit.knowledge.card

import java.time.Instant

import grit.core.document.{Document, DocumentShelf}
import grit.core.id.{DocKey, DocumentVersion}
import grit.core.store.{StoreError, Tx}
import grit.knowledge.ref.{CardKey, Ref}

/** Cards as another plugin reads them through [[grit.knowledge.plugin.Knowledge]]'s export. */
trait CardShelf extends caps.Pure {

  /** Every card its transaction reads written from `since` until `until`, both included, most
    * recent first, at most `n`, each by its version, key, title and what its pointer reads
    * ([[Ref.ofPointer]]): never its summary, which a model reads only as a document shown to
    * [[grit.core.act.Dreaming.summarise]].
    */
  def changed(since: Instant, until: Instant, n: Int)(using
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
    * (its unit's name, or its path when its source gave none) and the `unit` its pointer reads.
    */
  final case class Seen(version: DocumentVersion, key: CardKey, title: String, unit: Option[Ref])
      extends caps.Pure

  /** The cards among `documents`, the knowledge plugin's own. */
  def over(documents: DocumentShelf): CardShelf = new CardShelf {
    def changed(since: Instant, until: Instant, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[Seen]] =
      newest(since, until, n)(_ => true)

    def linked(units: Set[Ref], until: Instant, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[Seen]] =
      if (units.isEmpty) Right(Vector.empty)
      else newest(Instant.MIN, until, n)(_.unit.exists(units.contains))

    /** The cards `keep` keeps among those written from `since` until `until`, most recent first,
      * at most `n`.
      */
    private def newest(since: Instant, until: Instant, n: Int)(keep: Seen => Boolean)(using
        Tx^
    ): Either[StoreError, Vector[Seen]] = {
      // The newest `k` documents, `k` doubled until they hold `n` cards kept, reach one written
      // before `since` (every later one is older), or are all there are.
      def from(k: Int): Either[StoreError, Vector[Seen]] =
        documents.newest(k).flatMap { ds =>
          val seen = ds
            .filterNot(d => d.written.isBefore(since) || d.written.isAfter(until))
            .flatMap(card)
            .filter(keep)
          val done =
            seen.size >= n || ds.size < k || ds.lastOption.exists(_.written.isBefore(since))
          if (done || k > Int.MaxValue / 2) Right(seen.take(n)) else from(k * 2)
        }
      if (n <= 0) Right(Vector.empty) else from(n)
    }
  }

  /** `d` as a card seen, or `None` when it is no card. */
  private def card(d: Document): Option[Seen] =
    for {
      key <- CardKey.parse(DocKey.value(d.key)).toOption
      o <- d.data.objOpt
      path <- o.get("path").flatMap(_.strOpt)
    } yield Seen(
      d.version,
      key,
      o.get("title").flatMap(_.strOpt).getOrElse(path),
      d.pointer.flatMap(Ref.ofPointer)
    )
}
