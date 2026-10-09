package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{InMemoryTombstones, Savepoints, StoreError, Tx}
import grit.core.visibility.{Item, Label}

/** An in-memory [[DocumentStore]], with each plugin's [[DocumentKeeper]], for tests, keeping
  * [[DocumentContract]]. Versions are numbered by one counter across every plugin. It marks
  * versions in `tombstones`, which reads which plugin's a version is from it. Search scores a
  * document by how often the query's words occur in it, not by BM25: the contract's searches
  * order strictly under either. It floors, places and filters by the `Tx`'s clearance, and
  * otherwise ignores it: nothing is rolled back but by its [[savepoints]].
  */
final class InMemoryDocuments extends DocumentStore {
  import InMemoryDocuments.Row

  // Only ever replaced by a new immutable map, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[Long, Row]

  // The identity column's last value, one across every plugin: only ever counted up.
  @caps.unsafe.untrackedCaptures
  private var last = 0L

  // As `rows`: only ever replaced by a new immutable map. Each plugin's terms, and whether it
  // is enabled.
  @caps.unsafe.untrackedCaptures
  private var terms = Map.empty[PluginName, (DocumentTerms, Boolean)]

  /** Where its keepers mark the versions they leave for deletion. */
  val tombstones: InMemoryTombstones =
    new InMemoryTombstones(v => rows.get(DocumentVersion.value(v)).map(_.plugin))

  /** Its part of a transaction undone: what its keepers wrote, and the versions they marked. */
  val savepoints: Savepoints = new Savepoints {
    def atomic[A](body: Tx^ ?=> Either[StoreError, A])(using
        tx: Tx^
    ): Either[StoreError, A] = {
      // Its identity column is not rolled back, as a sequence is not: `last` stays.
      val before = rows
      val out = tombstones.undoing(body(using tx))
      if (out.isLeft) rows = before
      out
    }
  }

  /** `plugin`'s documents as it writes them, under `under`. */
  def keeper(plugin: PluginName, under: DocumentTerms): DocumentKeeper =
    new Shelf(plugin) with DocumentKeeper {
      def write(
          key: DocKey,
          label: Label,
          place: Place,
          text: DocText,
          data: ujson.Value,
          at: Instant
      )(using tx: Tx^): Either[StoreError, Written] = {
        val clearance = Tx.clearance(tx)
        val kept = label.join(clearance.floor)
        head(plugin, key, kept) match {
          case Some((v, r)) if r.text.contains(text) && r.place == place && r.data == data =>
            Right(Written.Unchanged(version(v), kept))
          case before =>
            for {
              _ <- before.fold(Right(()))((v, r) => supersede(v, r, at))
              v = insert(
                Row(
                  plugin,
                  key,
                  kept,
                  clearance.own.map(_.room),
                  place,
                  Some(text),
                  data,
                  at,
                  None,
                  Placement(0, at)
                )
              )
              over = holding(plugin).filterNot((_, r) => r.key == key && r.label == kept)
              excess = (over.size + 1 - under.bound) max 0
              gone = over
                .sortBy((_, r) =>
                  (r.placement.lastPlaced, DocKey.value(r.key), Label.written(r.label))
                )
                .take(excess)
                .map((_, r) => (r.key, r.label))
              _ <- gone.foldLeft[Either[StoreError, Unit]](Right(()))((acc, kl) =>
                acc.flatMap(_ => withdraw(kl._1, kl._2, at).map(_ => ()))
              )
            } yield Written.Versioned(version(v), kept, gone)
        }
      }

      def withdraw(key: DocKey, label: Label, at: Instant)(using
          Tx^
      ): Either[StoreError, Option[DocumentVersion]] =
        head(plugin, key, label) match {
          case Some((v, r)) if r.text.nonEmpty =>
            for {
              _ <- supersede(v, r, at)
              w = insert(
                r.copy(
                  text = None,
                  data = ujson.Obj(),
                  written = at,
                  superseded = None,
                  placement = Placement(0, at)
                )
              )
              _ <- tombstones.write(Target.Document(version(w)), at)
            } yield Some(version(w))
          case _ => Right(None)
        }
    }

  /** `plugin`'s documents as it reads them. */
  def shelf(plugin: PluginName): DocumentShelf = new Shelf(plugin)

  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
    Right(terms.toVector.collect { case (p, (t, true)) => p -> t })

  /** `r`, version `v`, no longer current from `at`, and marked for deletion. */
  private def supersede(v: Long, r: Row, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    rows = rows.updated(v, r.copy(superseded = Some(at)))
    tombstones.write(Target.Document(version(v)), at).map(_ => ())
  }

  /** Whether `r` held something current at `at`: written before it, superseded by none
    * written before it.
    */
  private def currentAt(r: Row, at: Instant): Boolean =
    r.text.nonEmpty && r.written.isBefore(at) && r.superseded.forall(!_.isBefore(at))

  def shelved(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Shelved]] =
    Right(
      rows.values.toVector
        .filter(r => plugins.contains(r.plugin) && currentAt(r, at) && reads(r))
        .map(r => Shelved(r.plugin, r.place))
        .distinct
    )

  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]] = {
    val words = InMemoryDocuments.words(query).distinct
    Right(
      if (words.isEmpty) Vector.empty
      else
        rows.toVector
          .collect {
            case (v, r @ Row(plugin, _, _, _, place, Some(text), _, _, _, _))
                if shelves.contains(Shelved(plugin, place)) && currentAt(r, at) && reads(r) =>
              val found = InMemoryDocuments.words(DocText.value(text))
              val score = words.map(w => found.count(_ == w)).sum.toDouble
              DocumentSearch.Hit(document(v, r, text), score)
          }
          .filter(_.score > 0)
          .sortBy(h =>
            (-h.score, -h.document.written.toEpochMilli, -DocumentVersion.value(h.document.version))
          )
          .take(limit max 0)
    )
  }

  def read(versions: Vector[DocumentVersion])(using Tx^): Either[StoreError, Vector[Document]] =
    Right(versions.flatMap { v =>
      rows
        .get(DocumentVersion.value(v))
        .filter(reads)
        .flatMap(r => r.text.map(document(DocumentVersion.value(v), r, _)))
    })

  def declare(enabled: Vector[(PluginName, DocumentTerms)])(using Tx^): Either[StoreError, Unit] = {
    terms = terms.map((p, t) => p -> (t._1, false)) ++ enabled.map((p, t) => p -> (t, true))
    Right(())
  }

  def placed(
      versions: Vector[DocumentVersion],
      at: Instant
  )(using Tx^): Either[StoreError, Unit] = {
    versions.map(DocumentVersion.value).foreach { v =>
      rows.get(v).foreach { r =>
        val latest = if (at.isAfter(r.placement.lastPlaced)) at else r.placement.lastPlaced
        rows = rows.updated(v, r.copy(placement = Placement(r.placement.count + 1, latest)))
      }
    }
    Right(())
  }

  def kept()(using Tx^): Either[StoreError, Vector[PluginName]] =
    Right((rows.values.map(_.plugin).toVector ++ terms.keys).distinct)

  def forget(version: DocumentVersion)(using Tx^): Either[StoreError, Unit] = {
    rows = rows - DocumentVersion.value(version)
    Right(())
  }

  def remove(plugin: PluginName, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    val gone = rows.collect { case (v, r) if r.plugin == plugin => v }.toSet
    val ended = tombstones.pending.map(_.target).foldLeft[Either[StoreError, Unit]](Right(())) {
      case (acc, t @ Target.Document(v)) if gone(DocumentVersion.value(v)) =>
        acc.flatMap(_ => tombstones.collected(t, at))
      case (acc, _) => acc
    }
    rows = rows -- gone
    terms = terms - plugin
    ended
  }

  private class Shelf(plugin: PluginName) extends DocumentShelf {
    def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]] =
      Right(
        head(plugin, key, label)
          .filter((_, r) => reads(r))
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )

    def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
      Right(
        holding(plugin)
          .filter((_, r) => reads(r))
          .sortBy((v, r) => (r.written, v))
          .reverse
          .take(n max 0)
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )
  }

  /** `plugin`'s current version under `key` at `label`, holding something or a withdrawal. */
  private def head(plugin: PluginName, key: DocKey, label: Label): Option[(Long, Row)] =
    rows.find((_, r) =>
      r.plugin == plugin && r.key == key && r.label == label && r.superseded.isEmpty
    )

  /** Whether the transaction reads `r`, kept in its room at its label. */
  private def reads(r: Row)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.label)

  /** `plugin`'s current versions holding something. */
  private def holding(plugin: PluginName): Vector[(Long, Row)] =
    rows.toVector.filter((_, r) => r.plugin == plugin && r.superseded.isEmpty && r.text.nonEmpty)

  private def insert(row: Row): Long = {
    last += 1
    rows = rows.updated(last, row)
    last
  }

  private def version(v: Long): DocumentVersion =
    DocumentVersion.of(v).getOrElse(throw new IllegalStateException(s"version $v"))

  private def document(v: Long, r: Row, text: DocText): Document =
    Document(version(v), r.plugin, r.key, r.label, r.place, text, r.data, r.written, r.placement)
}

object InMemoryDocuments {

  /** One version's row: `label` and `room` as it is kept; `text` `None` for a withdrawal. */
  private final case class Row(
      plugin: PluginName,
      key: DocKey,
      label: Label,
      room: Option[Place],
      place: Place,
      text: Option[DocText],
      data: ujson.Value,
      written: Instant,
      superseded: Option[Instant],
      placement: Placement
  )

  /** `text`'s words, lower case. */
  private def words(text: String): Vector[String] =
    text.toLowerCase.split("[^\\p{L}\\p{N}]+").toVector.filter(_.nonEmpty)
}
