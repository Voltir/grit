package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{InMemoryTombstones, StoreError, Tx}

/** An in-memory [[DocumentStore]], with each plugin's [[DocumentKeeper]], for tests, keeping
  * [[DocumentContract]]. Versions are numbered by one counter across every plugin. It marks
  * versions in `tombstones`, which reads which plugin's a version is from it. Search scores a
  * document by how often the query's words occur in it, not by BM25: the contract's searches
  * order strictly under either. It ignores the `Tx`: nothing is rolled back.
  */
final class InMemoryDocuments extends DocumentStore {
  import InMemoryDocuments.Row

  // Only ever replaced by a new immutable map, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[Long, Row]

  // The identity column's last value, one across every plugin: only ever counted up.
  @caps.unsafe.untrackedCaptures
  private var last = 0L

  // As `rows`: only ever replaced by a new immutable map.
  @caps.unsafe.untrackedCaptures
  private var terms = Map.empty[PluginName, DocumentTerms]

  /** Where its keepers mark the versions they leave for deletion. */
  val tombstones: InMemoryTombstones =
    new InMemoryTombstones(v => rows.get(DocumentVersion.value(v)).map(_.plugin))

  /** `plugin`'s documents as it writes them, under `under`. */
  def keeper(plugin: PluginName, under: DocumentTerms): DocumentKeeper =
    new Shelf(plugin) with DocumentKeeper {
      def write(key: DocKey, place: Place, text: DocText, data: ujson.Value, at: Instant)(using
          Tx^
      ): Either[StoreError, Written] =
        head(plugin, key) match {
          case Some((v, r)) if r.text.contains(text) && r.place == place && r.data == data =>
            Right(Written.Unchanged(version(v)))
          case before =>
            for {
              _ <- before.fold(Right(()))((v, r) => supersede(v, r, at))
              v = insert(Row(plugin, key, place, Some(text), data, at, None, Placement(0, at)))
              over = holding(plugin).filterNot((_, r) => r.key == key)
              excess = (over.size + 1 - under.bound) max 0
              gone = over
                .sortBy((_, r) => (r.placement.lastPlaced, DocKey.value(r.key)))
                .take(excess)
                .map(_._2.key)
              _ <- gone.foldLeft[Either[StoreError, Unit]](Right(()))((acc, k) =>
                acc.flatMap(_ => withdraw(k, at).map(_ => ()))
              )
            } yield Written.Versioned(version(v), gone)
        }

      def withdraw(key: DocKey, at: Instant)(using
          Tx^
      ): Either[StoreError, Option[DocumentVersion]] =
        head(plugin, key) match {
          case Some((v, r)) if r.text.nonEmpty =>
            for {
              _ <- supersede(v, r, at)
              w = insert(Row(plugin, key, r.place, None, ujson.Obj(), at, None, Placement(0, at)))
              _ <- tombstones.write(Target.Document(version(w)), at)
            } yield Some(version(w))
          case _ => Right(None)
        }
    }

  /** `plugin`'s documents as it reads them. */
  def shelf(plugin: PluginName): DocumentShelf = new Shelf(plugin)

  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
    Right(terms.toVector)

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
        .filter(r => plugins.contains(r.plugin) && currentAt(r, at))
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
            case (v, r @ Row(plugin, _, place, Some(text), _, _, _, _))
                if shelves.contains(Shelved(plugin, place)) && currentAt(r, at) =>
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
        .flatMap(r => r.text.map(document(DocumentVersion.value(v), r, _)))
    })

  def declare(enabled: Vector[(PluginName, DocumentTerms)])(using Tx^): Either[StoreError, Unit] = {
    terms = enabled.toMap
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
    Right(rows.values.map(_.plugin).toVector.distinct)

  private class Shelf(plugin: PluginName) extends DocumentShelf {
    def current(key: DocKey)(using Tx^): Either[StoreError, Option[Document]] =
      Right(head(plugin, key).flatMap((v, r) => r.text.map(document(v, r, _))))

    def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
      Right(
        holding(plugin)
          .sortBy((v, r) => (r.written, v))
          .reverse
          .take(n max 0)
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )
  }

  /** `plugin`'s current version under `key`, holding something or a withdrawal. */
  private def head(plugin: PluginName, key: DocKey): Option[(Long, Row)] =
    rows.find((_, r) => r.plugin == plugin && r.key == key && r.superseded.isEmpty)

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
    Document(version(v), r.plugin, r.key, r.place, text, r.data, r.written, r.placement)
}

object InMemoryDocuments {

  /** One version's row: `text` `None` for a withdrawal. */
  private final case class Row(
      plugin: PluginName,
      key: DocKey,
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
