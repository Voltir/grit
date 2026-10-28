package grit.eval.harness.reply

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.context.{AssemblyError, AssemblyRequest, Results, Retrieved, Width}
import grit.core.document.Document
import grit.core.id.{DocKey, DocumentVersionId, PluginName, TurnRef, WorkflowId}
import grit.core.place.Place
import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.internal.Reader
import grit.eval.harness.stats.Proportion
import grit.turn.TurnRecord

/** What kept a document, to split recall by. */
enum KeptAt {
  case Service(service: grit.core.place.Service)
  case Namespace(namespace: grit.core.place.Namespace)
  case Everywhere

  /** The service's name, else the namespace's key, else `everywhere`. */
  def written: String = this match {
    case Service(s) => s.name
    case Namespace(ns) => ns.key
    case Everywhere => "everywhere"
  }
}

object KeptAt {

  /** The service `place` is ([[Place.service]]), else the namespace it lies under, else
    * `Everywhere`.
    */
  def of(place: Place): KeptAt = place.service match {
    case Some(s) => Service(s)
    case None =>
      place.segments.headOption.flatMap(grit.core.place.Namespace.of).fold(Everywhere)(Namespace(_))
  }

  given Ordering[KeptAt] = Ordering.by(_.written)
}

/** The documents recorded turns followed, found again: each turn's recorded query searched
  * again by the shipped assembler over a restored database, and where each document it followed
  * ranks now.
  */
object Refound {

  /** Every current document's version by its plugin and key, as [[Reader.current]] reads them
    * all: what a followed version is looked up in, never a search's hits, which hold only what
    * was ranked.
    */
  final case class Current private (byKey: Map[(PluginName, DocKey), DocumentVersionId])

  object Current {
    def of(documents: Vector[Document]): Current =
      Current(documents.map(d => (d.plugin, d.key) -> d.version).toMap)
  }

  /** Where the search ranks a kept followed document now: `At` its position among all the
    * search's ranked results, 1 the best (entries, closings and documents together, as the
    * turn's judge took them); `Missed` when its document's current version is not among them;
    * `Gone` when its document has no current version.
    */
  enum Rank {
    case At(position: Int)
    case Missed
    case Gone
  }

  /** What became of a followed version: `Kept`, with its position among what the turn's own
    * search ranked when it ran (`recorded`, 1 the best; `None` when that search never ranked it,
    * so it reached the window otherwise, such as placed at its room) and where it ranks `now`;
    * `Forgotten` when retention no longer keeps it, so neither its document nor its source can
    * be read.
    */
  enum Found {
    case Kept(source: KeptAt, recorded: Option[Int], now: Rank)
    case Forgotten
  }

  /** A version a turn counted followed, and what became of it. */
  final case class Followed(version: DocumentVersionId, found: Found)

  /** 5, 10 and the judged cut ([[Admission.Shipped]]'s `judged`): none ranked past it is ever
    * packed.
    */
  val Cuts: Vector[Int] = Vector(5, 10, Admission.Shipped.judged)

  /** Each of `followed`, in order, found among `kept` (the followed versions still kept, as
    * [[grit.core.document.DocumentSearch.read]] returns them), placed by its own version in
    * `recorded` (what the turn's search ranked when it ran) and ranked in `now` by its
    * document's current version.
    */
  def rank(
      recorded: Results,
      now: Results,
      followed: Vector[DocumentVersionId],
      kept: Vector[Document],
      current: Current
  ): Vector[Followed] = {
    val byVersion = kept.map(d => d.version -> d).toMap
    val was = positions(recorded)
    val ranked = positions(now)
    followed.map(v =>
      Followed(
        v,
        byVersion.get(v) match {
          case None => Found.Forgotten
          case Some(d) =>
            Found.Kept(
              KeptAt.of(d.place),
              was.get(v),
              current.byKey.get((d.plugin, d.key)) match {
                case None => Rank.Gone
                case Some(c) => ranked.get(c).fold(Rank.Missed)(Rank.At(_))
              }
            )
        }
      )
    )
  }

  /** Reads of every row, whoever they are for ([[Reader.all]]). */
  private def every(reader: Reader^): Db^ = new Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      reader.all.read(body)
  }

  /** Each document version `results` ranked, by its first position, 1 the best. */
  private def positions(results: Results): Map[DocumentVersionId, Int] =
    results.ranked.zipWithIndex
      .collect { case ((Retrieved.Doc(v), _), i) => v -> (i + 1) }
      .reverse
      .toMap

  /** The versions the turn `workflow` counted followed ([[TurnRecord.followed]]), each ranked
    * ([[rank]]) in what the shipped assembler built as `assembled` finds for the query it
    * recorded ([[TurnRecord.retrieval]]), searched again over `reader`'s database as it stood at
    * `at`, documents included ([[AsOf.documentsAtEnd]]): no query written, no classifier asked.
    * The search reads every row, whatever the turn was cleared for, since a reader records no
    * deployment's groups and so cannot read as the turn did: a document hidden from the turn can
    * rank above one it followed. `None` when the turn recorded no search or followed nothing;
    * `Left` naming what failed, never a text.
    */
  def of(
      reader: Reader^,
      workflow: WorkflowId,
      assembled: Assembled,
      at: Instant,
      current: Current
  ): Either[String, Option[Vector[Followed]]] = {
    val name = WorkflowId.value(workflow)
    for {
      turn <- TurnRef.fromWorkflowId(workflow).toRight(s"$name is not a turn")
      steps <- reader.steps(workflow).left.map(e => s"steps of $name unread: ${e.kind}")
      searched <- TurnRecord
        .retrieval(steps)
        .left
        .map(why => s"the search of $name does not read: $why")
      followed <- TurnRecord
        .followed(steps)
        .left
        .map(why => s"the follows of $name do not read: $why")
      found <- (searched, followed.filter(_.nonEmpty)) match {
        case (Some(r), Some(versions)) =>
          for {
            kept <- reader.all
              .read(reader.documents.read(versions.distinct))
              .left
              .map((e: StoreError) => s"the documents $name followed unread: ${e.kind}")
            // `shipped` reads through an `AsOf`; at the dump it hides nothing, as a restored
            // capture is never written after it. Its documents are read at the dump, not at the
            // turn's root, where the shipped assembler draws them: the index as it is now.
            results <- Rebuild
              .shipped(AsOf(reader, at).documentsAtEnd, assembled)
              .search(AssemblyRequest(turn, Width.Deployed), r.written)(using every(reader))
              .left
              .map { case AssemblyError.Store(e) =>
                s"the search of $name was not run again: ${e.kind}"
              }
          } yield Some(rank(r.results, results, versions, kept, current))
        case _ => Right(None)
      }
    } yield found
  }

  /** Over the kept followed documents that have a current version, the share ranked now at or
    * above each cut, the share ranked now at all, and the share the turn's own search ranked
    * when it ran (`recorded`; the rest reached the window otherwise), clustered by turn.
    */
  final case class At(cuts: VectorMap[Int, Proportion], ranked: Proportion, recorded: Proportion)

  /** [[At]] over all the kept followed documents that have a current version, and by source (in
    * `written` order); `gone` and `forgotten` counted apart, in no share; `turns` the turns
    * summed.
    */
  final case class Summary(
      all: At,
      bySource: VectorMap[KeptAt, At],
      gone: Int,
      forgotten: Int,
      turns: Int
  )

  /** A kept followed document with a current version, as [[summary]] counts it. */
  private final case class Item(turn: WorkflowId, source: KeptAt, recorded: Option[Int], now: Rank)

  /** The summary of each turn's followed versions. */
  def summary(turns: Vector[(WorkflowId, Vector[Followed])]): Summary = {
    val ranked = turns.flatMap((w: WorkflowId, fs: Vector[Followed]) =>
      fs.collect { case Followed(_, Found.Kept(s, was, r @ (Rank.At(_) | Rank.Missed))) =>
        Item(w, s, was, r)
      }
    )
    def at(items: Vector[Item]): At =
      At(
        VectorMap.from(
          Cuts.map(cut =>
            cut -> Proportion.of(
              items.map(i =>
                i.turn -> (i.now match {
                  case Rank.At(p) => p <= cut
                  case _ => false
                })
              )
            )
          )
        ),
        Proportion.of(items.map(i => i.turn -> (i.now != Rank.Missed))),
        Proportion.of(items.map(i => i.turn -> i.recorded.isDefined))
      )
    val all = turns.flatMap(_._2)
    Summary(
      at(ranked),
      VectorMap.from(
        ranked.map(_.source).distinct.sorted.map(s => s -> at(ranked.filter(_.source == s)))
      ),
      all.count {
        case Followed(_, Found.Kept(_, _, Rank.Gone)) => true
        case _ => false
      },
      all.count(_.found == Found.Forgotten),
      turns.size
    )
  }
}
