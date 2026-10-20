package grit.knowledge.topic

import java.time.Instant

import grit.core.document.{DocText, DocumentKeeper, Written}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.topic.TopicWrite.Kept

/** What a label's topics runs set aside: each strand and card their attach ask placed in no
  * topic, at the digest or version it was shown at, and when, so no later run asks of it again
  * until it changes or its run's [[TopicParams.asideFor]] has passed. One document per label,
  * under [[Key]], which is no topic's ([[TopicKey.read]]).
  */
object Aside {

  /** One strand or card set aside (`tag`), and when (`at`). */
  final case class Entry(tag: Lineage.Tag, at: Instant) extends caps.Pure

  /** `topic:aside`. */
  private[topic] val Key: DocKey =
    // Lower case letters and one `:`: DocKey.of never refuses them.
    DocKey.of("topic:aside").fold(e => throw new IllegalStateException(e), identity)

  /** What the aside's document says: a document is never blank, and this one, read only by its
    * data, holds no word, so no search matches it.
    */
  private val Text: DocText =
    DocText.of("…").fold(e => throw new IllegalStateException(e), identity)

  /** The aside at `floor`: empty when there is none, or its data does not read. An entry kept
    * before entries were dated is dated when the aside was last written, the latest it can
    * have been set aside, so it never leaves the aside early.
    */
  def read(keeper: DocumentKeeper, floor: Label)(using Tx^): Either[StoreError, Vector[Entry]] =
    keeper
      .current(Key, floor)
      .map(_.flatMap(d => TopicJson.readAside(d.data, d.written).toOption).getOrElse(Vector.empty))

  /** `tags`, set aside `at`, joined to the aside at its transaction's floor ([[Tx.floor]]), each
    * replacing an entry of its tag, as [[kept]].
    */
  private[topic] def joined(keeper: DocumentKeeper, tags: Vector[Lineage.Tag], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Kept] = {
    val floor = Tx.floor(tx)
    read(keeper, floor).flatMap(held =>
      kept(keeper, floor, held.filterNot(e => tags.contains(e.tag)) ++ tags.map(Entry(_, at)), at)
    )
  }

  /** `aside` as the aside at `floor`, written `at`, not in any one room ([[Place.Everywhere]]):
    * withdrawn when it holds nothing; `Kept(None, 0)` when nothing was written, it being so
    * already.
    */
  private[topic] def kept(keeper: DocumentKeeper, floor: Label, aside: Vector[Entry], at: Instant)(
      using Tx^
  ): Either[StoreError, Kept] =
    if (aside.isEmpty)
      keeper.current(Key, floor).flatMap {
        case None => Right(Kept(None, 0))
        case Some(_) => keeper.withdraw(Key, floor, at).map(Kept(_, 0))
      }
    else
      keeper
        .write(
          Key,
          floor,
          Integrity.Authority,
          Place.Everywhere,
          Text,
          TopicJson.aside(aside),
          None,
          at
        )
        .map {
          case Written.Versioned(v, _, _) => Kept(Some(v), 0)
          case Written.Unchanged(_, _) => Kept(None, 0)
        }
}
