package grit.knowledge.topic

import java.time.Instant

import grit.core.document.{DocText, DocumentKeeper, Written}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.topic.TopicWrite.Kept

/** What a label's topics runs set aside: each strand and card their attach ask placed in no
  * topic, at the digest or version it was shown at, so no later run asks of it again until it
  * changes. One document per label, under [[Key]], which is no topic's ([[TopicKey.read]]).
  */
private[topic] object Aside {

  /** `topic:aside`. */
  val Key: DocKey =
    // Lower case letters and one `:`: DocKey.of never refuses them.
    DocKey.of("topic:aside").fold(e => throw new IllegalStateException(e), identity)

  /** What the aside's document says: a document is never blank, and this one, read only by its
    * data, holds no word, so no search matches it.
    */
  private val Text: DocText =
    DocText.of("\u2026").fold(e => throw new IllegalStateException(e), identity)

  /** The aside at `floor`: empty when there is none, or its data does not read. */
  def read(keeper: DocumentKeeper, floor: Label)(using Tx^): Either[StoreError, Lineage] =
    keeper
      .current(Key, floor)
      .map(_.flatMap(d => TopicJson.readAside(d.data).toOption).getOrElse(Lineage.of(Vector.empty)))

  /** `tags` joined to the aside at its transaction's floor ([[Tx.floor]]), as [[kept]]. */
  def joined(keeper: DocumentKeeper, tags: Vector[Lineage.Tag], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Kept] = {
    val floor = Tx.floor(tx)
    read(keeper, floor).flatMap(held => kept(keeper, floor, Lineage.of(held.tags ++ tags), at))
  }

  /** `aside` as the aside at `floor`, written `at`, not in any one room ([[Place.Everywhere]]):
    * withdrawn when it holds nothing; `Kept(None, 0)` when nothing was written, it being so
    * already.
    */
  def kept(keeper: DocumentKeeper, floor: Label, aside: Lineage, at: Instant)(using
      Tx^
  ): Either[StoreError, Kept] =
    if (aside.tags.isEmpty)
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
