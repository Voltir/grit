package grit.knowledge.card

import java.time.Instant

import grit.core.document.{DocText, Document, DocumentKeeper, Written}
import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A list a run keeps of its own, one document per label under `key`, read and written only by
  * its data. Its text holds no word, so no search matches it.
  */
abstract class FloorList[A <: caps.Pure](val key: DocKey) {

  /** The data that holds `all`. */
  protected def encode(all: Vector[A]): ujson.Value

  /** The entries `d`'s data holds, leaving out any that do not read. */
  protected def decode(d: Document): Vector[A]

  /** The entries kept at `floor`: none when no document is. */
  def read(keeper: DocumentKeeper, floor: Label)(using Tx^): Either[StoreError, Vector[A]] =
    keeper.current(key, floor).map(_.fold(Vector.empty)(decode))

  /** `all` as the list at `floor`, written `at` in no one room ([[Place.Everywhere]]) and
    * believed at no more than the list it replaces ([[DocumentKeeper.replacing]]), or withdrawn
    * when `all` is empty: the version written or withdrawn, `None` when the list was so already.
    * `Left` when the store refuses, as [[DocumentKeeper.write]] does.
    */
  def kept(keeper: DocumentKeeper, floor: Label, all: Vector[A], at: Instant)(using
      Tx^
  ): Either[StoreError, Option[DocumentVersionId]] =
    if (all.isEmpty) keeper.withdraw(key, floor, at)
    else
      keeper
        .current(key, floor)
        .map(DocumentKeeper.replacing)
        .flatMap(
          keeper.write(key, floor, _, Place.Everywhere, FloorList.Text, encode(all), None, at)
        )
        .map {
          case Written.Versioned(v, _, _) => Some(v)
          case Written.Unchanged(_, _) => None
        }

  /** The list at its transaction's floor ([[Tx.floor]]) changed by `change`, as [[kept]]. */
  def joined(keeper: DocumentKeeper, change: Vector[A] -> Vector[A], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Option[DocumentVersionId]] = {
    val floor = Tx.floor(tx)
    read(keeper, floor).flatMap(held => kept(keeper, floor, change(held), at))
  }
}

object FloorList {

  /** A document is never blank; this one holds no word. */
  private val Text: DocText = DocText.literal("…")
}
