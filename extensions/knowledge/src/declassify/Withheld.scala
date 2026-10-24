package grit.knowledge.declassify

import java.time.Instant

import grit.core.document.{Declassified, DocText, DocumentKeeper}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** What a label's declassify runs withheld: each card version, under the policy and application
  * versions it was asked at, whose rewrite the check kept back or the model wrote nothing of, so
  * no later run asks of it again at those versions. One document per label, at its floor, under
  * [[Key]], holding no text a search matches and no rewrite's data
  * ([[Declassified.Made.read]] reads none from it).
  */
object Withheld {

  /** `declassify:withheld`. */
  private[declassify] val Key: DocKey =
    // Lower case letters and one `:`: DocKey.of never refuses them.
    DocKey.of("declassify:withheld").fold(e => throw new IllegalStateException(e), identity)

  /** A document is never blank; this one, read only by its data, holds no word. */
  private val Text: DocText =
    DocText.of("…").fold(e => throw new IllegalStateException(e), identity)

  /** What is withheld at `floor`: none when there is no such document, or its data does not
    * read.
    */
  def read(keeper: DocumentKeeper, floor: Label)(using
      Tx^
  ): Either[StoreError, Vector[Declassified.Made]] =
    keeper
      .current(Key, floor)
      .map(
        _.flatMap(_.data.objOpt.flatMap(_.get("withheld")).flatMap(_.arrOpt))
          .fold(Vector.empty)(_.toVector.flatMap(Declassified.Made.read))
      )

  /** `made` joined to what is withheld at its transaction's floor ([[Tx.floor]]), as [[kept]]. */
  private[declassify] def joined(keeper: DocumentKeeper, made: Declassified.Made, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Kept.type] = {
    val floor = Tx.floor(tx)
    read(keeper, floor).flatMap(held => kept(keeper, floor, held.filterNot(_ == made) :+ made, at))
  }

  /** `withheld` as what is withheld at `floor`, written `at`, in no one room: withdrawn when it
    * holds nothing.
    */
  private[declassify] def kept(
      keeper: DocumentKeeper,
      floor: Label,
      withheld: Vector[Declassified.Made],
      at: Instant
  )(using Tx^): Either[StoreError, Kept.type] =
    if (withheld.isEmpty) keeper.withdraw(Key, floor, at).map(_ => Kept)
    else
      keeper
        .write(
          Key,
          floor,
          Integrity.Authority,
          Place.Everywhere,
          Text,
          ujson.Obj("withheld" -> ujson.Arr.from(withheld.map(_.written))),
          None,
          at
        )
        .map(_ => Kept)
}
