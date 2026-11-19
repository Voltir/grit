package grit.knowledge.declassify

import grit.core.document.{Declassified, Document}
import grit.core.id.DocKey
import grit.knowledge.card.FloorList

/** What a label's declassify runs withheld: each card version, under the policy and application
  * versions it was asked at, whose rewrite the check kept back or the model wrote nothing of, so
  * no later run asks of it again at those versions. Kept under `declassify:withheld`, holding no
  * rewrite's data ([[Declassified.Made.read]] reads none from it); an entry that does not read,
  * such as one with no revision, is left out.
  */
private[declassify] object Withheld
    extends FloorList[Declassified.Made](DocKey.literal("declassify:withheld")) {

  protected def encode(all: Vector[Declassified.Made]): ujson.Value =
    ujson.Obj("withheld" -> ujson.Arr.from(all.map(_.written)))

  protected def decode(d: Document): Vector[Declassified.Made] =
    d.data.objOpt
      .flatMap(_.get("withheld"))
      .flatMap(_.arrOpt)
      .fold(Vector.empty)(_.toVector.flatMap(Declassified.Made.read))
}
