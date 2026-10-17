package grit.knowledge.drive

import grit.core.tool.Outcome

/** A Drive listing as a reader kept it: the entries it kept, in order, and whether the listing
  * is all Drive holds there (`complete`: Drive had no more, and grit's tools cut none of it for
  * size).
  */
final case class DriveListing(entries: Vector[DriveEntry], complete: Boolean) extends caps.Pure

object DriveListing {

  /** `stated`, a listing's facts as Drive's tools write them (their form pinned by the goldens
    * under `test/resources/drive/facts`), or why not: the listing's own facts, or a part's,
    * missing a key or giving it in another form, the first such named with its key.
    */
  def read(stated: Outcome.Stated): Either[String, DriveListing] =
    for {
      own <- stated.answer.toRight("the listing states no `complete`")
      o <- Fields.obj(own.json, "the listing")
      complete <- Fields.bool(o, "complete", "the listing")
      entries <- stated.parts.zipWithIndex.foldLeft[Either[String, Vector[DriveEntry]]](
        Right(Vector.empty)
      ) { case (got, (part, i)) =>
        got.flatMap(es => DriveEntry.read(part.json, i + 1).map(es :+ _))
      }
    } yield DriveListing(entries, complete)
}
