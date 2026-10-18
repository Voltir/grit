package grit.eval.harness.lab

import grit.core.id.ConversationId
import grit.core.message.Usage
import grit.core.stitch.{Placed, StitchJson}
import grit.core.store.PayloadJson

/** The placement a room's truth gives an opening, kept in place of Jev's when the lab places
  * against the truth's strands ([[grit.eval.harness.score.Against.Truth]]).
  */
object Told {

  /** For an opening Jev placed as `jev`: following `root`, the conversation of its topic's
    * first opening, at 1, by the model `truth`, asking nothing, with what Jev was shown; `None`,
    * keeping nothing and so linking it to no strand, when `root` is `None` (it begins).
    * `Left` when the stored form of a placement does not read back.
    */
  def placement(root: Option[ConversationId], jev: Placed): Either[String, Option[Placed]] =
    root match {
      case None => Right(None)
      case Some(r) =>
        StitchJson
          .read(
            ujson.Obj(
              "kind" -> "follows",
              "root" -> ConversationId.value(r),
              "p" -> 1.0,
              "model" -> "truth",
              "usage" -> PayloadJson.writeUsage(Usage.Zero),
              "seen" -> StitchJson.write(jev)("seen")
            )
          )
          .map(Some(_))
    }
}
