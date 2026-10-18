package grit.core.stitch

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Classifier, TestClassifiers}
import grit.core.clock.Clock
import grit.core.route.Routed
import grit.core.store.Db
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

/** [[Placements]] over in-memory stores: each opening placed at once, as its own workflow
  * places it ([[Stitching.placing]], asked under what its read was routed for, kept in `reads`' stitches at `at`), the first time it
  * is waited for, so a bounded wait never runs out; every opening waited for kept in
  * [[waited]].
  */
final class InMemoryPlacements(
    classifier: Classifier^,
    reads: StitchReads,
    db: Db^,
    tuning: Tuning,
    at: Instant
) extends Placements {

  // Read only by the test that owns it.
  @caps.unsafe.untrackedCaptures
  var waited: Vector[Opening] = Vector.empty

  def awaitedWithin(
      opening: Opening,
      within: FiniteDuration,
      clock: Clock^
  ): Either[grit.core.stitch.Placements.Unplaced, String] =
    awaited(opening).left.map(grit.core.stitch.Placements.Unplaced.Failed(_))

  def awaited(opening: Opening): Either[String, String] = {
    val first = !waited.exists(_.ref == opening.ref)
    waited = waited :+ opening
    if (!first) Right("placed already")
    else {
      val cleared = TestClassifiers.cleared(classifier)
      Routed
        .reading(db.as(Subject.Conversation(opening.ref.turn.conversationId)))((routed, bounded) =>
          Stitching.placing(reads, bounded, opening.ref.turn, tuning)(
            (first, placed) => first -> placed,
            (first, q) => first -> q.placed(cleared.asking(routed).ask(q.state, q.choice))
          )
        )
        .flatten match {
        case Left(e) => Left(e.toString)
        case Right(None) => Right("nothing asked")
        case Right(Some((root, placed))) =>
          reads.stitches
            .record(root, placed, at)(using TestTx.fake)
            .left
            .map(_.toString)
            .map(_ => s"stitched: ${StitchJson.kindOf(placed)}")
      }
    }
  }
}
