package grit.eval.harness.pull

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, EntryId, PrincipalId, ShadowName, SourceId}
import grit.core.inbox.InboundId
import grit.core.review.{Candidate, Considered, Reason, Settled, Verdict}
import grit.core.speech.Outcome
import grit.core.store.{Origin, StoreError}
import grit.core.visibility.Visibility
import grit.dbos.engine.{LiveEngine, Reader}
import grit.dbos.sql.{LiveDb, SqlReviews, TestPostgres}
import grit.eval.harness.capture.CaseId
import grit.eval.harness.label.{Rated, Verdicts}

import utest.*

/** The verdicts a database's reviews keep, pulled by case. Every message is synthetic; none
  * has an entry, as a review's row outlives its message's.
  */
object PullVerdictsTests extends TestSuite {

  private val At = Instant.parse("2026-10-03T09:00:00Z")
  private val shadow = ShadowName.of("triage-v2").fold(sys.error, identity)
  private val rater = PrincipalId("slack:T1/U1")
  private val reviews = new SqlReviews

  private def right[A](e: Either[StoreError, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  val tests = Tests {
    test(
      "pull keeps the verdict standing on each picked message considered since, by its Slack case, and counts those no case names"
    ) {
      val config = TestPostgres.freshDatabase("harness_verdicts")
      LiveEngine.open(config, "test").close()
      val slack = LiveDb.conversation(config, Origin.Slack("T1", "C1", "100.0")).id
      val task = LiveDb.conversation(config, Origin.Task("t", "r")).id

      /** Message `ts` of `c`, considered as `as` at `considered`; posted at `ts`'s address when
        * picked, then each of `verdicts` given in turn.
        */
      def review(
          c: ConversationId,
          ts: String,
          as: Considered,
          considered: Instant,
          verdicts: Vector[Verdict]
      ): EntryId = {
        val entry = InboundId.of(c, SourceId(ts))
        LiveDb.transaction(config) {
          right(
            reviews.considered(
              Candidate(
                entry,
                c,
                considered,
                Settled.Drafted(Outcome.Passed),
                shadow,
                VectorMap.empty
              ),
              as,
              considered
            )
          )
          as match {
            case Considered.Picked(_) =>
              right(reviews.posted(entry, s"review/$ts", considered))
              verdicts.zipWithIndex.foreach((v, i) =>
                right(reviews.reacted(s"review/$ts", rater, v, considered.plusSeconds(i + 1L)))
              )
            case _ => ()
          }
        }
        entry
      }

      val _ = Vector(
        review(slack, "101.0", Considered.Picked(Reason.ShadowOnly), At, Vector(Verdict.Welcome)),
        // A later reaction replaces the one standing.
        review(
          slack,
          "102.0",
          Considered.Picked(Reason.Both),
          At,
          Vector(Verdict.Welcome, Verdict.CutIn)
        ),
        review(slack, "103.0", Considered.Picked(Reason.LiveOnly), At, Vector.empty),
        review(slack, "104.0", Considered.Passed(Reason.Neither), At, Vector.empty),
        review(
          slack,
          "105.0",
          Considered.Picked(Reason.Neither),
          At.minusSeconds(60),
          Vector(Verdict.Interruption)
        ),
        review(task, "106.0", Considered.Picked(Reason.Neither), At, Vector(Verdict.Interruption))
      )
      val reader = Reader.open(config, Visibility.Shipped)
      try {
        Pull.verdicts(reader, At) ==> Right(
          Pull.Standing(
            Verdicts(
              Map(
                id("C1/101.0") -> Rated(
                  shadow,
                  Reason.ShadowOnly,
                  Verdict.Welcome,
                  rater,
                  At.plusSeconds(1)
                ),
                id("C1/102.0") -> Rated(
                  shadow,
                  Reason.Both,
                  Verdict.CutIn,
                  rater,
                  At.plusSeconds(2)
                )
              )
            ),
            1
          )
        )
      } finally reader.close()
    }
  }
}
