package grit.slack.edge

import java.time.Instant

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.review.{Prompt, Reviews, Verdict}
import grit.core.store.{StoreError, Tx}
import grit.slack.event.TeamId

import utest.*

/** Triage's review as the Slack edge prompts for it: the reactions it is answered with. What
  * its prompt shows is pinned where it is posted (`ReviewingTests`).
  */
object TriageTests extends TestSuite {

  /** Reviews no test here reaches: reading a reaction asks no store. */
  private object Untouched extends Reviews[Prompt, Verdict] {
    def unposted(to: Place)(using Tx^): Either[StoreError, Vector[Prompt]] = Right(Vector.empty)
    def posted(subject: Prompt, address: String, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(false)
    def dropped(subject: Prompt, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      Right(false)
    def reacted(address: String, rater: Account, verdict: Verdict, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(false)
    def unreacted(address: String, rater: Account, verdict: Verdict)(using
        Tx^
    ): Either[StoreError, Boolean] = Right(false)
  }

  val tests = Tests {
    test(
      "the three reactions give their verdicts in the order they are added, a skin tone ignored; any other emoji gives none"
    ) {
      val verdicts = SlackEdge.triage(Untouched, TeamId("T1")).verdicts
      (
        verdicts.keys.toVector,
        Vector("+1", "-1", "bust_in_silhouette", "+1::skin-tone-4", "eyes", "thumbsup", "+1:", "")
          .map(e => verdicts.get(Prompting.named(e)))
      ) ==> (
        Vector("+1", "-1", "bust_in_silhouette"),
        Vector(
          Some(Verdict.Welcome),
          Some(Verdict.Interruption),
          Some(Verdict.CutIn),
          Some(Verdict.Welcome),
          None,
          None,
          None,
          None
        )
      )
    }
  }
}
