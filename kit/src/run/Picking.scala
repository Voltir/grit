package grit.kit.run

import java.time.Instant

import grit.core.review.{Considered, Review, ReviewStore, Reviewed}
import grit.core.spend.Budget
import grit.core.store.{Jot, StoreError}
import grit.kit.deployment.ShadowReview

/** A deployment's review, picked a round at a time ([[grit.core.review.Review.pick]]). */
private[run] object Picking {

  /** The most candidates a round reads: 200. A larger backlog is read, earliest said first,
    * over the rounds after it.
    */
  val Candidates: Int = 200

  /** A round of `review` at `now`, in one transaction of `jot`: the messages said at or after
    * `now` less `review`'s `within` that `reviews` offers as candidates, at most [[Candidates]],
    * each kept as considered as [[grit.core.review.Review.pick]] makes it with `review`'s gate,
    * the day's earlier picks being those since `now`'s midnight in `budget`'s zone. Returns how
    * many it picked, or the store's failure, keeping nothing; a message considered already is
    * never considered again, so a round rerun picks nothing new.
    */
  def round(
      review: ShadowReview,
      reviews: ReviewStore,
      jot: Jot,
      budget: Budget,
      now: Instant
  ): Either[StoreError, Int] = {
    val reviewing = review.reviewing
    val since = now.minusNanos(reviewing.within.toNanos)
    jot.write {
      for {
        found <- reviews.candidates(reviewing.shadow, since, Candidates)
        today <- reviews.reviewed(budget.today(now).from)
        earlier = today.collect { case Reviewed(_, _, _, _, Considered.Picked(r), _) => r }
        made = Review.pick(found, review.gate.drafts, earlier, reviewing)
        kept <- made.foldLeft[Either[StoreError, Int]](Right(0)) { case (acc, (c, as)) =>
          acc.flatMap(n =>
            reviews.considered(c, as, now).map { fresh =>
              as match {
                case Considered.Picked(_) if fresh => n + 1
                case _ => n
              }
            }
          )
        }
      } yield kept
    }
  }
}
