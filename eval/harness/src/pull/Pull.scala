package grit.eval.harness.pull

import java.time.Instant

import grit.core.review.Considered
import grit.core.store.{StoreError, Tx}
import grit.dbos.internal.Reader
import grit.eval.harness.capture.CaseId
import grit.eval.harness.label.{Rated, Verdicts}
import grit.prose.either.Traversal.traverse

/** What a deployment's database kept of its reviews: the verdicts standing, by case; text-free.
  */
object Pull {

  /** What [[verdicts]] read: the verdicts standing, by case; and how many stand on a message
    * no case id names, one not heard in Slack.
    */
  final case class Standing(verdicts: Verdicts, unnamed: Int)

  /** The verdicts standing on the messages a review considered at or after `since`, each by
    * its case, rebuilt from its message's conversation ([[CaseId.of]]); one a case, the latest
    * given when two messages share one. `Left` when the database cannot be read, naming what
    * was being read.
    */
  def verdicts(reader: Reader^, since: Instant): Either[String, Standing] = {
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]): Either[String, A] =
      reader.all.read(body).left.map(e => s"$what unread: ${e.getClass.getSimpleName}")
    for {
      reviewed <- read("reviews")(reader.reviews.reviewed(since))
      rated = reviewed.flatMap(r =>
        (r.as, r.label) match {
          case (Considered.Picked(reason), Some(l)) =>
            Some(r -> Rated(r.shadow, reason, l.verdict, l.rater, l.at))
          case _ => None
        }
      )
      named <- traverse(rated)((r, rated) =>
        read("conversation")(reader.conversations.get(r.conversation))
          .map(c => (c.flatMap(c => CaseId.of(c.origin, r.entry)), rated))
      )
    } yield {
      val byCase = named.collect { case (Some(id), rated) => id -> rated }
      Standing(
        Verdicts(byCase.sortBy(_._2.at).toMap),
        named.count(_._1.isEmpty)
      )
    }
  }
}
