package grit.core.review

import java.time.{Instant, LocalDate}

import grit.core.document.Document
import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.identity.Account
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A verdict word a plugin's review keeps: lower-case words joined by `-`. */
final case class ReviewWord private (word: String) extends caps.Pure

object ReviewWord {

  /** `word` as a verdict word, or why not. */
  def of(word: String): Either[String, ReviewWord] =
    Either.cond(
      Word.matches(word),
      new ReviewWord(word),
      s"a verdict word is lower-case words joined by '-': $word"
    )

  private val Word = "[a-z]+(-[a-z]+)*".r
}

/** A document put up for review: its plugin's `key` at `label` on `day`, and the version put
  * up, read as its transaction reads it.
  */
final case class DocumentReview(
    plugin: PluginName,
    key: DocKey,
    label: Label,
    day: LocalDate,
    document: Document
) extends caps.Pure

/** A label given on a document's review: the review's key, label and day, the version it was
  * given on while it is kept, the verdict, its rater and when.
  */
final case class DocumentLabel(
    key: DocKey,
    label: Label,
    day: LocalDate,
    version: Option[DocumentVersion],
    verdict: ReviewWord,
    rater: Account,
    at: Instant
) extends caps.Pure

/** The reviews of documents put up ([[grit.core.document.DocumentKeeper.forReview]]), one
  * plugin's at a time.
  */
trait DocumentReviews {

  /** `plugin`'s reviews: a subject waits from its put-up until posted or dropped, while its
    * version is kept; one is named by its key, label and day.
    */
  def of(plugin: PluginName): Reviews[DocumentReview, ReviewWord]

  /** Every label `plugin`'s reviews were given on days from `from` at a label this transaction
    * reads, as a document kept in no room, earliest day first, then earliest given (ties by
    * key).
    */
  def labels(plugin: PluginName, from: LocalDate)(using
      Tx^
  ): Either[StoreError, Vector[DocumentLabel]]

  /** Deletes every review put up before `before`, posted, labelled or not; how many. The
    * collector calls it each sweep with now less the ledger window, beside
    * [[grit.core.store.Tombstones.forget]].
    */
  def forget(before: Instant)(using Tx^): Either[StoreError, Int]
}
