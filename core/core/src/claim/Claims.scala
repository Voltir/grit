package grit.core.claim

import java.time.Instant

import grit.core.id.DocKey
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A live claim: `holder` works its plugin's `subject` as `role`, made at `made` by `by` (with
  * the accounts linked to them when it is read), kept at `label`.
  */
final case class Claim(
    subject: DocKey,
    holder: ClaimHolder,
    role: ClaimRole,
    by: Principal.Person,
    made: Instant,
    label: Label
)

/** What [[Claims.claim]] came to. When the label holds both a claim on the subject and one by
  * the holder, it is [[Taken]].
  */
enum Claimed {

  /** Made, and now live. */
  case Made(claim: Claim)

  /** Refused, making nothing: the label holds a claim on the subject already; it, when this
    * transaction reads it.
    */
  case Taken(first: Option[Claim])

  /** Refused, making nothing: the label holds a claim by the holder already; it, when this
    * transaction reads it.
    */
  case Holding(other: Option[Claim])
}

/** One plugin's live claims, as its transaction reads them: at each label, one per subject and
  * one per holder. A release deletes it; nothing is versioned.
  */
trait Claims extends caps.Pure {

  /** Claims `subject` (a key; the store never checks that it holds a document) for `holder` as
    * `role`, by `by` at `at`, kept at `label` joined with the transaction's floor and in its own
    * room, as [[grit.core.document.DocumentKeeper.write]] keeps a document.
    */
  def claim(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      by: Principal.Person,
      label: Label,
      at: Instant
  )(using Tx^): Either[StoreError, Claimed]

  /** Deletes the claim on `subject` kept at exactly `label`, when this transaction reads it: it,
    * or `None` when there is none it reads.
    */
  def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]]
}
