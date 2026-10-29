package grit.core.claim

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{ClaimHolder, DocKey}
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A claim: `holder` works its plugin's `subject` as `role`, first made at `made` by `by` (with
  * the accounts linked to them when it is read), kept at `label`. `active` is the latest of
  * `made` and the last event of any session working as `holder` that the reading transaction
  * reads.
  */
final case class Claim(
    subject: DocKey,
    holder: ClaimHolder,
    role: ClaimRole,
    by: Principal.Person,
    made: Instant,
    label: Label,
    active: Instant
) {

  /** [[Claims.Lease]] after `active`: from then on it is lapsed, holding neither its subject nor
    * its holder.
    */
  def lapses: Instant = active.plusMillis(Claims.Lease.toMillis)
}

/** What [[Claims.claim]] came to. Conflicts are with live claims only: when the label holds both
  * a claim on the subject and one by the holder, it is [[Taken]].
  */
enum Claimed {

  /** Made, and now live; or a claim the holder already had on the subject, renewed. */
  case Made(claim: Claim)

  /** Refused, making nothing: the label holds a claim on the subject by another holder already;
    * it, when this transaction reads it.
    */
  case Taken(first: Option[Claim])

  /** Refused, making nothing: the label holds a claim by the holder on another subject already;
    * it, when this transaction reads it.
    */
  case Holding(other: Option[Claim])
}

/** One plugin's claims, as its transaction reads them: at each label, one live claim per subject
  * and one per holder. A claim's lease is its holder's: any session working as that holder (a
  * directory's slot, kept by every session started in that directory) keeps it from lapsing
  * ([[Claim.lapses]]). A release deletes it, and so does a claim taking it over once lapsed; the
  * session sweep deletes a claim lapsed longer than [[grit.core.session.Session.Unended]]. Nothing
  * is versioned.
  */
trait Claims extends caps.Pure {

  /** Claims `subject` (a key; the store never checks that it holds a document) for `holder` as
    * `role`, by `by` at `at`, kept at `label` joined with the transaction's floor and in its own
    * room, as [[grit.core.document.DocumentKeeper.write]] keeps a document. A claim at that label
    * on `subject` or by `holder` that has lapsed at `at` is deleted first; a session this
    * transaction does not read keeps no claim from lapsing. A claim `holder` already has on
    * `subject` there, when this transaction reads it, is renewed: its role becomes `role`, it
    * keeps its `made` and `by`, and it is [[Claimed.Made]].
    */
  def claim(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      by: Principal.Person,
      label: Label,
      at: Instant
  )(using Tx^): Either[StoreError, Claimed]

  /** Deletes the claim on `subject` kept at exactly `label`, lapsed or not, when this transaction
    * reads it: it, or `None` when there is none it reads.
    */
  def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]]

  /** This plugin's claims at every label this transaction reads that have not lapsed at `at`,
    * by subject, then by when they were made. A session this transaction does not read keeps no
    * claim from lapsing.
    */
  def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]]
}

object Claims {

  /** 2 hours: how long after its holder last worked a claim lapses ([[Claim.lapses]]). */
  val Lease: FiniteDuration = 2.hours
}
