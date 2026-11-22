package grit.core.claim

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{ClaimHolder, DocKey, SessionId}
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** A claim: `holder` works its plugin's `subject` as `role`, made at `made` by `by` (with the
  * accounts linked to them when it is read), kept at `label`, worked by `session`. It has no
  * `session` until a session of its holder starts or resumes, which takes it
  * ([[grit.core.session.Sessions.record]]), and none again once that session's recorded end
  * frees it. `active` is the latest of `made` and the last event the reading transaction reads of
  * `session`, or, while it has none, of any session working as `holder`.
  */
final case class Claim(
    subject: DocKey,
    holder: ClaimHolder,
    role: ClaimRole,
    by: Principal.Person,
    made: Instant,
    label: Label,
    session: Option[SessionId],
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

  /** Refused, making nothing: the holder's own claim on the subject, which another session than
    * the one given works; it.
    */
  case HeldBy(claim: Claim)

  /** Refused, making nothing: the session given works as `holder`, not as the claim's. */
  case Elsewhere(session: SessionId, holder: ClaimHolder)
}

/** What [[Claims.moved]] came to. */
enum ClaimMoved {

  /** Moved, and now live. */
  case Made(claim: Claim)

  /** Refused, moving nothing: the label holds a claim by the holder moved to on another subject
    * already; it, when this transaction reads it.
    */
  case Holding(other: Option[Claim])

  /** Refused, moving nothing: the session moved to works as `holder`, another. */
  case Elsewhere(session: SessionId, holder: ClaimHolder)

  /** Moving nothing: the claim is gone. */
  case Gone
}

/** One plugin's claims, as its transaction reads them: at each label, one live claim per subject
  * and one per holder. A claim's lease is its session's: that session's last event keeps it from
  * lapsing, and while it has none, any session working as its holder does ([[Claim.lapses]]). A
  * release deletes it, and so does a claim taking it over once lapsed; the
  * session sweep deletes a claim lapsed longer than [[grit.core.session.Session.Unended]]. Nothing
  * is versioned.
  */
trait Claims extends caps.Pure {

  /** Claims `subject` (a key; the store never checks that it holds a document) for `holder` as
    * `role`, worked by `session` (none: by its holder's next session to start), by `by` at `at`,
    * kept at `label` joined with the transaction's floor and in its own room, as
    * [[grit.core.document.DocumentKeeper.write]] keeps a document. A claim at that label on
    * `subject` or by `holder` that has lapsed at `at` is deleted first; a session this
    * transaction does not read keeps no claim from lapsing. A claim `holder` already has on
    * `subject` there, when this transaction reads it, is renewed: its role becomes `role`, it
    * keeps its `made`, `by` and session, it takes `session` when it has none, and it is
    * [[Claimed.Made]]; when another session than a given `session` works it, nothing changes and
    * it is [[Claimed.HeldBy]]. A `session` this transaction reads working as another holder is
    * [[Claimed.Elsewhere]].
    */
  def claim(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      session: Option[SessionId],
      by: Principal.Person,
      label: Label,
      at: Instant
  )(using Tx^): Either[StoreError, Claimed]

  /** Deletes the claim on `subject` kept at exactly `label`, lapsed or not, when this transaction
    * reads it: it, or `None` when there is none it reads.
    */
  def release(subject: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Claim]]

  /** `claim`, as [[live]] answered it, moved to `holder`'s session `session` as `role`, made anew
    * at `at`, keeping `by` and its label: [[ClaimMoved.Made]] with it moved. [[ClaimMoved.Holding]] when
    * `holder` has another live claim at that label, [[ClaimMoved.Elsewhere]] when this transaction
    * reads `session` working as another holder, each moving nothing; [[ClaimMoved.Gone]] when it is
    * gone, as [[held]] finds it, though `holder` has another. A claim of `holder`'s on another subject, lapsed at `at`, is
    * deleted first. Who may
    * move it is the caller's rule.
    */
  def moved(claim: Claim, holder: ClaimHolder, session: SessionId, role: ClaimRole, at: Instant)(
      using Tx^
  ): Either[StoreError, ClaimMoved]

  /** This plugin's claims at every label this transaction reads that have not lapsed at `at`,
    * by subject, then by when they were made. A session this transaction does not read keeps no
    * claim from lapsing.
    */
  def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]]

  /** `claim`, as [[live]] answered it, as this transaction now reads it, locked until this
    * transaction ends: a second transaction asking for it, renewing it or releasing it waits
    * until this one ends, then finds what it left. Its role is the one it has now, renewed since
    * or not, and its `active` as this transaction reads sessions; one lapsed since is still
    * answered. `None` when it is gone (released, or taken over once lapsed), when its holder's
    * claim on its subject at its label is now another, made since, or when this transaction does
    * not read it. It writes nothing.
    */
  def held(claim: Claim)(using Tx^): Either[StoreError, Option[Claim]]

  /** Every claim on `subject` not lapsed at `at`, one at each label this transaction reads,
    * oldest `made` first, then by holder; each locked as [[held]] locks it. Empty when there is
    * none. It writes nothing.
    */
  def on(subject: DocKey, at: Instant)(using Tx^): Either[StoreError, Vector[Claim]]

  /** Waits until no other transaction holds `subject`'s lock for this plugin, at any label, then
    * holds it until this one ends. A higher room's holder delays a lower room's. A wait past the
    * transaction's `lock_timeout` answers its `StoreError`. It writes nothing and keeps no claim.
    */
  def lock(subject: DocKey)(using Tx^): Either[StoreError, Unit]
}

object Claims {

  /** 2 hours: how long after its session, or its holder, last worked a claim lapses
    * ([[Claim.lapses]]).
    */
  val Lease: FiniteDuration = 2.hours

  /** What a person is told of `claim` while `session` works it, after the words naming it: that
    * session, when it was last active, and when the claim frees itself.
    */
  def working(claim: Claim, session: SessionId): String =
    s"and session ${session.written} is working on it (last active ${claim.active}). The claim " +
      s"frees itself when that session ends, or at ${claim.lapses} if it does nothing more"
}
