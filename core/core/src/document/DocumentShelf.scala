package grit.core.document

import java.time.{Instant, LocalDate}

import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.place.Place
import grit.core.screen.Benched
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{DeclassificationPolicy, Integrity, Label}

/** One plugin's documents as it reads them: the current ones, withdrawals left out, and only
  * those its transaction reads ([[grit.core.store.Tx.clearance]]): a document kept in the
  * transaction's own room up to that room's label, any other up to
  * [[grit.core.store.Tx.cleared]]. Another plugin's are reached only through the service it
  * exports ([[grit.core.need.Needs]]).
  */
trait DocumentShelf {

  /** The current document under `key` kept at exactly `label`; `None` when there is none there,
    * it was withdrawn, or this transaction does not read it: one kept above what it reads is
    * never returned, whatever is kept, so choosing a variant wrongly cannot read up.
    */
  def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]]

  /** Its current documents this transaction reads, every key's and label's, most recently
    * written first, at most `n`; none when `n` is not positive.
    */
  def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]]

  /** What `keep` reads of its [[newest]] documents, most recently written first, back no further
    * than the first `past` holds to, and up to the `n`th of them `counts` holds to (so at most `n`
    * when it holds to all); none when `n` is not positive.
    */
  final def newestWhere[A](n: Int, past: Document => Boolean)(
      keep: Document => Option[A],
      counts: A => Boolean
  )(using
      Tx^
  ): Either[StoreError, Vector[A]] = {
    // Those kept up to and including the `n`th counted, or all when fewer are.
    def upTo(kept: Vector[A]): Vector[A] =
      kept.zipWithIndex
        .filter((a, _) => counts(a))
        .lift(n - 1)
        .fold(kept)((_, i) => kept.take(i + 1))
    // The newest `k`, `k` doubled until they hold `n` counted, reach one past, or are all.
    def from(k: Int): Either[StoreError, Vector[A]] =
      newest(k).flatMap { ds =>
        val kept = ds.takeWhile(d => !past(d)).flatMap(keep(_))
        val done = kept.count(counts) >= n || ds.size < k || ds.exists(past)
        if (done || k > Int.MaxValue / 2) Right(upTo(kept)) else from(k * 2)
      }
    if (n <= 0) Right(Vector.empty) else from(n)
  }

  /** Its current documents this transaction reads that hold `ref`, most recently written first,
    * at most `n`; none when `n` is not positive.
    */
  def referring(ref: DocRef, n: Int)(using Tx^): Either[StoreError, Vector[Document]]
}

/** One plugin's documents as it writes them, under the terms it declared. */
trait DocumentKeeper extends DocumentShelf {

  /** Makes `text`, `data`, `pointer` and `refs`, at `place`, the current document under `key` at `label`
    * joined with its transaction's floor ([[grit.core.visibility.Clearance.floor]]), written `at`:
    * never below what its writer could read. It is believed at `integrity`, as given. It is kept in the transaction's own room, or in none
    * when it has none, never a room the writer names. [[Written.kept]] says which label it was kept
    * at, and `current(key, kept)` finds it again. Under posting and a job's run the floor is
    * [[grit.core.store.Tx.cleared]], so that is `label join Tx.cleared(tx)`; a person's turn may
    * write above what it reads back. The version before under that key and label, if any, stops
    * being current and is marked for deletion ([[grit.core.retention.Target.Document]]); a document
    * under the key at another label is untouched. [[Written.Unchanged]] when the current version
    * there holds the same place, text, data, pointer, integrity and refs; [[StoreError.Invalid]],
    * writing nothing, for more than [[DocRef.Most]] refs. Past the bound, counted over every label,
    * the current documents placed least recently (ties by key, then label in its written form,
    * bytewise) are withdrawn as [[withdraw]] does, never the one written: a high variant few
    * readers place tends to go first. A `DatabaseError` when another transaction wrote `key` at
    * that label meanwhile: posting never does, as it runs one at a time per plugin, but two runs of
    * a plugin's keeping jobs can, each keeping in its own transaction.
    */
  def write(
      key: DocKey,
      label: Label,
      integrity: Integrity,
      place: Place,
      text: DocText,
      data: ujson.Value,
      pointer: Option[Pointer],
      at: Instant,
      refs: Set[DocRef] = Set.empty
  )(using Tx^): Either[StoreError, Written]

  /** Withdraws the current document under `key` at exactly `label`, at `at`: a version holding
    * nothing and no refs, kept at that label and in that document's room and believed as it was, becomes
    * current there, and both
    * are marked for deletion. The withdrawal's version, or `None` when `key` had no current
    * document at `label`.
    */
  def withdraw(key: DocKey, label: Label, at: Instant)(using
      Tx^
  ): Either[StoreError, Option[DocumentVersionId]]

  /** Puts `version`, one of its own current documents, up for review on `day`, at `at`, for an
    * edge to post ([[grit.core.review.DocumentReviews]]); `false`, writing nothing, when its key
    * at its label is up for `day` already, or `version` is not one of its current documents.
    * The label it is given outlives the version.
    */
  def forReview(version: DocumentVersionId, day: LocalDate, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean]

  /** The application in force for the document `pointer` reads, kept at `label`: its current
    * application, when `label` holds its compartment and that compartment has a policy version
    * in force: its newest published version, unless that is a revocation (a draft is never in
    * force); `None` otherwise, and for a label holding the unmapped mark. Its policy's `instructions`,
    * saying what a rewrite under it removes and what it writes in its place, and its `checks`, as
    * [[grit.core.screen.Checks.read]] reads them, only when its transaction reads the label the
    * policy is kept at (its compartment's own group's grant). Read from the operator's rows as
    * this transaction sees them now.
    */
  def declassifying(label: Label, pointer: Pointer)(using
      Tx^
  ): Either[StoreError, Option[PolicyApplication.InForce]]

  /** The compartment the current application to the document `pointer` reads names, when
    * `label` holds it, whether or not a version of its policy is in force: what [[benchable]]
    * is asked over, so that a compartment's first version is benched while still a draft.
    * `None` when the document has no application or its current one is a removal, and as
    * [[declassifying]] for `label`. Read from the operator's rows as this transaction sees them
    * now.
    */
  def applying(label: Label, pointer: Pointer)(using
      Tx^
  ): Either[StoreError, Option[DeclassificationPolicy.Over]]

  /** Keeps `d` as the current document of this plugin under a key derived from its source's
    * plugin and key (never repeating the source's key), at `d.label`, below this transaction's
    * floor (the *-property's one exception besides the trusted subject), at its source's place,
    * believed at `integrity`, holding its text, data naming only its source version and
    * application, and no pointer or refs; and records it as declassified. A rewrite kept under
    * that key at another label is withdrawn. As [[write]] in all else. [[StoreError.Invalid]],
    * writing nothing, when `d.applied`'s policy version is no longer in force ([[declassifying]])
    * or its application version no longer current.
    */
  final def declassified(d: Declassified, integrity: Integrity, at: Instant)(using
      Tx^
  ): Either[StoreError, Written] =
    Declassified.row(d, integrity).left.map(StoreError.Invalid(_)).flatMap(keepDeclassified(_, at))

  /** The primitive under [[declassified]], which alone makes a [[Declassified.Row]]: keeps
    * `row` as it says, after rechecking in its own statement that `row.applied`'s policy version
    * is in force ([[declassifying]]) and its application version current. It writes below the
    * floor; besides it, only the
    * plugin's own withdrawals do, each one bit at a key a reader could already see.
    */
  def keepDeclassified(row: Declassified.Row, at: Instant)(using Tx^): Either[StoreError, Written]

  /** `policy`'s checks, as [[grit.core.screen.Checks.read]] reads them, only when its
    * transaction reads the label the policy is kept at; `None` otherwise, when no such version
    * is kept, and for a revocation. Read from the operator's rows as this transaction sees them
    * now.
    */
  def checks(policy: DeclassificationPolicy)(using
      Tx^
  ): Either[StoreError, Option[Declassified.PolicyChecks]]

  /** The versions of `over` a bench may be due for, oldest first: the version in force (see
    * [[declassifying]]) and its latest draft above every published version, as this transaction
    * sees the operator's rows now; whether one is due is
    * [[grit.core.act.Declassifying.unbenched]]'s to say.
    */
  def benchable(over: DeclassificationPolicy.Over)(using
      Tx^
  ): Either[StoreError, Vector[DeclassificationPolicy]]

  /** The bench kept for `policy`, with the screening it was kept under; `None` when none is, or
    * when the one kept does not read, so the version is benched again.
    */
  def benched(policy: DeclassificationPolicy)(using
      Tx^
  ): Either[StoreError, Option[(Declassified.Screening, Benched)]]

  /** Keeps `row` as its policy version's bench, replacing the one kept before.
    * [[StoreError.Invalid]], keeping nothing, when no such policy version is kept.
    */
  def keepBenched(row: Declassified.BenchRow, at: Instant)(using Tx^): Either[StoreError, Unit]
}

object DocumentKeeper {

  /** `keeper`, each document it writes believed at the meet of the integrity given and `most`.
    */
  def believedAtMost(keeper: DocumentKeeper, most: Integrity): DocumentKeeper =
    new AtMost(keeper, most)

  private final class AtMost(keeper: DocumentKeeper, most: Integrity) extends DocumentKeeper {

    def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]] =
      keeper.current(key, label)

    def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] = keeper.newest(n)

    def referring(ref: DocRef, n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
      keeper.referring(ref, n)

    def write(
        key: DocKey,
        label: Label,
        integrity: Integrity,
        place: Place,
        text: DocText,
        data: ujson.Value,
        pointer: Option[Pointer],
        at: Instant,
        refs: Set[DocRef]
    )(using Tx^): Either[StoreError, Written] =
      keeper.write(key, label, integrity.meet(most), place, text, data, pointer, at, refs)

    def withdraw(key: DocKey, label: Label, at: Instant)(using
        Tx^
    ): Either[StoreError, Option[DocumentVersionId]] = keeper.withdraw(key, label, at)

    def forReview(version: DocumentVersionId, day: LocalDate, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = keeper.forReview(version, day, at)

    def declassifying(label: Label, pointer: Pointer)(using
        Tx^
    ): Either[StoreError, Option[PolicyApplication.InForce]] = keeper.declassifying(label, pointer)

    def applying(label: Label, pointer: Pointer)(using
        Tx^
    ): Either[StoreError, Option[DeclassificationPolicy.Over]] = keeper.applying(label, pointer)

    def keepDeclassified(row: Declassified.Row, at: Instant)(using
        Tx^
    ): Either[StoreError, Written] = keeper.keepDeclassified(row.believedAtMost(most), at)

    def checks(policy: DeclassificationPolicy)(using
        Tx^
    ): Either[StoreError, Option[Declassified.PolicyChecks]] = keeper.checks(policy)

    def benchable(over: DeclassificationPolicy.Over)(using
        Tx^
    ): Either[StoreError, Vector[DeclassificationPolicy]] = keeper.benchable(over)

    def benched(policy: DeclassificationPolicy)(using
        Tx^
    ): Either[StoreError, Option[(Declassified.Screening, Benched)]] = keeper.benched(policy)

    def keepBenched(row: Declassified.BenchRow, at: Instant)(using
        Tx^
    ): Either[StoreError, Unit] = keeper.keepBenched(row, at)
  }
}
