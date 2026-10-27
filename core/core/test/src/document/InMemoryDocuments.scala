package grit.core.document

import java.time.{Instant, LocalDate}

import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.review.{DocumentLabel, DocumentReview, DocumentReviews, ReviewWord, Reviews}
import grit.core.screen.{Benched, Checks}
import grit.core.store.{InMemoryTombstones, Savepoints, StoreError, Tx}
import grit.core.visibility.{DeclassificationPolicy, Integrity, Item, Label}

/** An in-memory [[DocumentStore]], with each plugin's [[DocumentKeeper]], for tests, keeping
  * [[DocumentContract]]. Versions are numbered by one counter across every plugin. It marks
  * versions in `tombstones`, which reads which plugin's a version is from it. Search scores a
  * document by how often the query's words occur in it, not by BM25: the contract's searches
  * order strictly under either. It floors, places and filters by the `Tx`'s clearance, and
  * otherwise ignores it: nothing is rolled back but by its [[savepoints]].
  */
final class InMemoryDocuments extends DocumentStore {
  import InMemoryDocuments.{Row, Up, UpFor}

  // Only ever replaced by a new immutable map, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[Long, Row]

  // The identity column's last value, one across every plugin: only ever counted up.
  @caps.unsafe.untrackedCaptures
  private var last = 0L

  // As `rows`: only ever replaced by a new immutable map. Each plugin's terms, and whether it
  // is enabled.
  @caps.unsafe.untrackedCaptures
  private var terms = Map.empty[PluginName, (DocumentTerms, Boolean)]

  // As `rows`: only ever replaced by a new immutable map. The reviews put up, by their key.
  @caps.unsafe.untrackedCaptures
  private var ups = Map.empty[UpFor, Up]

  /** Where its keepers mark the versions they leave for deletion. */
  val tombstones: InMemoryTombstones =
    new InMemoryTombstones({
      case Target.Document(v) => rows.get(DocumentVersionId.value(v)).map(_.plugin)
      case _ => None
    })

  // As `rows`: only ever replaced by a new immutable map. The operator's rows: each policy's
  // versions, each with its text (none for a revocation), its checks as written and the label
  // it is kept at; each document's applications, by its pointer.
  @caps.unsafe.untrackedCaptures
  private var policies = Map.empty[
    (DeclassificationPolicy.Over, Int),
    (
        text: Option[String],
        checks: Option[ujson.Value],
        kept: Label,
        status: InMemoryDocuments.Status
    )
  ]

  // As `policies`: only ever replaced by a new immutable map.
  @caps.unsafe.untrackedCaptures
  private var applications = Map.empty[(Pointer, Int), Option[DeclassificationPolicy.Over]]

  // As `rows`: each declassified version's audit row, by its version.
  @caps.unsafe.untrackedCaptures
  private var audits = Map.empty[Long, InMemoryDocuments.Audit]

  // As `rows`: each policy version's bench, the screening it was kept under and its verdict.
  @caps.unsafe.untrackedCaptures
  private var benches =
    Map.empty[(DeclassificationPolicy.Over, Int), (Declassified.Screening, Benched)]

  /** Records the `version`th of `over`'s policy, kept at `kept`, as an operator's row does:
    * `text`, or a revocation when `None`, with `checks` as the row's column holds them, at
    * `status`; and, when `benched` and those checks read, a passing bench of them, as the job's
    * bench move keeps one ([[grit.core.act.Declassifying.screening]]). Its bench is left as it
    * was otherwise.
    */
  def policy(
      over: DeclassificationPolicy.Over,
      version: Int,
      text: Option[String] = Some(InMemoryDocuments.Instructions),
      kept: Label = Label.Public,
      checks: Option[ujson.Value] = Some(InMemoryDocuments.Checks),
      benched: Boolean = true,
      status: InMemoryDocuments.Status = InMemoryDocuments.Status.Withdraw
  ): Unit = {
    policies = policies.updated((over, version), (text, checks, kept, status))
    if (benched)
      Checks.read(checks).foreach { read =>
        Benched.read(true, ujson.Arr()).foreach { pass =>
          benches = benches.updated(
            (over, version),
            (grit.core.act.Declassifying.screening(read), pass)
          )
        }
      }
  }

  /** Records `benched` as the bench of `over`'s `version`th policy, under the screening of its
    * checks as they are now, as the job's bench move keeps one; nothing when they do not read.
    */
  def bench(over: DeclassificationPolicy.Over, version: Int, benched: Benched): Unit =
    policies.get((over, version)).map(_.checks).map(Checks.read).foreach {
      case Right(read) =>
        benches = benches.updated(
          (over, version),
          (grit.core.act.Declassifying.screening(read), benched)
        )
      case Left(_) => ()
    }

  /** `over`'s `version`th policy at `status`, as the operator's publish sets it; nothing when no
    * such version is kept.
    */
  def publish(
      over: DeclassificationPolicy.Over,
      version: Int,
      status: InMemoryDocuments.Status
  ): Unit =
    policies.get((over, version)).foreach { row =>
      policies = policies.updated((over, version), (row.text, row.checks, row.kept, status))
    }

  /** Forgets the bench of `over`'s `version`th policy, as if none had been kept. */
  def unbench(over: DeclassificationPolicy.Over, version: Int): Unit =
    benches = benches - ((over, version))

  /** Records the `version`th application to the document `pointer` reads: `over`'s policy, or
    * none when removed, as an operator's row does.
    */
  def application(
      pointer: Pointer,
      version: Int,
      over: Option[DeclassificationPolicy.Over]
  ): Unit =
    applications = applications.updated((pointer, version), over)

  /** The audit row of the declassified version `v`, when it is one. */
  def declassification(v: DocumentVersionId): Option[InMemoryDocuments.Audit] =
    audits.get(DocumentVersionId.value(v))

  /** `over`'s policy in force: its newest published version, when that is not a revocation. */
  private def inForce(over: DeclassificationPolicy.Over): Option[DeclassificationPolicy] =
    published(over).lastOption.collect { case (v, true, _) => DeclassificationPolicy(over, v) }

  /** `over`'s published versions, oldest first: each with whether it holds text, and whether it
    * was published to withdraw.
    */
  private def published(over: DeclassificationPolicy.Over): Vector[(Int, Boolean, Boolean)] =
    policies.toVector
      .collect {
        case ((o, v), row) if o == over && row.status != InMemoryDocuments.Status.Draft =>
          (v, row.text.isDefined, row.status == InMemoryDocuments.Status.Withdraw)
      }
      .sortBy(_._1)

  /** The lowest version whose rewrites stand under `policy`, in force
    * ([[PolicyApplication.InForce.keptFrom]]).
    */
  private def keptFrom(policy: DeclassificationPolicy): Int =
    published(policy.over)
      .collect { case (v, text, withdraw) if v <= policy.version && (withdraw || !text) => v }
      .lastOption
      .getOrElse(1)

  /** `over`'s latest draft above its newest published version, when there is one. */
  private def drafted(over: DeclassificationPolicy.Over): Option[DeclassificationPolicy] = {
    val above = published(over).lastOption.fold(0)(_._1)
    policies.keys
      .collect {
        case (o, v)
            if o == over && v > above &&
              policies.get((o, v)).exists(_.status == InMemoryDocuments.Status.Draft) =>
          v
      }
      .maxOption
      .map(DeclassificationPolicy(over, _))
  }

  /** The current application to the document `pointer` reads, when it is not a removal. */
  private def applied(pointer: Pointer): Option[(DeclassificationPolicy.Over, Int)] =
    applications
      .collect { case ((p, v), over) if p == pointer => (v, over) }
      .maxByOption(_._1)
      .flatMap((v, over) => over.map(_ -> v))

  /** Its part of a transaction undone: what its keepers wrote, and the versions they marked. */
  val savepoints: Savepoints = new Savepoints {
    def atomic[A](body: Tx^ ?=> Either[StoreError, A])(using
        tx: Tx^
    ): Either[StoreError, A] = {
      // Its identity column is not rolled back, as a sequence is not: `last` stays.
      val (before, upBefore, auditsBefore) = (rows, ups, audits)
      val out = tombstones.undoing(body(using tx))
      if (out.isLeft) {
        rows = before
        ups = upBefore
        audits = auditsBefore
      }
      out
    }
  }

  /** `plugin`'s documents as it writes them, under `under`. */
  def keeper(plugin: PluginName, under: DocumentTerms): DocumentKeeper =
    new Shelf(plugin) with DocumentKeeper {
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
      )(using tx: Tx^): Either[StoreError, Written] =
        put(
          key,
          label.join(Tx.clearance(tx).floor),
          integrity,
          place,
          text,
          data,
          pointer,
          at,
          refs
        )

      /** As [[write]], kept at exactly `kept`. */
      private def put(
          key: DocKey,
          kept: Label,
          integrity: Integrity,
          place: Place,
          text: DocText,
          data: ujson.Value,
          pointer: Option[Pointer],
          at: Instant,
          refs: Set[DocRef]
      )(using tx: Tx^): Either[StoreError, Written] = {
        val clearance = Tx.clearance(tx)
        head(plugin, key, kept) match {
          case _ if refs.size > DocRef.Most =>
            Left(StoreError.Invalid(s"a document holds at most ${DocRef.Most} refs"))
          case _ if Document.oversized(data).nonEmpty =>
            Left(StoreError.Invalid(Document.oversized(data).mkString))
          case Some((v, r))
              if r.text.contains(text) && r.place == place && r.data == data &&
                r.pointer == pointer && r.integrity == integrity && r.refs == refs =>
            Right(Written.Unchanged(version(v), kept))
          case _
              if under.full == DocumentTerms.Full.Refuse &&
                holding(plugin).count((_, r) => !(r.key == key && r.label == kept)) >=
                under.bound =>
            Left(StoreError.Full(under.bound))
          case before =>
            for {
              _ <- before.fold(Right(()))((v, r) => supersede(v, r, at, under.keepLast))
              v = insert(
                Row(
                  plugin,
                  key,
                  kept,
                  integrity,
                  clearance.own.map(_.room),
                  place,
                  Some(text),
                  data,
                  pointer,
                  at,
                  None,
                  Placement(0, at),
                  None,
                  refs
                )
              )
              over = holding(plugin).filterNot((_, r) => r.key == key && r.label == kept)
              excess = (over.size + 1 - under.bound) max 0
              gone = over
                .sortBy((_, r) =>
                  (r.placement.lastPlaced, DocKey.value(r.key), Label.written(r.label))
                )
                .take(excess)
                .map((_, r) => (r.key, r.label))
              _ <- gone.foldLeft[Either[StoreError, Unit]](Right(()))((acc, kl) =>
                acc.flatMap(_ => withdraw(kl._1, kl._2, at).map(_ => ()))
              )
            } yield Written.Versioned(version(v), kept, gone)
        }
      }

      def withdraw(key: DocKey, label: Label, at: Instant)(using
          Tx^
      ): Either[StoreError, Option[DocumentVersionId]] =
        head(plugin, key, label) match {
          case Some((v, r)) if r.text.nonEmpty =>
            for {
              _ <- supersede(v, r, at, under.keepLast)
              _ <- marked(plugin, key, label, 0, under.keepLast)
              w = insert(
                r.copy(
                  text = None,
                  data = ujson.Obj(),
                  pointer = None,
                  written = at,
                  superseded = None,
                  placement = Placement(0, at),
                  following = None,
                  refs = Set.empty
                )
              )
              _ <- tombstones.write(Target.Document(version(w)), at)
            } yield Some(version(w))
          case _ => Right(None)
        }

      def declassifying(label: Label, pointer: Pointer)(using
          tx: Tx^
      ): Either[StoreError, Option[PolicyApplication.InForce]] =
        Right(
          for {
            (over, version) <- applied(pointer)
            policy <- inForce(over)
            _ <- policy.lowered(label)
          } yield new PolicyApplication.InForce(
            PolicyApplication(policy, pointer, version),
            keptFrom(policy),
            policies.get((policy.over, policy.version)).collect {
              case (Some(text), checks, kept, _) if Tx.clearance(tx).reads(Item.Kept(None), kept) =>
                (text, Checks.read(checks))
            }
          )
        )

      def keepDeclassified(row: Declassified.Row, at: Instant)(using
          Tx^
      ): Either[StoreError, Written] = {
        val a = row.applied
        if (
          !inForce(a.policy.over).contains(a.policy) ||
          !applied(a.document).contains((a.policy.over, a.version))
        )
          Left(
            StoreError.Invalid(
              "the declassification's policy or application is no longer current"
            )
          )
        else
          for {
            written <- put(
              row.key,
              row.label,
              row.integrity,
              row.place,
              row.text,
              row.data,
              None,
              at,
              Set.empty
            )
            _ <- rows
              .collect {
                case (_, r)
                    if r.plugin == plugin && r.key == row.key && r.label != row.label &&
                      r.superseded.isEmpty && r.text.nonEmpty =>
                  r.label
              }
              .foldLeft[Either[StoreError, Unit]](Right(()))((acc, l) =>
                acc.flatMap(_ => withdraw(row.key, l, at).map(_ => ()))
              )
          } yield {
            written match {
              case Written.Versioned(v, _, _) =>
                audits = audits.updated(
                  DocumentVersionId.value(v),
                  (
                    source = row.source,
                    policy = a.policy.version,
                    application = a.version,
                    attempt = row.attempt.number
                  )
                )
              case Written.Unchanged(_, _) => ()
            }
            written
          }
      }

      def checks(policy: DeclassificationPolicy)(using
          tx: Tx^
      ): Either[StoreError, Option[Declassified.PolicyChecks]] =
        Right(policies.get((policy.over, policy.version)).collect {
          case (Some(_), checks, kept, _) if Tx.clearance(tx).reads(Item.Kept(None), kept) =>
            new Declassified.PolicyChecks(policy, Checks.read(checks))
        })

      def applying(label: Label, pointer: Pointer)(using
          Tx^
      ): Either[StoreError, Option[DeclassificationPolicy.Over]] =
        Right(applied(pointer).map(_._1).filter(_.lowered(label).isDefined))

      def benchable(over: DeclassificationPolicy.Over)(using
          Tx^
      ): Either[StoreError, Vector[DeclassificationPolicy]] =
        Right(inForce(over).toVector ++ drafted(over))

      def benched(policy: DeclassificationPolicy)(using
          Tx^
      ): Either[StoreError, Option[(Declassified.Screening, Benched)]] =
        Right(benches.get((policy.over, policy.version)))

      def keepBenched(row: Declassified.BenchRow, at: Instant)(using
          Tx^
      ): Either[StoreError, Unit] =
        if (!policies.contains((row.policy.over, row.policy.version)))
          Left(StoreError.Invalid("no such policy version is kept"))
        else {
          benches = benches.updated(
            (row.policy.over, row.policy.version),
            (row.screening, row.benched)
          )
          Right(())
        }

      def forReview(version: DocumentVersionId, day: LocalDate, at: Instant)(using
          Tx^
      ): Either[StoreError, Boolean] = {
        val v = DocumentVersionId.value(version)
        Right(
          rows
            .get(v)
            .filter(r => r.plugin == plugin && r.superseded.isEmpty && r.text.nonEmpty && reads(r))
            .map(r => UpFor(plugin, r.key, r.label, day))
            .filterNot(ups.contains)
            .fold(false) { u =>
              ups = ups.updated(u, Up(Some(v), at, None, None, None))
              true
            }
        )
      }
    }

  /** `plugin`'s documents as it reads them. */
  def shelf(plugin: PluginName): DocumentShelf = new Shelf(plugin)

  /** The reviews of the documents its keepers put up. */
  val reviews: DocumentReviews = new DocumentReviews {
    def of(plugin: PluginName): Reviews[DocumentReview, ReviewWord] =
      new Reviews[DocumentReview, ReviewWord] {
        def unposted(to: Place)(using tx: Tx^): Either[StoreError, Vector[DocumentReview]] =
          Right(Tx.writable(to).fold(Vector.empty) { at =>
            ups.toVector
              .collect {
                case (u, Up(Some(v), queued, None, None, _))
                    if u.plugin == plugin && at.dominates(u.label) =>
                  rows
                    .get(v)
                    .filter(reads)
                    .flatMap(r => r.text.map(document(v, r, _)))
                    .map(d => (queued, v, DocumentReview(plugin, u.key, u.label, u.day, d)))
              }
              .flatten
              .sortBy((queued, v, r) => (queued, DocKey.value(r.key), r.day, v))
              .map(_._3)
          })

        def posted(subject: DocumentReview, address: String, at: Instant)(using
            Tx^
        ): Either[StoreError, Boolean] = {
          val taken = ups.values.exists(_.address.exists(_._1 == address))
          waiting(subject) match {
            case Some((u, up)) if !taken =>
              ups = ups.updated(u, up.copy(address = Some((address, at))))
              Right(true)
            case _ => Right(false)
          }
        }

        def dropped(subject: DocumentReview, at: Instant)(using Tx^): Either[StoreError, Boolean] =
          waiting(subject) match {
            case Some((u, up)) =>
              ups = ups.updated(u, up.copy(dropped = Some(at)))
              Right(true)
            case None => Right(false)
          }

        def reacted(address: String, rater: Account, verdict: ReviewWord, at: Instant)(using
            Tx^
        ): Either[StoreError, Boolean] =
          Right(postedAt(address).fold(false) { (u, up) =>
            ups = ups.updated(u, up.copy(verdict = Some((verdict, rater, at))))
            true
          })

        def unreacted(address: String, rater: Account, verdict: ReviewWord)(using
            Tx^
        ): Either[StoreError, Boolean] =
          Right(postedAt(address) match {
            case Some((u, up)) if up.verdict.exists(v => v._1 == verdict && v._2 == rater) =>
              ups = ups.updated(u, up.copy(verdict = None))
              true
            case _ => false
          })

        /** `subject`'s review while it waits: neither posted nor dropped, its version kept. */
        private def waiting(subject: DocumentReview): Option[(UpFor, Up)] = {
          val u = UpFor(plugin, subject.key, subject.label, subject.day)
          ups
            .get(u)
            .filter(up => up.version.nonEmpty && up.address.isEmpty && up.dropped.isEmpty)
            .map(u -> _)
        }

        private def postedAt(address: String): Option[(UpFor, Up)] =
          ups.find((u, up) => u.plugin == plugin && up.address.exists(_._1 == address))
      }

    def labels(plugin: PluginName, from: LocalDate)(using
        tx: Tx^
    ): Either[StoreError, Vector[DocumentLabel]] =
      Right(
        ups.toVector
          .collect {
            case (u, up @ Up(_, _, _, _, Some((verdict, rater, at))))
                if u.plugin == plugin && !u.day.isBefore(from) &&
                  Tx.clearance(tx).reads(Item.Kept(None), u.label) =>
              DocumentLabel(u.key, u.label, u.day, up.version.map(version), verdict, rater, at)
          }
          .sortBy(l => (l.day, l.at, DocKey.value(l.key)))
      )

    def forget(before: Instant)(using Tx^): Either[StoreError, Int] = {
      val gone = ups.filter((_, up) => up.queued.isBefore(before))
      ups = ups -- gone.keys
      Right(gone.size)
    }
  }

  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
    Right(terms.toVector.collect { case (p, (t, true)) => p -> t })

  /** `r`, version `v`, no longer current from `at`; the version of its key and label leaving the
    * `keepLast` newest superseded is marked for deletion.
    */
  private def supersede(v: Long, r: Row, at: Instant, keepLast: Int)(using
      Tx^
  ): Either[StoreError, Unit] = {
    rows = rows.updated(v, r.copy(superseded = Some(at)))
    marked(r.plugin, r.key, r.label, keepLast, 1)
  }

  /** Marks for deletion `n` of `plugin`'s superseded versions holding something under `key` at
    * `label`, from the `from`th newest (0 the newest), each as of when it stopped being current.
    */
  private def marked(plugin: PluginName, key: DocKey, label: Label, from: Int, n: Int)(using
      Tx^
  ): Either[StoreError, Unit] =
    rows.toVector
      .flatMap((v, r) =>
        r.superseded
          .filter(_ => r.plugin == plugin && r.key == key && r.label == label && r.text.nonEmpty)
          .map(s => (v, s))
      )
      .sortBy((v, s) => (s, v))
      .reverse
      .slice(from, from + n)
      .foldLeft[Either[StoreError, Unit]](Right(()))((acc, vs) =>
        acc.flatMap(_ => tombstones.write(Target.Document(version(vs._1)), vs._2).map(_ => ()))
      )

  /** Whether `r` held something current at `at`: written before it, superseded by none
    * written before it.
    */
  private def currentAt(r: Row, at: Instant): Boolean =
    r.text.nonEmpty && r.written.isBefore(at) && r.superseded.forall(!_.isBefore(at))

  def shelved(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Shelved]] =
    Right(
      rows.values.toVector
        .filter(r => plugins.contains(r.plugin) && currentAt(r, at) && reads(r))
        .map(r => Shelved(r.plugin, r.place))
        .distinct
    )

  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]] = {
    val words = InMemoryDocuments.words(query).distinct
    Right(
      if (words.isEmpty) Vector.empty
      else
        rows.toVector
          .collect {
            case (v, r @ Row(plugin, _, _, _, _, place, Some(text), _, _, _, _, _, _, _))
                if shelves.contains(Shelved(plugin, place)) && currentAt(r, at) && reads(r) =>
              val found = InMemoryDocuments.words(DocText.value(text))
              val score = words.map(w => found.count(_ == w)).sum.toDouble
              DocumentSearch.Hit(document(v, r, text), score)
          }
          .filter(_.score > 0)
          .sortBy(h =>
            (
              -h.score,
              -h.document.written.toEpochMilli,
              -DocumentVersionId.value(h.document.version)
            )
          )
          .take(limit max 0)
    )
  }

  def read(versions: Vector[DocumentVersionId])(using Tx^): Either[StoreError, Vector[Document]] =
    Right(versions.flatMap { v =>
      rows
        .get(DocumentVersionId.value(v))
        .filter(reads)
        .flatMap(r => r.text.map(document(DocumentVersionId.value(v), r, _)))
    })

  def declare(enabled: Vector[(PluginName, DocumentTerms)])(using Tx^): Either[StoreError, Unit] = {
    terms = terms.map((p, t) => p -> (t._1, false)) ++ enabled.map((p, t) => p -> (t, true))
    Right(())
  }

  def placed(
      versions: Vector[DocumentVersionId],
      at: Instant
  )(using Tx^): Either[StoreError, Unit] = {
    versions.map(DocumentVersionId.value).foreach { v =>
      rows.get(v).foreach { r =>
        val latest = if (at.isAfter(r.placement.lastPlaced)) at else r.placement.lastPlaced
        rows = rows.updated(v, r.copy(placement = Placement(r.placement.count + 1, latest)))
      }
    }
    Right(())
  }

  def followed(
      versions: Vector[DocumentVersionId],
      at: Instant
  )(using Tx^): Either[StoreError, Unit] = {
    versions.map(DocumentVersionId.value).foreach { v =>
      rows.get(v).filter(reads).foreach { r =>
        val count = r.following.fold(0L)(_.count) + 1
        val latest = r.following.map(_.last).filter(_.isAfter(at)).getOrElse(at)
        rows = rows.updated(v, r.copy(following = Following.of(count, latest)))
      }
    }
    Right(())
  }

  def kept()(using Tx^): Either[StoreError, Vector[PluginName]] =
    Right((rows.values.map(_.plugin).toVector ++ terms.keys).distinct)

  def forget(version: DocumentVersionId)(using Tx^): Either[StoreError, Unit] = {
    val v = DocumentVersionId.value(version)
    rows = rows - v
    ups = ups.map((u, up) => u -> (if (up.version.contains(v)) up.copy(version = None) else up))
    Right(())
  }

  def remove(plugin: PluginName, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    val gone = rows.collect { case (v, r) if r.plugin == plugin => v }.toSet
    val ended = tombstones.pending.map(_.target).foldLeft[Either[StoreError, Unit]](Right(())) {
      case (acc, t @ Target.Document(v)) if gone(DocumentVersionId.value(v)) =>
        acc.flatMap(_ => tombstones.collected(t, at))
      case (acc, _) => acc
    }
    rows = rows -- gone
    terms = terms - plugin
    ups = ups.filterNot((u, _) => u.plugin == plugin)
    ended
  }

  private class Shelf(plugin: PluginName) extends DocumentShelf {
    def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]] =
      Right(
        head(plugin, key, label)
          .filter((_, r) => reads(r))
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )

    def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
      Right(
        holding(plugin)
          .filter((_, r) => reads(r))
          .sortBy((v, r) => (r.written, v))
          .reverse
          .take(n max 0)
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )

    def referring(ref: DocRef, n: Int)(using Tx^): Either[StoreError, Vector[Document]] =
      Right(
        holding(plugin)
          .filter((_, r) => reads(r) && r.refs.contains(ref))
          .sortBy((v, r) => (r.written, v))
          .reverse
          .take(n max 0)
          .flatMap((v, r) => r.text.map(document(v, r, _)))
      )
  }

  /** `plugin`'s current version under `key` at `label`, holding something or a withdrawal. */
  private def head(plugin: PluginName, key: DocKey, label: Label): Option[(Long, Row)] =
    rows.find((_, r) =>
      r.plugin == plugin && r.key == key && r.label == label && r.superseded.isEmpty
    )

  /** Whether the transaction reads `r`, kept in its room at its label. */
  private def reads(r: Row)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.label)

  /** `plugin`'s current versions holding something. */
  private def holding(plugin: PluginName): Vector[(Long, Row)] =
    rows.toVector.filter((_, r) => r.plugin == plugin && r.superseded.isEmpty && r.text.nonEmpty)

  private def insert(row: Row): Long = {
    last += 1
    rows = rows.updated(last, row)
    last
  }

  private def version(v: Long): DocumentVersionId =
    DocumentVersionId.of(v).getOrElse(throw new IllegalStateException(s"version $v"))

  private def document(v: Long, r: Row, text: DocText): Document =
    Document(
      version(v),
      r.plugin,
      r.key,
      r.label,
      r.integrity,
      r.place,
      text,
      r.data,
      r.pointer,
      r.written,
      r.placement,
      r.following
    )
}

object InMemoryDocuments {

  /** The text a policy recorded without one given holds. */
  val Instructions: String = "remove the team's details"

  /** The checks a policy recorded without them given holds: one signal that must be absent, and
    * one example of each kind, as [[grit.core.screen.Checks.read]] reads them.
    */
  val Checks: ujson.Value = ujson.Obj(
    "signals" -> ujson.Arr(ujson.Obj("infer" -> "where the team meets", "must" -> "absent")),
    "examples" -> ujson.Obj(
      "accept" -> ujson.Arr("The team meets."),
      "refuse" -> ujson.Arr("The team meets at the lake house.")
    )
  )

  /** Where a policy version stands, as its row's `status` and `on_publish` hold them. */
  enum Status extends caps.Pure {

    /** Written, not yet published (`draft`): never in force. */
    case Draft

    /** Published to withdraw the rewrites made under earlier versions (`withdraw`), as every
      * revocation is.
      */
    case Withdraw

    /** Published to keep each earlier rewrite until this version decides its document (`keep`). */
    case Keep
  }

  /** A declassified version's audit row: the version it was made from, the policy and
    * application versions it was made under, and the number of the attempt that wrote it.
    */
  type Audit = (source: DocumentVersionId, policy: Int, application: Int, attempt: Int)

  /** One version's row: `label`, `integrity` and `room` as it is kept; `text` `None` for a
    * withdrawal.
    */
  private final case class Row(
      plugin: PluginName,
      key: DocKey,
      label: Label,
      integrity: Integrity,
      room: Option[Place],
      place: Place,
      text: Option[DocText],
      data: ujson.Value,
      pointer: Option[Pointer],
      written: Instant,
      superseded: Option[Instant],
      placement: Placement,
      following: Option[Following],
      refs: Set[DocRef]
  )

  /** A review's key: a plugin's document key at its label, put up on a day. */
  private final case class UpFor(plugin: PluginName, key: DocKey, label: Label, day: LocalDate)

  /** A review put up: its version while kept, when it was put up, where and when it was posted
    * or when dropped, and the verdict standing, by whom and when.
    */
  private final case class Up(
      version: Option[Long],
      queued: Instant,
      address: Option[(String, Instant)],
      dropped: Option[Instant],
      verdict: Option[(ReviewWord, Account, Instant)]
  )

  /** `text`'s words, lower case. */
  private def words(text: String): Vector[String] =
    text.toLowerCase.split("[^\\p{L}\\p{N}]+").toVector.filter(_.nonEmpty)
}
