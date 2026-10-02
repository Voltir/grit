package grit.eval.harness.corpus

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef}
import grit.core.stitch.{Placed, Seen as LiveSeen, StitchReads, Stitching, Tuning}
import grit.core.store.{Origin, StoreError}
import grit.core.triage.{Tags, TriageStore}
import grit.dbos.engine.{Build, Reader}
import grit.lifecycle.triage.{TriageInput, TriageQuestion}

/** A corpus: its manifest, and its cases in their order. */
final case class Corpus(manifest: Manifest, cases: Vector[Case])

/** A corpus captured from a restored database, through the shipped builders. */
object Capture {

  /** The corpus of every heard message `reader`'s database tagged before `dump.at` and that is
    * a Slack message ([[CaseId.of]]): its manifest and its cases, ordered by when they were
    * tagged, then by id. Each case's inputs are rebuilt under its own placement's tuning, or
    * the manifest's. `source` and `restored` name the databases dumped and read, and
    * `capture` the build capturing. Equal databases capture equal corpora. `Left` when the
    * store cannot be read, naming what was being read; never a message's text.
    */
  def apply(
      reader: Reader^,
      source: String,
      restored: String,
      dump: Dump,
      capture: Build
  ): Either[String, Corpus] = {
    val reads = StitchReads(
      reader.entries,
      reader.conversations,
      reader.lifecycle,
      reader.stitches,
      reader.search,
      reader.principals
    )
    def read[A](what: String)(body: (grit.core.store.Tx^) ?=> Either[StoreError, A]) =
      reader.db.read(body).left.map(e => s"$what unread: ${kind(e)}")
    def originOf(c: ConversationId): Either[String, Option[Origin]] =
      read("conversation")(reader.conversations.get(c)).map(_.map(_.origin))
    def opening(c: ConversationId): Either[String, CaseId] =
      originOf(c).flatMap(
        _.flatMap(CaseId.opening).toRight(s"no Slack thread for ${ConversationId.value(c)}")
      )
    for {
      tagged <- read("tags")(reader.triage.tagged(Instant.EPOCH, dump.at))
      starts <- reader.starts().left.map(e => s"engine starts unread: ${kind(e)}")
      settings <- read("settings")(reader.lifecycle.current())
      placed <- each(tagged)(t => read("placement")(reader.stitches.placed(t.entry)).map(t -> _))
      tunings = placed.flatMap(_._2.map(_.seen.tuning))
      tuning = tunings.distinct
        .sortBy(t => (-tunings.count(_ == t), t.toString))
        .headOption
        .getOrElse(Tuning.Default)
      cases <- each(placed) { (t, live) =>
        val c = t.triage.period.conversationId
        originOf(c).flatMap {
          case None => Right(None)
          case Some(origin) =>
            CaseId.of(origin, t.entry) match {
              case None => Right(None)
              case Some(id) =>
                val own = live.map(_.seen.tuning).filter(_ != tuning)
                val under = own.getOrElse(tuning)
                for {
                  thread <- opening(c)
                  links <- read("links")(reader.stitches.links(Vector(c)))
                  exchange <- links
                    .find(_.conversation == c)
                    .fold[Either[String, CaseId]](Right(thread))(l => opening(l.root))
                  speakers <- read("speakers")(reader.principals.speakers(Vector(t.entry)))
                  stitch <- live.fold[Either[String, Option[Stitched]]](Right(None))(p =>
                    stitched(reads, reader, TurnRef(c, t.triage.turn), p, under, opening)
                      .map(Some(_))
                  )
                } yield {
                  val recorded = reader.workflow(t.triage.workflowId)
                  Some(
                    Case(
                      id,
                      t.entry,
                      c,
                      t.at,
                      Triaged(
                        t.triage.workflowId,
                        recorded,
                        recorded.fold(Build.Unknown)(r => Triaged.buildAt(starts, r.created))
                      ),
                      liveTags(t.tags),
                      asked(reads, reader, t, under),
                      speakers.of(t.entry).map(Digest.text),
                      Clusters(thread, exchange),
                      stitch,
                      own
                    )
                  )
                }
            }
        }
      }
    } yield {
      val kept = cases.flatten.sortBy(k => (k.tagged, k.id.written))
      Corpus(
        Manifest(
          source,
          restored,
          dump,
          Settings.of(settings),
          tuning,
          tunings.distinct.size,
          Constants.Shipped,
          capture,
          kept.size,
          kept.count(_.stitch.isDefined)
        ),
        kept
      )
    }
  }

  /** Triage's question for `t`, rebuilt under `tuning`; `None` when the builder refuses it. */
  private def asked(
      reads: StitchReads,
      reader: Reader^,
      t: TriageStore.Tagged,
      tuning: Tuning
  ): Option[Asked] =
    TriageInput.build(reads, reader.db, t.triage, tuning).toOption.flatMap { (_, state) =>
      TriageQuestion
        .request(TriageQuestion.Wording.Shipped, state)
        .map(r =>
          Asked(
            Built(Digest.json(r.state), Digest.request(r)),
            state.message.length,
            state.thread.length
          )
        )
    }

  /** `live`, the placement kept for `turn`'s message, beside what the builder offers it now
    * under `tuning`.
    */
  private def stitched(
      reads: StitchReads,
      reader: Reader^,
      turn: TurnRef,
      live: Placed,
      tuning: Tuning,
      opening: ConversationId => Either[String, CaseId]
  ): Either[String, Stitched] =
    for {
      placement <- live match {
        case f: Placed.Follows => opening(f.root).map(Placement.Follows(_, f.p))
        case b: Placed.Begins => Right(Placement.Begins(b.p))
        case u: Placed.Unread => Right(Placement.Unread(Failure.of(u.why)))
      }
      offered <- each(live.seen.offered)(o => opening(o.root).map(Offering(_, o.why, o.p)))
    } yield {
      val rebuilt = Stitching.offered(reads, reader.db, turn, tuning).toOption.flatten
      val liveOffered = live.seen.offered.map((o: LiveSeen.Offer) => o.root -> o.why)
      rebuilt match {
        case None => Stitched(placement, offered, None, SeenCheck.Unbuilt, 0)
        case Some(offer) =>
          val now = offer.exchanges.map(e => e.root -> e.offered)
          val shown = Stitching.shown(offer)
          Stitched(
            placement,
            offered,
            Stitching.request(offer).map(r => Built(Digest.json(shown), Digest.request(r))),
            SeenCheck.compare(live.seen.state, liveOffered.map(_._1), shown, now.map(_._1)),
            Stitched.drift(liveOffered, now)
          )
      }
    }

  private def liveTags(tags: Tags): Live = tags match {
    case Tags.Weighed(kind, kindP, waiting, durable, helps, model, usage) =>
      Live.Weighed(kind, kindP, waiting, durable, helps, model, usage.costUsd)
    case Tags.Unanswered(why) => Live.Unanswered(Failure.of(why))
  }

  /** `e`'s kind: a store's error can quote what it was given, so its words are never kept. */
  private def kind(e: StoreError): String = e match {
    case StoreError.DuplicateId(_) => "duplicate id"
    case StoreError.DatabaseError(_) => "database error"
    case StoreError.Invalid(_) => "invalid"
  }

  private def each[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )
}
