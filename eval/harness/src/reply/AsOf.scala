package grit.eval.harness.reply

import java.time.Instant

import grit.core.document.{Document, DocumentSearch, DocumentTerms, Shelved}
import grit.core.id.{
  CallSlot,
  CloseRef,
  ConversationId,
  DocumentVersion,
  EntryId,
  EntrySeq,
  PeriodRef,
  PluginName,
  PrincipalId,
  TurnRef,
  TurnSeq
}
import grit.core.period.{
  Activity,
  CloseOrdinal,
  CloseReason,
  Closing,
  LifecycleSettings,
  Period,
  PeriodState,
  Verdict
}
import grit.core.place.Place
import grit.core.stitch.{Link, Placed, Said, StitchStore}
import grit.core.store.{
  ClosedElsewhere,
  ClosedPeriod,
  ClosingEntry,
  Conversation,
  ConversationStore,
  Entry,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  OpenActivity,
  OpenPeriod,
  Origin,
  PeriodStore,
  Principals,
  Sealed,
  StoreError,
  Tx
}
import grit.dbos.engine.Reader

/** A restored database's stores as they stood at `at`, for an assembler to read a window from
  * as it would have then. Each read returns only the rows created before `at`: entries (and
  * closing entries) by when they were written, conversations by when they began, a strand's
  * link by when its follower began (a link is kept a moment after that), and documents by
  * when each version was written, read as current at a moment no later than `at`. A search
  * ranks those rows alone, best first and at most as many as asked, though each hit keeps the
  * score the database's index gives it now, so a rank among them can differ from the one live
  * gave when later rows moved the index's statistics. The periods open elsewhere are those
  * opened before `at` and not closed by it; the closing kept elsewhere is each conversation's
  * newest closed before `at`; every other read of periods, the lifecycle settings, who is
  * named and the documents' terms are as they stand now. What a purge or a document's
  * retention removed is gone. A write is the reader's, which Postgres refuses.
  */
final class AsOf private (
    val at: Instant,
    val entries: EntryStore,
    val conversations: ConversationStore,
    val periods: PeriodStore,
    val principals: Principals,
    val lifecycle: LifecycleStore,
    val search: EntrySearch,
    val documents: DocumentSearch,
    val stitches: StitchStore
) {

  /** These stores, but for the lifecycle settings, read as `settings` whatever the database
    * holds.
    */
  def settled(settings: LifecycleSettings): AsOf =
    new AsOf(
      at,
      entries,
      conversations,
      periods,
      principals,
      new AsOf.Settled(lifecycle, settings),
      search,
      documents,
      stitches
    )
}

object AsOf {

  /** `reader`'s stores as they stood at `at`. */
  def apply(reader: Reader^, at: Instant): AsOf = {
    val entries = new Entries(reader.entries, at)
    val conversations = new Conversations(reader.conversations, at)
    new AsOf(
      at,
      entries,
      conversations,
      new Periods(reader.periods, reader.conversations, at),
      reader.principals,
      reader.lifecycle,
      new Search(reader.search, reader.entries, at),
      new Documents(reader.documents, at),
      new Stitches(reader.stitches, reader.conversations, at)
    )
  }

  /** The most closed periods asked for at once, paging through them in close order. */
  private val Page = 500

  private def before(t: Instant, at: Instant): Boolean = t.isBefore(at)

  private final class Settled(under: LifecycleStore, settings: LifecycleSettings)
      extends LifecycleStore {
    def current()(using Tx^): Either[StoreError, LifecycleSettings] = Right(settings)
    def set(settings: LifecycleSettings)(using Tx^): Either[StoreError, Unit] = under.set(settings)
  }

  private final class Entries(under: EntryStore, at: Instant) extends EntryStore {
    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = under.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
      under.get(id).map(_.filter(e => before(e.createdAt, at)))
    def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
      under.list(conversation).map(_.filter(e => before(e.createdAt, at)))
    def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] =
      under.at(conversation, seqs).map(_.filter(e => before(e.createdAt, this.at)))
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      under.ofTurn(turn).map(_.filter(e => before(e.createdAt, at)))
    def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      under.lockNext(conversation)
  }

  private final class Conversations(under: ConversationStore, at: Instant)
      extends ConversationStore {
    def findOrCreate(origin: Origin, by: PrincipalId)(using
        Tx^
    ): Either[StoreError, Conversation] = under.findOrCreate(origin, by)
    def find(origin: Origin)(using Tx^): Either[StoreError, Option[Conversation]] =
      under.find(origin).map(_.filter(c => before(c.createdAt, at)))
    def get(id: ConversationId)(using Tx^): Either[StoreError, Option[Conversation]] =
      under.get(id).map(_.filter(c => before(c.createdAt, at)))
    def postedBy(conversation: ConversationId)(using Tx^): Either[StoreError, Option[CallSlot]] =
      get(conversation).flatMap(_.fold(Right(None))(_ => under.postedBy(conversation)))
    def remove(conversation: ConversationId)(using Tx^): Either[StoreError, Unit] =
      under.remove(conversation)
  }

  private final class Periods(under: PeriodStore, conversations: ConversationStore, at: Instant)
      extends PeriodStore {
    def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using
        Tx^
    ): Either[StoreError, Period] = under.openFor(conversation, turn, at)
    def get(period: PeriodRef)(using Tx^): Either[StoreError, Option[Period]] = under.get(period)
    def all(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Period]] =
      under.all(conversation)
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Period]] = under.of(turn)
    def judged(period: PeriodRef, verdict: Verdict)(using Tx^): Either[StoreError, Boolean] =
      under.judged(period, verdict)
    def open()(using Tx^): Either[StoreError, Vector[OpenActivity]] = under.open()
    def activity(period: PeriodRef)(using Tx^): Either[StoreError, Option[Activity]] =
      under.activity(period)
    def seal(attempt: CloseRef, reason: CloseReason, closing: Closing, at: Instant)(using
        Tx^
    ): Either[StoreError, Sealed] = under.seal(attempt, reason, closing, at)
    def closingBefore(turn: TurnRef)(using Tx^): Either[StoreError, Option[ClosingEntry]] =
      under.closingBefore(turn).map(_.filter(c => before(c.entry.createdAt, at)))
    def closedAfter(after: CloseOrdinal, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[ClosedPeriod]] = under.closedAfter(after, n)
    def purge(period: PeriodRef, at: Instant)(using Tx^): Either[StoreError, Unit] =
      under.purge(period, at)
    def drop(period: PeriodRef)(using Tx^): Either[StoreError, Boolean] = under.drop(period)

    /** Each other conversation's period open at `at`: opened before it and not closed by it,
      * whether open now or closed since; oldest opened first, then by conversation.
      */
    def openElsewhere(conversation: ConversationId)(using
        Tx^
    ): Either[StoreError, Vector[OpenPeriod]] =
      for {
        now <- under.openElsewhere(conversation)
        since <- closedSince(CloseOrdinal.Start, Vector.empty)
        candidates = (now.map(_.conversation) ++ since.map(_.ref.conversationId)).distinct
          .filterNot(_ == conversation)
        open <- each(candidates) { c =>
          for {
            began <- conversations.get(c)
            kept <- under.all(c)
          } yield
            for {
              started <- began
              period <- kept
                .filter(p => before(p.openedAt, at) && openAt(p))
                .maxByOption(p => grit.core.id.PeriodSeq.value(p.ref.seq))
            } yield (period.openedAt, OpenPeriod(c, started.origin.place, period.first))
        }
      } yield open.flatten
        .sortBy((opened, o) => (opened, ConversationId.value(o.conversation)))
        .map(_._2)

    /** Each other conversation with a closing kept from before `at`: its newest period closed
      * before `at`, closed earned; in the close order of that closing.
      */
    def closedElsewhere(conversation: ConversationId)(using
        Tx^
    ): Either[StoreError, Vector[ClosedElsewhere]] =
      for {
        now <- under.closedElsewhere(conversation)
        kept <- each(now) { c =>
          under.all(c.conversation).map { periods =>
            periods
              .flatMap(p =>
                p.state match {
                  case PeriodState.Closed(_, closedAt, reason, closing, order, _)
                      if before(closedAt, at) && reason.shownElsewhere =>
                    Some((grit.core.id.PeriodSeq.value(p.ref.seq), order, closing))
                  case _ => None
                }
              )
              .maxByOption(_._1)
              .map((_, order, closing) =>
                (order, ClosedElsewhere(c.conversation, c.place, closing))
              )
          }
        }
      } yield kept.flatten.sortBy((order, _) => CloseOrdinal.value(order)).map(_._2)

    private def openAt(p: Period): Boolean = p.state match {
      case PeriodState.Open => true
      case PeriodState.Closed(_, closedAt, _, _, _, _) => !before(closedAt, at)
    }

    /** Every period closed at or after `at`, in close order, from `after` on. */
    private def closedSince(after: CloseOrdinal, done: Vector[ClosedPeriod])(using
        Tx^
    ): Either[StoreError, Vector[ClosedPeriod]] =
      under.closedAfter(after, Page).flatMap { page =>
        val kept = done ++ page.filterNot(c => before(c.at, at))
        page.lastOption match {
          case Some(last) if page.size == Page => closedSince(last.order, kept)
          case _ => Right(kept)
        }
      }
  }

  private final class Search(under: EntrySearch, entries: EntryStore, at: Instant)
      extends EntrySearch {
    def search(
        conversation: ConversationId,
        from: TurnSeq,
        before: TurnSeq,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[EntrySearch.Hit]] =
      under.search(conversation, from, before, query, Int.MaxValue).flatMap(kept(_, limit))
    def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] =
      under.nearby(open, query, Int.MaxValue).flatMap(kept(_, limit))
    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] =
      under.closings(conversations, query, Int.MaxValue).flatMap(kept(_, limit))
    def room(room: Place, from: Instant, until: Instant, query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] =
      under.room(room, from, until, query, Int.MaxValue).flatMap(kept(_, limit))

    /** The first `limit` of `hits`, in their order, whose entries were written before `at`. */
    private def kept(hits: Vector[EntrySearch.Hit], limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] =
      hits.foldLeft[Either[StoreError, Vector[EntrySearch.Hit]]](Right(Vector.empty)) { (acc, h) =>
        acc.flatMap(done =>
          if (done.size >= limit) Right(done)
          else
            entries
              .get(h.id)
              .map(e => if (e.exists(x => AsOf.before(x.createdAt, at))) done :+ h else done)
        )
      }
  }

  /** `under`'s documents as they stood at `at`: one asked for as of a later moment is read
    * as of `at`, and a version written at or after `at` is never read.
    */
  private[reply] final class Documents(under: DocumentSearch, at: Instant) extends DocumentSearch {
    def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
      under.declared()
    def shelved(plugins: Vector[PluginName], at: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Shelved]] = under.shelved(plugins, earlier(at))
    def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
        Tx^
    ): Either[StoreError, Vector[DocumentSearch.Hit]] =
      under.search(shelves, query, limit, earlier(at))
    def read(versions: Vector[DocumentVersion])(using Tx^): Either[StoreError, Vector[Document]] =
      under.read(versions).map(_.filter(d => before(d.written, at)))

    private def earlier(until: Instant): Instant = if (until.isBefore(at)) until else at
  }

  private final class Stitches(under: StitchStore, conversations: ConversationStore, at: Instant)
      extends StitchStore {
    def record(root: EntryId, placed: Placed, at: Instant)(using
        Tx^
    ): Either[StoreError, Boolean] = under.record(root, placed, at)
    def placed(root: EntryId)(using Tx^): Either[StoreError, Option[Placed]] = under.placed(root)
    def spokenIn(room: Place, from: Instant, until: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Said]] = under.spokenIn(room, from, earlier(until))
    def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Said]] = under.said(conversations, from, earlier(until))
    def openings(conversations: Vector[ConversationId])(using
        Tx^
    ): Either[StoreError, Vector[Said]] =
      under.openings(conversations).map(_.filter(s => before(s.entry.createdAt, at)))
    def links(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Link]] =
      under
        .links(conversations)
        .flatMap(links =>
          each(links)(l =>
            this.conversations
              .get(l.conversation)
              .map(_.filter(c => before(c.createdAt, at)).map(_ => l))
          ).map(_.flatten)
        )

    private def earlier(until: Instant): Instant = if (until.isBefore(at)) until else at
  }

  private def each[A, B](
      as: Vector[A]
  )(f: A => Either[StoreError, B]): Either[StoreError, Vector[B]] =
    as.foldLeft[Either[StoreError, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )
}
