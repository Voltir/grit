package grit.core.edge

import java.time.Instant

import grit.core.document.{DocText, Document, Placement}
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.Place
import grit.core.session.Session
import grit.core.store.StoreError
import grit.core.visibility.{Integrity, Label}

/** An [[EdgeSessions]] over the sessions and briefs `script` holds, for the accounts in
  * `vouched`. Held to [[EdgeSessionsContract]]; it reads no labels or rooms, so every session
  * and brief is every vouched account's, and what a session did lately is its record alone, with
  * no commits, edits or reads. While the script is failing, every call fails as a database that
  * cannot be read does. Every call is recorded in the script.
  */
final class ScriptedSessions(vouched: Set[Account], script: ScriptedSessions.Script)
    extends EdgeSessions {

  def record(
      by: Account,
      room: Place,
      batches: Vector[EdgeSessions.Batch]
  ): Either[StoreError, Int] =
    script.called(ScriptedSessions.Call.Recorded(by, room, batches)) match {
      case Some(cause) => Left(StoreError.DatabaseError(cause))
      case None =>
        person(by) match {
          case None => Left(StoreError.Invalid(s"no person is linked to ${Account.written(by)}"))
          case Some(p) =>
            batches
              .flatMap(b => EdgeSessions.refused(b.id))
              .headOption
              .toLeft(script.log(p, batches))
        }
    }

  def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]] =
    script.called(ScriptedSessions.Call.Read(room, id)) match {
      case Some(cause) => Left(StoreError.DatabaseError(cause))
      case None => Right(script.session(id).map(ScriptedSessions.lately))
    }

  def briefed(
      by: Account,
      room: Place,
      holder: ClaimHolder,
      besides: Option[SessionId],
      at: Instant
  ): Either[BriefError, EdgeSessions.Briefed] =
    script.called(ScriptedSessions.Call.Briefed(by, room, holder, besides, at)) match {
      case Some(cause) => Left(BriefError.Unavailable(cause))
      case None if person(by).isEmpty => Left(BriefError.NotVouched(by))
      case None =>
        Right(
          EdgeSessions.Briefed(
            script.briefOf(holder, at),
            script.newest(holder, besides).map(ScriptedSessions.lately)
          )
        )
    }

  private def person(by: Account): Option[Principal.Person] =
    Option.when(vouched.contains(by))(TestAccounts.principal(by)).collect {
      case p: Principal.Person => p
    }
}

object ScriptedSessions {

  /** One call, as it was made. */
  enum Call {
    case Recorded(by: Account, room: Place, batches: Vector[EdgeSessions.Batch])
    case Read(room: Place, id: SessionId)
    case Briefed(
        by: Account,
        room: Place,
        holder: ClaimHolder,
        besides: Option[SessionId],
        at: Instant
    )
  }

  private def lately(s: Session): Session.Lately =
    Session.Lately(s, Vector.empty, 0, Vector.empty, Vector.empty)

  /** A public brief holding `text`, kept at [[EdgeSessionsContract.Worktree]], written at
    * `written`.
    */
  def document(text: String, written: Instant): Document = {
    def got[A](e: Either[String, A]): A =
      e.fold(why => throw new java.lang.AssertionError(why), a => a)
    Document(
      got(DocumentVersionId.of(1).toRight("a version is numbered from 1")),
      got(PluginName.of("contracted")),
      got(DocKey.of("contract/brief")),
      Label.Public,
      Integrity.Member,
      EdgeSessionsContract.Worktree,
      got(DocText.of(text)),
      ujson.Obj(),
      None,
      written,
      Placement(0, written),
      None
    )
  }

  /** What a [[ScriptedSessions]] answers from, which a test writes and reads back; plain state, so
    * a suite may hold it in a field where it may not hold the sessions.
    */
  final class Script {

    // Each replaced whole by this script's own methods, under its lock; nothing else holds them.
    @caps.unsafe.untrackedCaptures
    private var sessions = Vector.empty[Session]
    @caps.unsafe.untrackedCaptures
    private var ordinals = Map.empty[SessionId, Set[Long]]
    @caps.unsafe.untrackedCaptures
    private var briefs = Vector.empty[(ClaimHolder, Document)]
    @caps.unsafe.untrackedCaptures
    private var calls = Vector.empty[Call]
    @caps.unsafe.untrackedCaptures
    private var failure = Option.empty[String]

    /** Every call so far, first first. */
    def called: Vector[Call] = synchronized(calls)

    /** Fails every call from now on, saying `cause`, or none when `None`. */
    def failing(cause: Option[String]): Unit = synchronized { failure = cause }

    /** Keeps `document` as `holder`'s brief. */
    def brief(holder: ClaimHolder, document: Document): Unit = synchronized {
      briefs = briefs :+ (holder -> document)
    }

    /** The session `id`, as recorded. */
    def session(id: SessionId): Option[Session] = synchronized(sessions.find(_.id == id))

    private[ScriptedSessions] def called(call: Call): Option[String] = synchronized {
      calls = calls :+ call
      failure
    }

    private[ScriptedSessions] def briefOf(holder: ClaimHolder, at: Instant): Option[Document] =
      synchronized {
        briefs.collect { case (h, d) if h == holder && !d.written.isAfter(at) => d }.lastOption
      }

    private[ScriptedSessions] def newest(
        holder: ClaimHolder,
        besides: Option[SessionId]
    ): Option[Session] = synchronized {
      sessions
        .filter(s => s.holder.contains(holder) && !besides.contains(s.id))
        .maxByOption(_.lastEvent)
    }

    /** Logs each batch's events new to its session, as `by`: how many. */
    private[ScriptedSessions] def log(
        by: Principal.Person,
        batches: Vector[EdgeSessions.Batch]
    ): Int = synchronized {
      batches.foldLeft(0) { (logged, b) =>
        val seen = ordinals.getOrElse(b.id, Set.empty)
        val fresh = b.events.filterNot(e => seen.contains(e.n)).distinctBy(_.n)
        if (fresh.isEmpty) logged
        else {
          ordinals = ordinals.updated(b.id, seen ++ fresh.map(_.n))
          sessions = sessions.find(_.id == b.id) match {
            case Some(s) =>
              sessions.map(o =>
                if (o.id != b.id) o
                else
                  o.copy(
                    lastEvent = (o.lastEvent +: fresh.map(_.at)).max,
                    counters = o.counters + Session.Counters.of(fresh),
                    holder = o.holder.orElse(b.holder)
                  )
              )
            case None =>
              sessions :+ Session(
                b.id,
                b.parent,
                by,
                Label.Public,
                b.at,
                fresh.map(_.at).min,
                None,
                fresh.map(_.at).max,
                None,
                Session.Counters.of(fresh),
                false,
                b.holder
              )
          }
          logged + fresh.size
        }
      }
    }
  }
}
