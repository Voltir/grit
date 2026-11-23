package grit.core.edge

import java.time.Instant

import grit.core.claim.{Claim, ClaimRole, ClaimSessions}
import grit.core.document.{BriefClaim, DocText, Document, Placement, Takeover}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.place.Place
import grit.core.session.{Event, Session}
import grit.core.store.StoreError
import grit.core.visibility.{Integrity, Label}

/** An [[EdgeSessions]] over the sessions and briefs `script` holds, for the accounts in
  * `vouched`. Held to [[EdgeSessionsContract]]; it reads no labels or rooms, so every session
  * and brief is every vouched account's, and what a session did lately is its record alone (its
  * summary the newest closing it logged, its counters its events'), with no commits, edits or
  * reads; a batch moves claims as [[Session.claiming]] says, by the holder its session kept, and a
  * claim it holds lapses as [[Script.claim]] says. While the script is failing, every call fails as a database that
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
              .flatMap(b =>
                EdgeSessions
                  .refused(b.id)
                  .orElse(
                    Option.when(script.session(b.id).exists(_.by != p))(
                      EdgeSessions.notTheirs(b.id)
                    )
                  )
              )
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
      own: SessionId,
      at: Instant
  ): Either[BriefError, EdgeSessions.Briefed] =
    script.called(ScriptedSessions.Call.Briefed(by, room, holder, own, at)) match {
      case Some(cause) => Left(BriefError.Unavailable(cause))
      case None if person(by).isEmpty => Left(BriefError.NotVouched(by))
      case None =>
        Right(
          EdgeSessions.Briefed(
            script.briefOf(holder, at).fold(EdgeSessions.Briefed.Brief.Absent) { d =>
              val about = BriefClaim.of(d)
              about
                .flatMap(b => script.claimOn(b.subject, holder, at))
                .filter(_.session.exists(_ != own))
                .fold(EdgeSessions.Briefed.Brief.Kept(d))(
                  EdgeSessions.Briefed.Brief.Worked(_, about.flatMap(_.takeover))
                )
            },
            script.session(own).map(ScriptedSessions.lately)
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
        own: SessionId,
        at: Instant
    )
  }

  /** What `s` did lately, as the fake answers it: its record alone, no commits, edits or reads. */
  def lately(s: Session): Session.Lately =
    Session.Lately(s, Vector.empty, 0, Vector.empty, Vector.empty)

  /** A public brief holding `text`, kept at [[EdgeSessionsContract.Worktree]], written at
    * `written`.
    */
  def document(text: String, written: Instant): Document = {
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
    private var claims = Vector.empty[Claim]
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

    /** Keeps `claim`, live a lease after its [[Claim.active]] as this script's sessions answer it
      * ([[ClaimSessions.active]]).
      */
    private def claim(claim: Claim): Unit = synchronized { claims = claims :+ claim }

    /** Keeps `document` as `holder`'s brief, its data naming the claim on `subject` and
      * `takeover` ([[BriefClaim]]), and that claim: `holder`'s as `role`, made by a person when
      * `document` was written, worked by `session`.
      */
    def claimedBrief(
        holder: ClaimHolder,
        document: Document,
        subject: DocKey,
        takeover: Option[Takeover],
        role: ClaimRole,
        session: Option[SessionId]
    ): Unit = {
      brief(
        holder,
        document.copy(data = BriefClaim.kept(ujson.Obj(), BriefClaim(subject, takeover)))
      )
      claim(
        Claim(
          subject,
          holder,
          role,
          new Principal.Person(
            TestAccounts.principalId(TestAccounts.account("slack:T1/U-ann")),
            Set.empty
          ),
          document.written,
          Label.Public,
          session,
          document.written
        )
      )
    }

    /** `holder`'s claim on `subject` live at `at`, its `active` as [[claim]] reads it. */
    private[ScriptedSessions] def claimOn(
        subject: DocKey,
        holder: ClaimHolder,
        at: Instant
    ): Option[Claim] = synchronized {
      claims
        .findLast(c => c.subject == subject && c.holder == holder)
        .map { c =>
          val working = sessions
            .map(s =>
              s.id -> ClaimSessions
                .Working(s.holder, s.lastEvent, s.parent, s.ended.isDefined, s.by)
            )
            .toMap
          c.copy(active = ClaimSessions.active(c, working))
        }
        .filter(_.lapses.isAfter(at))
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

    /** Logs each batch's events new to its session, as `by`: how many, its claims moved as
      * [[ScriptedSessions]] says. A batch with none begins no session, but gives one with no
      * holder the batch's, as grit.dbos' sessions do.
      */
    private[ScriptedSessions] def log(
        by: Principal.Person,
        batches: Vector[EdgeSessions.Batch]
    ): Int = synchronized {
      batches.foldLeft(0) { (logged, b) =>
        val seen = ordinals.getOrElse(b.id, Set.empty)
        val fresh = b.events.filterNot(e => seen.contains(e.n)).distinctBy(_.n)
        if (fresh.isEmpty) {
          if (b.events.nonEmpty)
            sessions =
              sessions.map(o => if (o.id == b.id) o.copy(holder = o.holder.orElse(b.holder)) else o)
          logged
        } else {
          ordinals = ordinals.updated(b.id, seen ++ fresh.map(_.n))
          val said = fresh.sortBy(_.n).collect { case Event(_, _, Event.Kind.Closing(s), _) => s }
          val before = sessions.find(_.id == b.id)
          val ending = Session.ending(before.flatMap(_.ended), fresh.sortBy(_.n))
          val holder = before.flatMap(_.holder).orElse(b.holder)
          claims = Session.claiming(seen.size, before.flatMap(_.ended), fresh.sortBy(_.n)) match {
            case Session.Claiming.Frees =>
              claims.map(c => if (c.session.contains(b.id)) c.copy(session = None) else c)
            case Session.Claiming.Takes =>
              claims.map(c =>
                if (holder.contains(c.holder) && c.session.isEmpty) c.copy(session = Some(b.id))
                else c
              )
            case Session.Claiming.Leaves => claims
          }
          sessions = sessions.find(_.id == b.id) match {
            case Some(_) =>
              sessions.map(o =>
                if (o.id != b.id) o
                else
                  o.copy(
                    lastEvent = (o.lastEvent +: fresh.map(_.at)).max,
                    summary = said.lastOption.orElse(o.summary),
                    counters = o.counters + Session.Counters.of(fresh),
                    holder = o.holder.orElse(b.holder),
                    ended = ending
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
                ending,
                fresh.map(_.at).max,
                said.lastOption,
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
