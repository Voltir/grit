package grit.dbos.sql

import java.time.Instant
import javax.sql.DataSource

import grit.core.edge.{BriefError, EdgeSessions}
import grit.core.id.{ClaimHolder, SessionId}
import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.session.Session
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.Subject

/** [[EdgeSessions]] over [[SqlSessions]], each call one transaction: a record and a session read
  * written through `jot`; a brief read on `dataSource`, opened through `opener` as its asker
  * ([[SqlAsking.read]]).
  */
private[dbos] final class SqlEdgeSessions(jot: Jot, dataSource: DataSource, opener: Opener)
    extends EdgeSessions {

  def briefed(
      by: Account,
      room: Place,
      holder: ClaimHolder,
      besides: Option[SessionId],
      at: Instant
  ): Either[BriefError, EdgeSessions.Briefed] =
    SqlAsking.read(dataSource, opener, by, room)(
      BriefError.NotVouched(_),
      BriefError.Unavailable(_)
    ) { (_, tx) =>
      (for {
        brief <- search.briefOf(holder, at)(using tx)
        newest <- SqlSessions.newest(holder, besides)(using tx)
        lately <- newest.fold(Right(None))(sessions.lately(_)(using tx))
      } yield EdgeSessions.Briefed(brief, lately)).left.map(e => BriefError.Unavailable(e.toString))
    }

  private val search = new SqlDocumentSearch

  private val sessions = new SqlSessions

  /** Whether `batch`'s session, read back once recorded, was begun by `person`: read after, not
    * before, since recording waits on any other recording of that session, so of two first
    * records the read sees whichever won. A batch of no events begins no session.
    */
  private def begunBy(batch: EdgeSessions.Batch, person: Principal.Person)(using
      Tx^
  ): Either[StoreError, Unit] =
    sessions.read(batch.id).flatMap {
      case Some(s) if s.by == person => Right(())
      case None if batch.events.isEmpty => Right(())
      case _ => Left(EdgeSessions.notTheirs(batch.id))
    }

  // Jot opens read-write transactions alone; this one only reads.
  def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]] =
    jot.write(Subject.Room(room))(sessions.lately(id))

  def record(
      by: Account,
      room: Place,
      batches: Vector[EdgeSessions.Batch]
  ): Either[StoreError, Int] =
    jot.write(Subject.Room(room)) { (tx: Tx^) ?=>
      SqlIdentities.person(by).flatMap {
        case Some(person: Principal.Person) =>
          batches.foldLeft[Either[StoreError, Int]](Right(0)) { (sum, batch) =>
            sum.flatMap { logged =>
              EdgeSessions
                .refused(batch.id)
                .toLeft(())
                .flatMap(_ =>
                  sessions
                    .record(
                      Session.Header(
                        batch.id,
                        batch.parent,
                        person,
                        Tx.floor(tx),
                        batch.at,
                        batch.holder
                      ),
                      batch.events
                    )
                    // The session store's one refusal of a header is a session kept where the
                    // transaction does not read, told as one begun by someone else.
                    .left
                    .map {
                      case StoreError.Invalid(_) => EdgeSessions.notTheirs(batch.id)
                      case other => other
                    }
                    .flatMap(recorded => begunBy(batch, person).map(_ => logged + recorded.logged))
                )
            }
          }
        case _ => Left(StoreError.Invalid(s"no person is linked to ${Account.written(by)}"))
      }
    }
}
