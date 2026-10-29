package grit.dbos.sql

import grit.core.edge.EdgeSessions
import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.session.Session
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.Subject

/** [[EdgeSessions]] over [[SqlSessions]], each call one transaction written through `jot`. */
private[dbos] final class SqlEdgeSessions(jot: Jot) extends EdgeSessions {

  private val sessions = new SqlSessions

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
                .map(logged + _.logged)
            }
          }
        case _ => Left(StoreError.Invalid(s"no person is linked to ${Account.written(by)}"))
      }
    }
}
