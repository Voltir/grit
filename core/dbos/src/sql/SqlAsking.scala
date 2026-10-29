package grit.dbos.sql

import java.sql.Connection

import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.Subject

/** A transaction opened for the person an account is linked to, in a room, as an edge runs what
  * that person asks: a command ([[SqlPluginCommands]]) or a watch.
  */
private[dbos] object SqlAsking {

  /** `body`, given the person `by` is linked to now and a transaction opened on `conn` through
    * `opener` for them in `room` ([[Subject.Person]]), the person read first under
    * [[Subject.Public]]; or `notVouched` when no person is linked to `by`, or `unavailable` of
    * why the database could not open the transaction. Whether `conn` is read-only, and whether
    * it commits, is the caller's.
    */
  def as[E, A](conn: Connection, opener: Opener, by: Account, room: Place)(
      notVouched: Account => E,
      unavailable: String => E
  )(body: (Principal.Person, Tx^) => Either[E, A]): Either[E, A] =
    opener
      .open(Subject.Public, conn)
      .flatMap(tx => SqlIdentities.person(by)(using tx)) match {
      case Left(e) => Left(unavailable(e.toString))
      case Right(Some(person: Principal.Person)) =>
        opener.open(Subject.Person(person, room), conn) match {
          case Left(e) => Left(unavailable(e.toString))
          case Right(tx) => body(person, tx)
        }
      case Right(_) => Left(notVouched(by))
    }
}
