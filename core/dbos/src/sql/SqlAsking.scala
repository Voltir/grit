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

  /** Why no transaction was opened for an asker. */
  enum Unasked {

    /** No person is linked to the account. */
    case NotVouched

    /** The database could not open the transaction, `cause` saying why. */
    case Unavailable(cause: String)
  }

  /** `body`, given the person `by` is linked to now and a transaction opened on `conn` through
    * `opener` for them in `room` ([[Subject.Person]]), the person read first under
    * [[Subject.Public]]. Whether `conn` is read-only, and whether it commits, is the caller's.
    */
  def as[A](conn: Connection, opener: Opener, by: Account, room: Place)(
      body: (Principal.Person, Tx^) => A
  ): Either[Unasked, A] =
    opener
      .open(Subject.Public, conn)
      .flatMap(tx => SqlIdentities.person(by)(using tx)) match {
      case Left(e) => Left(Unasked.Unavailable(e.toString))
      case Right(Some(person: Principal.Person)) =>
        opener.open(Subject.Person(person, room), conn) match {
          case Left(e) => Left(Unasked.Unavailable(e.toString))
          case Right(tx) => Right(body(person, tx))
        }
      case Right(_) => Left(Unasked.NotVouched)
    }
}
