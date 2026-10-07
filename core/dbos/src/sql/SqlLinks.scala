package grit.dbos.sql

import java.sql.{Connection, PreparedStatement}

import scala.util.Using

import grit.core.identity.{Account, Handle, Identities}
import grit.core.store.{StoreError, Tx}

/** Every move of an account from one person to another (ADR 0032): what a deployment's
  * declaration makes so.
  */
private[dbos] object SqlLinks {
  import SqlEntryStore.attempt

  /** Makes the stored people what `identities` declares:
    *   - a handle no longer declared is cleared, and each account declared before and not now
    *     goes back to a new person of its own, enrolled;
    *   - each declared person is found by its handle, or minted;
    *   - each declared account, enrolled first when not seen before, is linked to its person,
    *     declared, with no home. A person the move leaves with no account, no handle and no
    *     account whose home it is (the account's own home, or the declared person it was
    *     linked to) is merged into the declared person: the schedules, edges and tool requests
    *     naming it are rewritten to name the declared person, and it is deleted.
    *
    * Each move locks the people it touches, in id order, before it writes. A start under the
    * declaration the database already holds changes nothing.
    */
  def reconcile(identities: Identities)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: Connection^{tx} = Tx.connection(tx)
    val people = identities.people.sortBy(p => Handle.value(p.handle))
    val handles = people.map(p => Handle.value(p.handle))
    val accounts = people.flatMap(_.accounts).toSet
    for {
      _ <- attempt {
        update(conn)(
          """UPDATE grit.principals SET handle = NULL
            | WHERE handle IS NOT NULL
            |   AND handle NOT IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
        )(_.setString(1, json(handles)))
      }
      _ <- attempt {
        query(conn)(
          """SELECT account, principal_id FROM grit.identities
            | WHERE evidence = 'declared' AND account NOT IN ('local', 'grit')
            |   AND account NOT IN (SELECT jsonb_array_elements_text(?::jsonb))
            | ORDER BY account""".stripMargin
        )(_.setString(1, json(accounts.toVector.map(Account.written))))(rs =>
          (rs.getString(1), rs.getString(2))
        ).foreach((account, from) => undeclare(conn, account, from))
      }
      _ <- SqlIdentities.enroll(accounts)
      _ <- each(people) { person =>
        val handle = Handle.value(person.handle)
        attempt(principal(conn, handle)).flatMap {
          case None => Left(StoreError.Invalid(s"the person declared as $handle was not kept"))
          case Some(to) =>
            each(person.accounts.toVector.map(Account.written).sorted) { account =>
              attempt(declare(conn, account, to)).flatMap(
                if (_) Right(()) else Left(StoreError.Invalid(s"$account was enrolled, not kept"))
              )
            }
        }
      }
    } yield ()
  }

  /** `f` of each of `as` in turn, up to the first that fails. */
  private def each[A](as: Vector[A])(f: A => Either[StoreError, Unit]): Either[StoreError, Unit] =
    as.foldLeft[Either[StoreError, Unit]](Right(()))((done, a) => done.flatMap(_ => f(a)))

  /** `account`, declared and now not, moved from `from` to a new person of its own. */
  private def undeclare(conn: Connection^, account: String, from: String): Unit = {
    lock(conn, Vector(from))
    update(conn)(
      """WITH home AS (
        |  INSERT INTO grit.principals (id, kind) VALUES (uuidv7()::text, 'person') RETURNING id)
        |UPDATE grit.identities SET principal_id = home.id, home = home.id,
        |       evidence = 'enrolled', linked_at = now()
        |  FROM home WHERE account = ?""".stripMargin
    )(_.setString(1, account))
  }

  /** The person declared as `handle`: found, or minted; `None` when neither. */
  private def principal(conn: Connection^, handle: String): Option[String] = {
    update(conn)(
      """INSERT INTO grit.principals (id, kind, handle) VALUES (uuidv7()::text, 'person', ?)
        |ON CONFLICT (handle) DO NOTHING""".stripMargin
    )(_.setString(1, handle))
    query(conn)("SELECT id FROM grit.principals WHERE handle = ?")(_.setString(1, handle))(
      _.getString(1)
    ).headOption
  }

  /** `account` linked to `to`, declared: a declaration's merge; false when it is not kept. */
  private def declare(conn: Connection^, account: String, to: String): Boolean =
    query(conn)(
      "SELECT principal_id, evidence, home FROM grit.identities WHERE account = ?"
    )(_.setString(1, account))(rs =>
      (rs.getString(1), rs.getString(2), Option(rs.getString(3)))
    ).headOption match {
      case None => false
      case Some((`to`, "declared", _)) => true
      case Some((from, _, home)) =>
        val left = (Vector(from) ++ home).filter(_ != to).distinct
        lock(conn, left :+ to)
        update(conn)(
          """UPDATE grit.identities SET principal_id = ?, evidence = 'declared', home = NULL,
            |       linked_at = now()
            | WHERE account = ?""".stripMargin
        ) { ps =>
          ps.setString(1, to)
          ps.setString(2, account)
        }
        left.foreach(merge(conn, _, to))
        true
    }

  /** `from` merged into `to` when nothing keeps it a person: no account linked to it or at home
    * there, and no handle; else nothing.
    */
  private def merge(conn: Connection^, from: String, to: String): Unit = {
    val emptied = query(conn)(
      """SELECT 1 FROM grit.principals p
        | WHERE p.id = ? AND p.handle IS NULL AND p.id NOT IN ('local', 'grit')
        |   AND NOT EXISTS (SELECT 1 FROM grit.identities i
        |                    WHERE i.principal_id = p.id OR i.home = p.id)""".stripMargin
    )(_.setString(1, from))(_ => ()).nonEmpty
    if (emptied) {
      Vector("grit.schedules", "grit.edges", "grit.tool_requests").foreach { table =>
        update(conn)(s"UPDATE $table SET principal = ? WHERE principal = ?") { ps =>
          ps.setString(1, to)
          ps.setString(2, from)
        }
      }
      update(conn)("DELETE FROM grit.principals WHERE id = ?")(_.setString(1, from))
    }
  }

  /** Locks `ids`' rows for update, in id order, so two moves never wait on each other's. */
  private def lock(conn: Connection^, ids: Vector[String]): Unit =
    query(conn)(
      """SELECT id FROM grit.principals WHERE id IN (SELECT jsonb_array_elements_text(?::jsonb))
        | ORDER BY id FOR UPDATE""".stripMargin
    )(_.setString(1, json(ids)))(_ => ()).foreach(identity)

  /** `texts` as a JSON array: no Java array crosses JDBC (separation checking). */
  private def json(texts: Vector[String]): String = ujson.Arr.from(texts.map(ujson.Str(_))).render()

  private def update(conn: Connection^)(sql: String)(bind: PreparedStatement => Unit): Unit =
    Using.resource(conn.prepareStatement(sql)) { ps =>
      bind(ps)
      val _ = ps.executeUpdate()
    }

  private def query[A](conn: Connection^)(sql: String)(bind: PreparedStatement => Unit)(
      row: java.sql.ResultSet => A
  ): Vector[A] =
    Using.resource(conn.prepareStatement(sql)) { ps =>
      bind(ps)
      Using.resource(ps.executeQuery()) { rs =>
        val out = Vector.newBuilder[A]
        while (rs.next()) out += row(rs)
        out.result()
      }
    }
}
