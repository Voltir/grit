package grit.dbos.sql

import scala.util.Using

import grit.core.id.PrincipalIds
import grit.core.identity.{Account, Evidence, Handle, Held, Principal}
import grit.core.store.{StoreError, Tx}

/** Accounts as `grit.identities` keeps them (ADR 0032): each one an action came through, and the
  * principal it is linked to now.
  */
private[dbos] object SqlIdentities {
  import SqlEntryStore.attempt

  /** `account` as every column that names one holds it. */
  def written(account: Account): String = Account.written(account)

  /** The account the stored `text` spells; `Invalid`, saying why, when it spells none. */
  def read(text: String): Either[StoreError, Account] =
    Account.read(text).left.map(why => StoreError.Invalid(s"a stored account: $why"))

  /** Keeps each of `accounts`: one not seen before as the one account of a new person, enrolled,
    * whose id Postgres mints; nothing for one kept already. Of two transactions seeing an account
    * first at once, the second waits for the first to commit and finds its person. A transaction
    * that keeps several accounts keeps them in one call: every call waits on its accounts in one
    * order, so two never deadlock, where two calls each could.
    */
  def enroll(accounts: Set[Account])(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val spelled = accounts.toVector.map(written).sorted
    attempt {
      // The first sightings' locks, in the order of their keys: two transactions taking some of
      // the same keys take them in the same order, so neither holds one the other waits on
      // while it waits on one the other holds.
      val keys = Using.resource(
        conn.prepareStatement(
          """SELECT DISTINCT hashtext(a) AS k FROM jsonb_array_elements_text(?::jsonb) a
            | WHERE NOT EXISTS (SELECT 1 FROM grit.identities WHERE account = a)
            | ORDER BY k""".stripMargin
        )
      ) { ps =>
        // As JSON, so no Java array crosses JDBC (separation checking).
        ps.setString(1, ujson.Arr.from(spelled.map(ujson.Str(_))).render())
        Using.resource(ps.executeQuery()) { rs =>
          val keys = Vector.newBuilder[Int]
          while (rs.next()) keys += rs.getInt(1)
          keys.result()
        }
      }
      keys.foreach { key =>
        Using.resource(conn.prepareStatement("SELECT pg_advisory_xact_lock(?, ?)")) { ps =>
          ps.setInt(1, EnrolClass)
          ps.setInt(2, key)
          Using.resource(ps.executeQuery())(_.next())
        }
      }
      // Read committed: each statement sees a first sighting that committed while it waited.
      if (keys.nonEmpty) spelled.foreach { account =>
        Using.resource(
          conn.prepareStatement(
            """WITH home AS (
              |  INSERT INTO grit.principals (id, kind)
              |  SELECT uuidv7()::text, 'person'
              |   WHERE NOT EXISTS (SELECT 1 FROM grit.identities WHERE account = ?)
              |  RETURNING id)
              |INSERT INTO grit.identities (account, principal_id, evidence, home)
              |SELECT ?, id, 'enrolled', id FROM home""".stripMargin
          )
        ) { ps =>
          ps.setString(1, account)
          ps.setString(2, account)
          ps.executeUpdate()
        }
      }
      ()
    }
  }

  /** The principal `id`, of `kind` (`grit.principals.kind`), declared as `handle`, holding
    * `held`, a JSON array of `[account, evidence, member]` triples; `Invalid` when a stored
    * value is none of its kind's.
    */
  def principal(
      id: String,
      kind: String,
      handle: Option[String],
      held: String
  ): Either[StoreError, Principal] =
    kind match {
      case "grit" => Right(Principal.Grit)
      case "person" =>
        for {
          h <- handle match {
            case None => Right(None)
            case Some(text) =>
              Handle.of(text).map(Some(_)).left.map(why => StoreError.Invalid(s"a stored $why"))
          }
          accounts <- ujson
            .read(held)
            .arrOpt
            .fold(Vector.empty[ujson.Value])(_.toVector)
            .foldLeft[Either[StoreError, Set[Held]]](Right(Set.empty))((read, row) =>
              read.flatMap(so => this.held(row).map(so + _))
            )
        } yield Principal.Person(PrincipalIds.stored(id), h, accounts)
      case other => Left(StoreError.Invalid(s"a principal is a person or grit: $other"))
    }

  /** One `[account, evidence, member]` triple. */
  private def held(row: ujson.Value): Either[StoreError, Held] =
    row.arrOpt.map(_.toVector) match {
      case Some(Vector(ujson.Str(account), ujson.Str(evidence), ujson.Bool(member))) =>
        for {
          a <- read(account)
          e <- evidence match {
            case "enrolled" => Right(Evidence.Enrolled)
            case "declared" => Right(Evidence.Declared)
            case "vouched" => Right(Evidence.Vouched)
            case other => Left(StoreError.Invalid(s"a stored account's evidence: $other"))
          }
        } yield Held(a, e, member)
      case _ => Left(StoreError.Invalid(s"a stored account's link: ${row.render()}"))
    }

  /** The advisory-lock class a first sighting is locked under, with the hash of its account's
    * spelling: the bytes of `gids`. Two ints, so it never meets the engine's one bigint key, and
    * another class than an edge's.
    */
  private val EnrolClass: Int = 0x67696473
}
