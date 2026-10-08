package grit.dbos.sql

import java.sql.PreparedStatement

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.id.PrincipalIds
import grit.core.identity.{
  Account,
  Domain,
  Email,
  Evidence,
  Held,
  Principal,
  Realm,
  Standing,
  Vouched
}
import grit.core.store.{LastWord, Linking, StoreError, Tx, Voucher}

/** [[Voucher]] over `grit.attestations`, `grit.emails` and `grit.identities` (ADR 0032), for
  * `realms`, keeping an email only in one of `domains`, and saying each change's clearances as
  * the transaction it runs in clears people ([[Tx.clearanceOf]]). A vouching that keeps an
  * email takes that email's lock before it touches the account's row, always, so two vouchings
  * never wait on each other in opposite orders; the row is one upsert, which a renewal leaves a heap-only update (`seen_at` is
  * unindexed).
  */
private[dbos] final class SqlVoucher(
    val realms: Set[Realm],
    domains: Set[Domain]
) extends Voucher {
  import SqlEntryStore.attempt
  import SqlVoucher.*

  def vouch(vouched: Vouched)(using tx: Tx^): Either[StoreError, Vector[Linking]] = {
    val account = vouched.account
    if (!realms.exists(_.holds(account))) Right(Vector(Linking.Outside(account)))
    else {
      val (kept, member, unclaimed) = vouched.standing match {
        case Standing.Full(Some(e)) if domains.contains(Email.domain(e)) => (Some(e), true, false)
        case Standing.Full(Some(_)) => (None, true, true)
        case Standing.Full(None) => (None, true, false)
        case Standing.Outside => (None, false, false)
      }
      for {
        _ <- kept.fold[Either[StoreError, Unit]](Right(()))(personFor)
        _ <- SqlIdentities.enroll(Set(account))
        row <- recorded(account, kept, member)
        said <-
          if (row.old.contains(row.now)) Right(Vector.empty)
          else changes(account, row).map(c => if (unclaimed) c :+ Linking.Unclaimed(account) else c)
      } yield said
    }
  }

  def lastWord(account: Account)(using tx: Tx^): Either[StoreError, LastWord] =
    many(
      s"SELECT $Ago FROM grit.attestations WHERE account = ?"
    )(_.setString(1, SqlIdentities.written(account)))(rs => rs.getLong(1))
      .map(ages => LastWord(account, ages.headOption.map(_.micros)))

  def lastWords(realm: Realm)(using tx: Tx^): Either[StoreError, Vector[LastWord]] =
    many(
      s"""SELECT i.account, grit.attestations.account IS NOT NULL AS said, coalesce($Ago, 0) AS ago
         |  FROM grit.identities i
         |  LEFT JOIN grit.attestations ON grit.attestations.account = i.account
         | WHERE i.account LIKE ? ESCAPE '\\'""".stripMargin
    )(_.setString(1, s"${escaped(s"${realm.namespace}:${realm.within}/")}%"))(rs =>
      (rs.getString(1), Option.when(rs.getBoolean(2))(rs.getLong(3).micros))
    ).flatMap(rows =>
      rows.foldLeft[Either[StoreError, Vector[LastWord]]](Right(Vector.empty)) {
        case (read, (text, ago)) =>
          read.flatMap(so => SqlIdentities.read(text).map(a => so :+ LastWord(a, ago)))
      }
    )

  /** Ends every attestation this voucher would not make now: of an account none of [[realms]]
    * holds, or holding an email in none of its domains. Each is kept as no full member, with no
    * email, answered when it was; one already so is not written. Each change, in the accounts'
    * order, is a [[Linking]] as [[vouch]] reports it.
    */
  def untrust(using tx: Tx^): Either[StoreError, Vector[Linking]] =
    many(
      """SELECT account, email FROM grit.attestations
        | WHERE member OR email IS NOT NULL ORDER BY account""".stripMargin
    )(_ => ())(rs => (rs.getString(1), Option(rs.getString(2))))
      .flatMap(live =>
        live.foldLeft[Either[StoreError, Vector[Linking]]](Right(Vector.empty)) {
          case (so, (text, stored)) =>
            so.flatMap { said =>
              SqlIdentities.read(text).flatMap { account =>
                val claimed = stored.forall(e =>
                  Email.of(e).toOption.exists(kept => domains.contains(Email.domain(kept)))
                )
                if (realms.exists(_.holds(account)) && claimed) Right(said)
                else ended(account).map(said ++ _)
              }
            }
        }
      )

  /** `account`'s attestation made no full member with no email, unless it is already. */
  private def ended(account: Account)(using tx: Tx^): Either[StoreError, Vector[Linking]] =
    many(
      """UPDATE grit.attestations SET email = NULL, member = false
        | WHERE account = ? AND (member OR email IS NOT NULL)
        |RETURNING old.email, old.member, new.email, new.member""".stripMargin
    )(_.setString(1, SqlIdentities.written(account)))(rs =>
      Row(
        Some(Said(Option(rs.getString(1)), rs.getBoolean(2))),
        Said(Option(rs.getString(3)), rs.getBoolean(4))
      )
    ).flatMap(_.headOption.fold(Right(Vector.empty))(changes(account, _)))

  /** `e`'s person, minted when no realm has attested `e` before, under `e`'s lock, which the
    * transaction holds to its end: of two first attestations at once, the second waits for the
    * first to commit and finds its person.
    */
  private def personFor(e: Email)(using tx: Tx^): Either[StoreError, Unit] =
    for {
      _ <- many("SELECT pg_advisory_xact_lock(?, hashtext(?))") { ps =>
        ps.setInt(1, EmailClass)
        ps.setString(2, Email.value(e))
      }(_ => ()).map(_ => ())
      // Read committed: this statement sees a first attestation that committed while it waited.
      _ <- update(
        """WITH person AS (
          |  INSERT INTO grit.principals (id, kind)
          |  SELECT uuidv7()::text, 'person'
          |   WHERE NOT EXISTS (SELECT 1 FROM grit.emails WHERE email = ?)
          |  RETURNING id)
          |INSERT INTO grit.emails (email, principal_id) SELECT ?, id FROM person""".stripMargin
      ) { ps =>
        ps.setString(1, Email.value(e))
        ps.setString(2, Email.value(e))
      }
    } yield ()

  /** `account`'s attestation made `kept` and `member`, answered now; what it held before, if
    * anything, and what it holds now.
    */
  private def recorded(account: Account, kept: Option[Email], member: Boolean)(using
      tx: Tx^
  ): Either[StoreError, Row] =
    many(
      """INSERT INTO grit.attestations (account, email, member, seen_at)
        |VALUES (?, ?, ?, now())
        |ON CONFLICT (account) DO UPDATE
        |   SET email = EXCLUDED.email, member = EXCLUDED.member, seen_at = EXCLUDED.seen_at
        |RETURNING old.member IS NOT NULL AS had, old.email, coalesce(old.member, false),
        |          new.email, new.member""".stripMargin
    ) { ps =>
      ps.setString(1, SqlIdentities.written(account))
      kept match {
        case Some(e) => ps.setString(2, Email.value(e))
        case None => ps.setNull(2, java.sql.Types.VARCHAR)
      }
      ps.setBoolean(3, member)
    }(rs =>
      Row(
        Option.when(rs.getBoolean(1))(Said(Option(rs.getString(2)), rs.getBoolean(3))),
        Said(Option(rs.getString(4)), rs.getBoolean(5))
      )
    ).flatMap(_.headOption.toRight(StoreError.Invalid("an attestation's upsert returned no row")))

  /** What `row`'s change did to `account`: whom it moved from and to, and its membership, each
    * with the clearance writing through it had before and has now.
    */
  private def changes(account: Account, row: Row)(using
      tx: Tx^
  ): Either[StoreError, Vector[Linking]] = {
    val was = row.old.getOrElse(Said(None, member = false))
    for {
      before <- before(account, was)
      after <- now(account)
      wasCleared = Tx.clearanceOf(before)
      nowCleared = Tx.clearanceOf(after)
    } yield {
      val (from, to) = (idOf(before), idOf(after))
      Vector(
        Option.when(was.email.isDefined && from != to)(
          Linking.Unlinked(account, PrincipalIds.stored(from), wasCleared, nowCleared)
        ),
        Option.when(row.now.email.isDefined && from != to)(
          Linking.Linked(account, PrincipalIds.stored(to), wasCleared, nowCleared)
        ),
        Option.when(was.member != row.now.member)(
          Linking.Standing(account, row.now.member, wasCleared, nowCleared)
        )
      ).flatten
    }
  }

  /** Whom `account` was while its attestation said `was`: its email's person, holding the other
    * accounts that person holds now and `account` as it was; else its home, holding it alone.
    */
  private def before(account: Account, was: Said)(using tx: Tx^): Either[StoreError, Principal] =
    was.email match {
      case Some(e) =>
        many(
          s"""SELECT e.principal_id, $HeldColumn
             |  FROM grit.emails e
             |  LEFT JOIN LATERAL (SELECT jsonb_agg(jsonb_build_array(n.account, n.vouched, n.member))
             |                       AS held
             |                      FROM grit.links n
             |                     WHERE n.principal_id = e.principal_id AND n.account <> ?) h
             |    ON true
             | WHERE e.email = ?""".stripMargin
        ) { ps =>
          ps.setString(1, SqlIdentities.written(account))
          ps.setString(2, e)
        }(rs => (rs.getString(1), rs.getString(2))).flatMap(person(account, was))
      case None =>
        many("SELECT home, '[]' FROM grit.identities WHERE account = ?")(
          _.setString(1, SqlIdentities.written(account))
        )(rs => (rs.getString(1), rs.getString(2))).flatMap(person(account, was))
    }

  /** The person the first of `rows` names, holding its accounts and `account` as `was` says. */
  private def person(account: Account, was: Said)(
      rows: Vector[(String, String)]
  ): Either[StoreError, Principal] =
    rows.headOption
      .toRight(StoreError.Invalid(s"no person for ${SqlIdentities.written(account)} before"))
      .flatMap((id, others) => SqlIdentities.principal(id, "person", others))
      .map {
        case Principal.Person(id, held) =>
          Principal.Person(
            id,
            held + Held(
              account,
              if (was.email.isDefined) Evidence.Vouched else Evidence.Home,
              was.member
            )
          )
        case Principal.Grit => Principal.Grit
      }

  /** Whom `account` is now, as `grit.links` says, with every account they hold. */
  private def now(account: Account)(using tx: Tx^): Either[StoreError, Principal] =
    many(
      s"""SELECT l.principal_id, $HeldColumn
         |  FROM grit.links l
         |  LEFT JOIN LATERAL (SELECT jsonb_agg(jsonb_build_array(n.account, n.vouched, n.member))
         |                       AS held
         |                      FROM grit.links n WHERE n.principal_id = l.principal_id) h
         |    ON true
         | WHERE l.account = ?""".stripMargin
    )(_.setString(1, SqlIdentities.written(account)))(rs => (rs.getString(1), rs.getString(2)))
      .flatMap(
        _.headOption
          .toRight(StoreError.Invalid(s"${SqlIdentities.written(account)} is no one"))
          .flatMap((id, held) => SqlIdentities.principal(id, "person", held))
      )

  private def many[A](sql: String)(set: PreparedStatement => Unit)(read: java.sql.ResultSet => A)(
      using tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        set(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[A]
          while (rs.next()) out += read(rs)
          out.result()
        }
      }
    }
  }

  private def update(sql: String)(set: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        set(ps)
        val _ = ps.executeUpdate()
      }
    }
  }
}

private object SqlVoucher {

  /** What an attestation says: its email, as stored, and whether its account is a full member. */
  final case class Said(email: Option[String], member: Boolean)

  /** An attestation as it was before a vouching (`None` when there was none) and as it is now. */
  final case class Row(old: Option[Said], now: Said)

  /** The id of the person `p` is; grit's own spelling for grit, which no account attested is. */
  def idOf(p: Principal): String = p match {
    case Principal.Person(id, _) => grit.core.id.PrincipalId.value(id)
    case Principal.Grit => "grit"
  }

  /** How long ago an attestation's account was last answered for, in microseconds, by this
    * transaction's clock; never below zero, though a vouching that began earlier and committed
    * later wrote it.
    */
  val Ago: String =
    "greatest(0, (extract(epoch FROM now() - grit.attestations.seen_at) * 1000000)::bigint)"

  /** The held accounts' JSON column, an empty array for none. */
  val HeldColumn: String = "coalesce(h.held, '[]'::jsonb)::text"

  /** `prefix` matched literally by `LIKE … ESCAPE '\'`. */
  def escaped(prefix: String): String =
    prefix.flatMap {
      case c @ ('%' | '_' | '\\') => s"\\$c"
      case c => c.toString
    }

  /** The advisory-lock class an email's first attestation is locked under, with the hash of the
    * address: the bytes of `geml`. Another class than a first sighting's and an edge's.
    */
  val EmailClass: Int = 0x67656d6c
}
