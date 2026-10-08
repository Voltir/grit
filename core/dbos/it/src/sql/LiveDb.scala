package grit.dbos.sql

import java.sql.DriverManager
import java.time.Instant

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PrincipalId, PrincipalIds, TurnRef}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.store.{Conversation, Entry, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, TestLabels, Visibility}

/** Direct transactions on a live test database, for arranging rows and reading them back
  * outside the code under test.
  */
object LiveDb {

  /** What these transactions read and write at unless told: maintenance's clearance under
    * [[TestLabels.Trialled]], the visibility the labelled suites declare, so every label a
    * suite writes, unmapped included. A label naming a compartment it does not declare is
    * not read: a suite labelling with another declares it there first.
    */
  val Everything: Clearance = Trialled.maintenance

  /** What opens these transactions: [[TestLabels.Trialled]]'s. */
  lazy val Trialled: Opener = new Opener(TestLabels.Trialled)

  /** Runs `body` in one transaction on `config`'s database, opened at `clearance`: committed
    * if it returns, rolled back if it throws.
    */
  def transaction[A](config: DbConfig, clearance: Clearance = Everything)(body: (Tx^) ?=> A): A =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        conn.setAutoCommit(false)
        try {
          val a = body(using Trialled.at(clearance, conn))
          conn.commit()
          a
        } catch { case e: Throwable => conn.rollback(); throw e }
    }

  /** Runs `body` as [[transaction]] does, at maintenance's clearance, labelling places as
    * `visibility` does.
    */
  def under[A](config: DbConfig, visibility: Visibility)(body: (Tx^) ?=> A): A =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        conn.setAutoCommit(false)
        try {
          val opener = new Opener(visibility)
          val a = body(using opener.maintained(conn))
          conn.commit()
          a
        } catch { case e: Throwable => conn.rollback(); throw e }
    }

  /** The conversation for `origin`, created at `label` if new. */
  def conversation(config: DbConfig, origin: Origin, label: Label = Label.Public): Conversation =
    transaction(config)(
      new SqlConversationStore().findOrCreate(origin, Account.Local, label)
    ) match {
      case Right(c) => c
      case Left(e: StoreError) => sys.error(s"arranging a conversation: $e")
    }

  /** `turn` recorded as one asked from, as the inbox and an edge record it: its first entry a
    * message `by` wrote, and, when given, its delivery's `address`.
    */
  def asking(config: DbConfig, turn: TurnRef, by: Account, address: Option[String]): Unit =
    transaction(config) {
      val id = EntryId(s"${ConversationId.value(turn.conversationId)}:asked")
      val entries = new SqlEntryStore()
      for {
        _ <-
          if (by == Account.Local || by == Account.Grit) Right(())
          else new SqlPrincipals().name(by, Account.written(by))
        next <- entries.lockNext(turn.conversationId)
        _ <- entries.insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User("remind me")),
            Instant.parse("2026-10-07T08:00:00Z")
          )
        )
        _ <- authored(id, by)
        _ <- address.fold[Either[StoreError, Unit]](Right(()))(new SqlDeliveries().await(turn, _))
      } yield ()
    }.fold(e => sys.error(s"asking: $e"), identity)

  /** Records that the inbound entry `id` was written through `by`, as the inbox does: `by`
    * kept first, as a new person's one account when not seen before.
    */
  def authored(id: EntryId, by: Account)(using tx: Tx^): Either[StoreError, Unit] =
    SqlIdentities.enroll(Set(by)).map { _ =>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("INSERT INTO grit.inbound (entry_id, account) VALUES (?, ?)")
      ) { ps =>
        ps.setString(1, EntryId.value(id))
        ps.setString(2, Account.written(by))
        val _ = ps.executeUpdate()
      }
    }

  /** The principal `account` is linked to now; throws when it was never seen. */
  def principal(config: DbConfig, account: Account): PrincipalId =
    transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("SELECT principal_id FROM grit.links WHERE account = ?")
      ) { ps =>
        ps.setString(1, Account.written(account))
        Using.resource(ps.executeQuery()) { rs =>
          if (rs.next()) PrincipalIds.stored(rs.getString(1))
          else sys.error(s"${Account.written(account)} was never seen")
        }
      }
    }

  /** Every ledger row: entry, model, cost, and the estimate of the request's input. */
  def ledger(config: DbConfig): Vector[(String, String, Option[BigDecimal], Long)] =
    transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT entry_id, model, cost_usd, estimated_input_tokens FROM grit.usage_ledger ORDER BY entry_id"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String, Option[BigDecimal], Long)]
          while (rs.next())
            rows += ((
              rs.getString(1),
              rs.getString(2),
              Option(rs.getBigDecimal(3)).map(BigDecimal(_)),
              rs.getLong(4)
            ))
          rows.result()
        }
      }
    }
}
