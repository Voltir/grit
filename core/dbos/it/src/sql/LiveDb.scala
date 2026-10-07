package grit.dbos.sql

import java.sql.DriverManager
import java.time.Instant

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PrincipalId, TurnRef}
import grit.core.message.Message
import grit.core.store.{Conversation, Entry, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Visibility}

/** Direct transactions on a live test database, for arranging rows and reading them back
  * outside the code under test.
  */
object LiveDb {

  /** What these transactions read and write at: maintenance's clearance under the shipped
    * visibility.
    */
  val Everything: Clearance = new Opener(Visibility.Shipped).maintenance

  /** Runs `body` in one transaction on `config`'s database: committed if it returns,
    * rolled back if it throws.
    */
  def transaction[A](config: DbConfig)(body: (Tx^) ?=> A): A =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        conn.setAutoCommit(false)
        try {
          val a = body(using Tx.open(conn, LiveDb.Everything))
          conn.commit()
          a
        } catch { case e: Throwable => conn.rollback(); throw e }
    }

  /** The conversation for `origin`, created at `label` if new. */
  def conversation(config: DbConfig, origin: Origin, label: Label = Label.Public): Conversation =
    transaction(config)(
      new SqlConversationStore().findOrCreate(origin, PrincipalId.Local, label)
    ) match {
      case Right(c) => c
      case Left(e: StoreError) => sys.error(s"arranging a conversation: $e")
    }

  /** `turn` recorded as one asked from, as the inbox and an edge record it: its first entry a
    * message `by` wrote, and, when given, its delivery's `address`.
    */
  def asking(config: DbConfig, turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
    transaction(config) {
      val id = EntryId(s"${ConversationId.value(turn.conversationId)}:asked")
      val entries = new SqlEntryStore()
      for {
        _ <-
          if (by == PrincipalId.Local || by == PrincipalId.Grit) Right(())
          else new SqlPrincipals().enroll(by, PrincipalId.value(by))
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

  /** Records that `by` wrote the inbound entry `id`, as the inbox does. */
  private def authored(id: EntryId, by: PrincipalId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(
      conn.prepareStatement("INSERT INTO grit.inbound (entry_id, author) VALUES (?, ?)")
    ) { ps =>
      ps.setString(1, EntryId.value(id))
      ps.setString(2, PrincipalId.value(by))
      ps.executeUpdate()
    }
    Right(())
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
