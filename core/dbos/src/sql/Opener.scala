package grit.dbos.sql

import java.sql.Connection

import scala.util.Using

import grit.core.id.{ConversationId, PrincipalId, TurnSeq}
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Maintenance, Subject, Visibility}

/** How every transaction this module opens gets its clearance (ADR 0030): a [[Subject]]
  * resolved against the rows it names and the deployment's `visibility`, or [[maintenance]].
  */
private[dbos] final class Opener(visibility: Visibility) {
  import SqlEntryStore.attempt

  /** What this module's own transactions read and write at (an edge's inbox and desk, the
    * sweeps, the collector): every label the database can hold. An engine refuses to start
    * under compartments that drop one it ran under before (`SqlLabels.reconcile`), so the
    * declared ones' top is above every stored label, an unmapped one's included.
    */
  val maintenance: Clearance = Maintenance.clearance(visibility.compartments.top)

  /** `conn`'s transaction, opened for `subject`, its clearance resolved on `conn` first; a
    * `DatabaseError` when that read fails.
    */
  def open(subject: Subject, conn: Connection^): Either[StoreError, Tx^{conn}] =
    clearance(subject)(using Tx.open(conn, maintenance)).map(Tx.open(conn, _))

  /** The clearance `subject` reads at, as [[Subject]]'s cases say: one read of its
    * conversation, its label, and its turn's first entry and that entry's author.
    */
  def clearance(subject: Subject)(using tx: Tx^): Either[StoreError, Clearance] =
    subject match {
      case Subject.Public => Right(Clearance.of(Label.Public))
      case Subject.Conversation(id) =>
        named(id, None).map(_.fold(Clearance.of(Label.Public)) { n =>
          Clearance.inRoom(n.origin.room, n.label, n.label)
        })
      case Subject.Turn(turn) =>
        named(turn.conversationId, Some(turn.turnSeq)).map(_.fold(Clearance.of(Label.Public)) { n =>
          val asker = n.origin match {
            case Origin.Task(_, _) => Some(PrincipalId.Grit)
            case _ => n.first.map(_.getOrElse(PrincipalId.Grit))
          }
          Clearance.inRoom(n.origin.room, n.label, asker.fold(Label.Public)(visibility.cleared))
        })
    }

  /** A conversation as a subject names it: where it is, its label, and, when a turn was named,
    * that turn's first entry, if any, as its author (`None` when grit's own).
    */
  private final case class Named(origin: Origin, label: Label, first: Option[Option[PrincipalId]])

  /** `id`'s conversation and, for `turn`, its first entry; `None` when the conversation is gone.
    */
  private def named(id: ConversationId, turn: Option[TurnSeq])(using
      tx: Tx^
  ): Either[StoreError, Option[Named]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""SELECT c.origin::text AS origin, ${SqlLabels.columns("l")},
             |       e.id AS first_entry, i.author
             |  FROM grit.conversations c
             |  JOIN grit.labels l ON l.id = c.label_id
             |  LEFT JOIN LATERAL (SELECT id FROM grit.entries
             |                      WHERE conversation_id = c.id AND turn_seq = ?
             |                      ORDER BY seq LIMIT 1) e ON true
             |  LEFT JOIN grit.inbound i ON i.entry_id = e.id
             | WHERE c.id = ?::uuid""".stripMargin
        )
      ) { ps =>
        turn match {
          case Some(t) => ps.setLong(1, TurnSeq.value(t))
          case None => ps.setNull(1, java.sql.Types.BIGINT)
        }
        ps.setString(2, ConversationId.value(id))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next()) {
            val first = Option(rs.getString("first_entry")).map { _ =>
              Option(rs.getString("author")).map(PrincipalId(_))
            }
            (rs.getString("origin"), SqlLabels.read(rs), first)
          }
        }
      }
    }.flatMap {
      case None => Right(None)
      case Some((origin, label, first)) =>
        SqlConversationStore
          .readOrigin(ujson.read(origin))
          .map(o => Some(Named(o, label, first)))
          .left
          .map(why => StoreError.Invalid(s"conversation ${ConversationId.value(id)}: $why"))
    }
  }
}
