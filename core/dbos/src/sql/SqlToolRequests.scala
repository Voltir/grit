package grit.dbos.sql

import java.sql.ResultSet

import scala.util.Using

import grit.core.edge.{
  Advert,
  EdgeDirectory,
  OutcomeJson,
  Permit,
  RequestState,
  ToolRequest,
  ToolRequests
}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, WorkflowId}
import grit.core.model.CatalogJson
import grit.core.place.Place
import grit.core.prompt.FragmentId
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSetId}

/** [[ToolRequests]] over `grit.tool_requests`. */
final class SqlToolRequests extends ToolRequests {
  import SqlEntryStore.attempt

  def dispatch(requests: Vector[ToolRequest])(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""WITH place AS (
             |  INSERT INTO grit.places (path)
             |  VALUES (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))
             |  ON CONFLICT (path) DO UPDATE SET path = EXCLUDED.path
             |  RETURNING id)
             |INSERT INTO grit.tool_requests (key, protocol, workflow_id, conversation_id, turn_seq,
             |  workspace_id, principal, tool, permit, retry, arguments, repairs, state)
             |SELECT ?, ?, ?, ?::uuid, ?, id, ?, ?, ?, ?, ?::jsonb, ?::jsonb, 'open' FROM place
             |ON CONFLICT (key) DO NOTHING""".stripMargin
        )
      ) { ps =>
        requests.foreach { q =>
          ps.setString(1, SqlToolRequests.pathJson(q.workspace))
          ps.setString(2, q.slot.key)
          ps.setInt(3, q.protocol)
          ps.setString(4, WorkflowId.value(q.slot.turn.workflowId))
          ps.setString(5, ConversationId.value(q.conversation))
          ps.setLong(6, grit.core.id.TurnSeq.value(q.slot.turn.turnSeq))
          ps.setString(7, PrincipalId.value(q.principal))
          ps.setString(8, ToolName.value(q.tool))
          ps.setString(9, q.permit.key)
          ps.setString(10, q.retry.key)
          ps.setString(11, ujson.write(q.arguments))
          ps.setString(12, SqlToolRequests.repairsJson(q))
          ps.executeUpdate()
        }
      }
      if (requests.nonEmpty)
        Using.resource(conn.createStatement())(
          _.execute(s"SELECT pg_notify('${SqlToolRequests.Channel}', '')")
        )
      ()
    }
  }

  def settle(slot: CallSlot)(using tx: Tx^): Either[StoreError, RequestState] =
    SqlToolRequests.expire(slot, "state = 'open'")

  def abandon(slot: CallSlot)(using tx: Tx^): Either[StoreError, RequestState] =
    SqlToolRequests.expire(slot, "state IN ('open', 'claimed')")

  def answered(slot: CallSlot)(using tx: Tx^): Either[StoreError, Option[Outcome]] =
    SqlToolRequests.standing(slot).flatMap {
      case None => Left(StoreError.Invalid(s"no tool request ${slot.key}"))
      case Some(("answered", Some(outcome))) => SqlToolRequests.outcome(slot, outcome).map(Some(_))
      case Some(_) => Right(None)
    }
}

/** [[EdgeDirectory]] over `grit.edges` and `grit.edge_places`, live by `pg_locks`. */
final class SqlEdgeDirectory extends EdgeDirectory {
  import SqlEntryStore.attempt

  def serving(place: Place)(using tx: Tx^): Either[StoreError, Option[Advert]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""SELECT e.id, ep.tools, ep.fragments::text FROM grit.edge_places ep
             |  JOIN grit.edges e ON e.id = ep.edge_id
             |  JOIN grit.places p ON p.id = ep.place_id
             | WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
             |   AND ep.tools IS NOT NULL AND ${SqlToolRequests.live("e")}
             | ORDER BY e.id LIMIT 1""".stripMargin
        )
      ) { ps =>
        ps.setString(1, SqlToolRequests.pathJson(place))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())((rs.getString(1), rs.getString(2), rs.getString(3)))
        }
      }
    }.flatMap {
      case None => Right(None)
      case Some((edge, tools, fragments)) =>
        (for {
          set <- ToolSetId.of(tools)
          ids <- ujson
            .read(fragments)
            .arrOpt
            .toRight("fragments are not a list")
            .flatMap(_.toVector.foldLeft[Either[String, Vector[FragmentId]]](Right(Vector.empty)) {
              (acc, v) =>
                acc.flatMap(done =>
                  v.strOpt
                    .toRight("a fragment id is not a string")
                    .flatMap(FragmentId.of)
                    .map(done :+ _)
                )
            })
        } yield Some(Advert(EdgeId(edge), set, ids))).left
          .map(why => StoreError.DatabaseError(s"an edge's advert is unreadable: $why"))
    }
  }
}

private[dbos] object SqlToolRequests {
  import SqlEntryStore.attempt

  /** The channel a dispatch notifies and every desk listens on. */
  val Channel = "grit_tool_requests"

  /** The advisory-lock class every edge's lock is taken under, with its `lock_key`: the bytes
    * of `gred`. Two ints, so an edge's lock never meets the engine's one bigint key.
    */
  val EdgeClass: Int = 0x67726564

  /** Whether the edge row `e` is live: its lock is held in this database. */
  def live(e: String): String =
    s"""EXISTS (SELECT 1 FROM pg_locks l WHERE l.locktype = 'advisory' AND l.granted
       |  AND l.classid = $EdgeClass AND l.objid = $e.lock_key AND l.objsubid = 2
       |  AND l.database = (SELECT oid FROM pg_database WHERE datname = current_database()))""".stripMargin

  def pathJson(place: Place): String = ujson.Arr.from(place.segments.map(ujson.Str(_))).render()

  def repairsJson(q: ToolRequest): String =
    ujson.Arr
      .from(q.repairs.toVector.map(r => ujson.Str(CatalogJson.argRepair(r))).sortBy(_.str))
      .render()

  /** The columns [[read]] reads, from `tool_requests r` joined to `places p` on its workspace. */
  val Columns: String =
    "r.key, r.protocol, r.conversation_id, array_to_json(p.path)::text, r.principal, r.tool, r.permit, r.retry, r.arguments::text, r.repairs::text"

  /** The request `rs` holds ([[Columns]]), or why it cannot be read. */
  def read(rs: ResultSet): Either[String, ToolRequest] = {
    val key = rs.getString(1)
    val path: Vector[String] =
      ujson.read(rs.getString(4)).arrOpt.fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
    for {
      slot <- CallSlot.read(key).toRight(s"not a slot: $key")
      workspace <- path match {
        case ns +: rest =>
          grit.core.place.Namespace
            .of(ns)
            .map(n => Place.under(n, rest))
            .toRight(s"no namespace $ns")
        case _ => Left("an empty workspace")
      }
      tool <- ToolName.of(rs.getString(6))
      permit <- Permit.of(rs.getString(7)).toRight("no permit")
      retry <- Retry.of(rs.getString(8)).toRight("no retry")
      repairs <- ujson
        .read(rs.getString(10))
        .arrOpt
        .toRight("repairs are not a list")
        .flatMap(
          _.toVector.foldLeft[Either[String, Set[grit.core.model.ArgRepair]]](Right(Set.empty)) {
            (acc, v) => acc.flatMap(done => CatalogJson.readArgRepair(v).map(done + _))
          }
        )
    } yield ToolRequest(
      slot,
      rs.getInt(2),
      ConversationId(rs.getString(3)),
      workspace,
      PrincipalId(rs.getString(5)),
      tool,
      permit,
      retry,
      ujson.read(rs.getString(9)),
      repairs
    )
  }

  /** Expires the request at `slot` where `when` holds, then reads where it stands. */
  def expire(slot: CallSlot, when: String)(using tx: Tx^): Either[StoreError, RequestState] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"UPDATE grit.tool_requests SET state = 'expired' WHERE key = ? AND $when"
        )
      ) { ps =>
        ps.setString(1, slot.key)
        ps.executeUpdate()
      }
    }.flatMap(_ => standing(slot)).flatMap {
      case None => Left(StoreError.Invalid(s"no tool request ${slot.key}"))
      case Some(("expired", _)) => Right(RequestState.Expired)
      case Some(("answered", Some(json))) => outcome(slot, json).map(RequestState.Answered(_))
      case Some((_, _)) => Right(RequestState.Claimed)
    }
  }

  /** The state of the request at `slot` and its outcome's JSON; `None` when no request has
    * its key.
    */
  def standing(
      slot: CallSlot
  )(using tx: Tx^): Either[StoreError, Option[(String, Option[String])]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement("SELECT state, outcome::text FROM grit.tool_requests WHERE key = ?")
      ) { ps =>
        ps.setString(1, slot.key)
        Using.resource(ps.executeQuery())(rs =>
          Option.when(rs.next())((rs.getString(1), Option(rs.getString(2))))
        )
      }
    }
  }

  /** The outcome `json` holds, the request at `slot`'s; `DatabaseError` when it cannot be read. */
  def outcome(slot: CallSlot, json: String): Either[StoreError, Outcome] =
    OutcomeJson
      .read(ujson.read(json))
      .left
      .map(why => StoreError.DatabaseError(s"tool request ${slot.key}: $why"))
}
