package grit.dbos.engine

import java.sql.{Connection, DriverManager}
import java.util.UUID
import javax.sql.DataSource

import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.edge.{Desk, DeskError, OutcomeJson, Registration, ToolRequest}
import grit.core.host.ProcessIdentity
import grit.core.id.{CallSlot, EdgeId, PrincipalId, WorkflowId}
import grit.core.place.Place
import grit.core.prompt.{Fragment, FragmentId}
import grit.core.tool.{Outcome, Retry, ToolSet, ToolSetId}
import grit.dbos.sql.{DbConfig, Opener, SqlPromptStore, SqlToolRequests, SqlToolSets}

import dev.dbos.transact.DBOSClient
import org.postgresql.PGConnection
import org.slf4j.LoggerFactory

/** [[Desk]] over Postgres (ADR 0017): the edge's registration in `grit.edges`, live for as
  * long as `conn`, a connection of its own, holds its advisory lock; `conn` also listens for
  * dispatched requests and writes the heartbeat, from the one thread that calls [[await]].
  * Everything else runs on `dataSource`, from any thread. The turn waiting on an answer is
  * rung through `client`.
  */
final class SqlDesk private (
    conn: Connection,
    dataSource: DataSource,
    client: DBOSClient,
    val registration: Registration,
    session: UUID,
    opener: Opener
) extends Desk,
      AutoCloseable {

  // Read and written only by the thread that calls await.
  @caps.unsafe.untrackedCaptures
  private var beaten = SqlDesk.monotonic()

  def await(within: FiniteDuration): Boolean =
    try {
      if (SqlDesk.monotonic() - beaten > SqlDesk.Beat.toNanos) {
        Using.resource(
          conn.prepareStatement(
            "UPDATE grit.edges SET heartbeat_at = now() WHERE id = ?::uuid AND session = ?::uuid"
          )
        ) { ps =>
          ps.setString(1, EdgeId.value(registration.edge))
          ps.setString(2, session.toString)
          ps.executeUpdate()
        }
        beaten = SqlDesk.monotonic()
      }
      val got = conn.unwrap(classOf[PGConnection]).getNotifications(within.toMillis.toInt.max(1))
      Option(got).exists(_.length > 0)
    } catch {
      case NonFatal(_) =>
        // A closed or broken connection: wait out `within` anyway, so a caller's loop
        // does not spin.
        try Thread.sleep(within.toMillis)
        catch { case _: InterruptedException => () }
        false
    }

  def open(): Either[DeskError, Vector[ToolRequest]] =
    listed(
      s"""SELECT ${SqlToolRequests.Columns} FROM grit.tool_requests r
         |  JOIN grit.places p ON p.id = r.workspace_id
         | WHERE r.state = 'open' AND r.claimed_by IS NULL AND ${hosted("r")}
         | ORDER BY r.created_at, r.key""".stripMargin
    ).map(_.map(_._1))

  // Known gap: a claim never asks whether this desk's own edge is still live
  // (`SqlToolRequests.live`). If the connection holding its lock drops while the process
  // lives on, the desk keeps claiming and running requests under a dead session, and
  // another edge's `orphans` takes the same requests over: a `Rerun` tool runs twice, and
  // an `Interrupt` one is answered Interrupted while it may still be running. The fix is a
  // claim that requires this edge's lock, and a desk that stops serving once its lock is
  // gone. `EdgesContract` pins today's behaviour ("a desk's claim does not ask whether its
  // edge is live").
  def claim(request: ToolRequest): Either[DeskError, Boolean] =
    changed(
      """UPDATE grit.tool_requests SET state = 'claimed', claimed_by = ?::uuid,
        |  claim_session = ?::uuid, claimed_at = clock_timestamp()
        | WHERE key = ? AND state = 'open'""".stripMargin,
      EdgeId.value(registration.edge),
      session.toString,
      request.slot.key
    ).map(_ == 1)

  def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean] = {
    val json = ujson.write(OutcomeJson.write(outcome))
    changed(
      """UPDATE grit.tool_requests SET state = 'answered', outcome = ?::jsonb,
        |  answered_at = clock_timestamp()
        | WHERE key = ? AND state = 'claimed' AND claim_session = ?::uuid""".stripMargin,
      json,
      slot.key,
      session.toString
    ).map { rows =>
      if (rows == 1) tell(slot)
      rows == 1
    }
  }

  def orphans(): Either[DeskError, Vector[ToolRequest]] =
    listed(
      s"""SELECT ${SqlToolRequests.Columns}, r.claim_session FROM grit.tool_requests r
         |  JOIN grit.places p ON p.id = r.workspace_id
         |  JOIN grit.edges e ON e.id = r.claimed_by
         | WHERE r.state = 'claimed' AND ${hosted("r")}
         |   AND (r.claim_session <> e.session OR NOT ${SqlToolRequests.live("e")})
         | ORDER BY r.created_at, r.key""".stripMargin,
      withSession = true
    ).flatMap { found =>
      found.foldLeft[Either[DeskError, Vector[ToolRequest]]](Right(Vector.empty)) {
        case (acc, (q, dead)) =>
          acc.flatMap { reruns =>
            q.retry match {
              case Retry.Interrupt =>
                val json = ujson.write(OutcomeJson.write(Outcome.Interrupted))
                changed(
                  """UPDATE grit.tool_requests SET state = 'answered', outcome = ?::jsonb,
                    |  answered_at = clock_timestamp()
                    | WHERE key = ? AND state = 'claimed' AND claim_session = ?::uuid""".stripMargin,
                  json,
                  q.slot.key,
                  dead
                ).map { rows =>
                  if (rows == 1) tell(q.slot)
                  reruns
                }
              case Retry.Rerun =>
                changed(
                  """UPDATE grit.tool_requests SET claimed_by = ?::uuid, claim_session = ?::uuid,
                    |  claimed_at = clock_timestamp()
                    | WHERE key = ? AND state = 'claimed' AND claim_session = ?::uuid""".stripMargin,
                  EdgeId.value(registration.edge),
                  session.toString,
                  q.slot.key,
                  dead
                ).map(rows => if (rows == 1) reruns :+ q else reruns)
            }
          }
      }
    }

  def advertise(
      place: Place,
      tools: ToolSet,
      instructions: Vector[Fragment]
  ): Either[DeskError, Unit] =
    SqlDesk.inTransaction(dataSource, opener) { (tx: grit.core.store.Tx^) ?=>
      for {
        _ <- new SqlToolSets().keep(tools)
        _ <- new SqlPromptStore().keep(instructions)
        _ <- grit.dbos.sql.SqlEntryStore.attempt {
          val c: java.sql.Connection^{tx} = grit.core.store.Tx.connection(tx)
          Using.resource(
            c.prepareStatement(
              """INSERT INTO grit.edge_places (edge_id, place_id, tools, fragments)
                |SELECT ?::uuid, p.id, ?, ?::jsonb FROM grit.places p
                | WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
                |ON CONFLICT (edge_id, place_id) DO UPDATE
                |  SET tools = EXCLUDED.tools, fragments = EXCLUDED.fragments""".stripMargin
            )
          ) { ps =>
            ps.setString(1, EdgeId.value(registration.edge))
            ps.setString(2, ToolSetId.value(tools.id))
            ps.setString(
              3,
              ujson.Arr.from(instructions.map(f => ujson.Str(FragmentId.value(f.id)))).render()
            )
            ps.setString(4, SqlToolRequests.pathJson(place))
            ps.executeUpdate()
            ()
          }
        }
      } yield ()
    }

  /** Releases the registration, by closing its connection. */
  def close(): Unit =
    try conn.close()
    catch { case NonFatal(_) => () }

  /** Rings the turn waiting on `slot` with [[Desk.Doorbell]]; the turn reads the answer from
    * the row. A turn that is not waiting, or a ring that fails, is left to read it there when
    * its wait ends.
    */
  private def tell(slot: CallSlot): Unit =
    try client.send(WorkflowId.value(slot.turn.workflowId), Desk.Doorbell, slot.key, slot.key)
    catch {
      case NonFatal(e) =>
        LoggerFactory.getLogger("grit.edge").debug(s"not told ${slot.key}: ${e.getMessage}")
    }

  /** Whether the request `r`'s workspace is a place this edge registered. */
  private def hosted(r: String): String =
    s"""$r.workspace_id IN (SELECT place_id FROM grit.edge_places WHERE edge_id = '${EdgeId.value(
        registration.edge
      )}'::uuid)"""

  private def listed(
      sql: String,
      withSession: Boolean = false
  ): Either[DeskError, Vector[(ToolRequest, String)]] =
    try
      Using.resource(dataSource.getConnection()) { c =>
        Using.resource(c.createStatement()) { st =>
          Using.resource(st.executeQuery(sql)) { rs =>
            val rows = Vector.newBuilder[Either[String, (ToolRequest, String)]]
            while (rs.next())
              rows += SqlToolRequests
                .read(rs)
                .map(q => (q, if (withSession) rs.getString("claim_session") else ""))
            rows
              .result()
              .foldLeft[Either[DeskError, Vector[(ToolRequest, String)]]](Right(Vector.empty)) {
                (acc, r) => acc.flatMap(done => r.map(done :+ _).left.map(DeskError(_)))
              }
          }
        }
      }
    catch { case NonFatal(e) => Left(DeskError(Option(e.getMessage).getOrElse(e.toString))) }

  private def changed(sql: String, values: String*): Either[DeskError, Int] =
    try
      Right(Using.resource(dataSource.getConnection()) { c =>
        Using.resource(c.prepareStatement(sql)) { ps =>
          values.zipWithIndex.foreach((v, i) => ps.setString(i + 1, v))
          ps.executeUpdate()
        }
      })
    catch { case NonFatal(e) => Left(DeskError(Option(e.getMessage).getOrElse(e.toString))) }
}

object SqlDesk {

  /** How often a desk writes its heartbeat, for a person to read: 2 seconds. */
  val Beat: FiniteDuration = 2.seconds

  /** The JVM's monotonic nanoseconds, for the heartbeat's interval: a desk holds no capability
    * but its connection (the engine's proof for keeping it rests on that), so it takes no Clock.
    */
  private def monotonic(): Long = System.nanoTime() // clock-check: see the doc above

  /** Registers an edge for `principal` hosting `places`, in the process `identity` names,
    * live while the desk is open: the same principal, machine and places reuse a
    * registration no live edge holds, or take a new one. `Left` when the database cannot be
    * reached. Its own transactions run at `opener`'s maintenance.
    */
  def open(
      config: DbConfig,
      dataSource: DataSource,
      client: DBOSClient,
      principal: PrincipalId,
      places: Set[Place],
      identity: ProcessIdentity,
      opener: Opener
  ): Either[DeskError, SqlDesk^] =
    try {
      val conn = DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
      try {
        val key = (PrincipalId
          .value(principal) +: identity.machine +: places.toVector.map(_.written).sorted)
          .mkString("|")
        // clock-check: a desk holds no capability but its connection (the engine's proof for
        // keeping it rests on that), so it takes no Fresh; Postgres only compares the session
        val session = UUID.randomUUID()
        val edge = reuse(conn, key).getOrElse(insert(conn, key, principal, identity))
        Using.resource(
          conn.prepareStatement(
            """UPDATE grit.edges SET session = ?::uuid, pid = ?, protocol = ?, started_at = now(),
              |  heartbeat_at = now() WHERE id = ?::uuid""".stripMargin
          )
        ) { ps =>
          ps.setString(1, session.toString)
          ps.setLong(2, identity.pid)
          ps.setInt(3, ToolRequest.Protocol)
          ps.setString(4, EdgeId.value(edge))
          ps.executeUpdate()
        }
        places.foreach { place =>
          Using.resource(
            conn.prepareStatement(
              """WITH place AS (
                |  INSERT INTO grit.places (path)
                |  VALUES (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))
                |  ON CONFLICT (path) DO UPDATE SET path = EXCLUDED.path
                |  RETURNING id)
                |INSERT INTO grit.edge_places (edge_id, place_id) SELECT ?::uuid, id FROM place
                |ON CONFLICT (edge_id, place_id) DO NOTHING""".stripMargin
            )
          ) { ps =>
            ps.setString(1, SqlToolRequests.pathJson(place))
            ps.setString(2, EdgeId.value(edge))
            ps.executeUpdate()
          }
        }
        Using.resource(conn.createStatement())(_.execute(s"LISTEN ${SqlToolRequests.Channel}"))
        Right(
          new SqlDesk(
            conn,
            dataSource,
            client,
            Registration(edge, principal, places),
            session,
            opener
          )
        )
      } catch {
        case NonFatal(e) =>
          conn.close()
          throw e
      }
    } catch { case NonFatal(e) => Left(DeskError(Option(e.getMessage).getOrElse(e.toString))) }

  /** A registration under `key` whose lock `conn` could take, now held by `conn`. */
  private def reuse(conn: Connection, key: String): Option[EdgeId] = {
    val candidates = Using.resource(
      conn.prepareStatement("SELECT id, lock_key FROM grit.edges WHERE key = ? ORDER BY id")
    ) { ps =>
      ps.setString(1, key)
      Using.resource(ps.executeQuery()) { rs =>
        val found = Vector.newBuilder[(String, Int)]
        while (rs.next()) found += ((rs.getString(1), rs.getInt(2)))
        found.result()
      }
    }
    candidates.find((_, lockKey) => locked(conn, lockKey)).map((id, _) => EdgeId(id))
  }

  /** A new registration under `key`, its lock held by `conn`. */
  private def insert(
      conn: Connection,
      key: String,
      principal: PrincipalId,
      identity: ProcessIdentity
  ): EdgeId = {
    val (id, lockKey) = Using.resource(
      conn.prepareStatement(
        """INSERT INTO grit.edges (key, principal, machine, pid, session, protocol, started_at, heartbeat_at)
          |VALUES (?, ?, ?, ?, gen_random_uuid(), ?, now(), now()) RETURNING id, lock_key""".stripMargin
      )
    ) { ps =>
      ps.setString(1, key)
      ps.setString(2, PrincipalId.value(principal))
      ps.setString(3, identity.machine)
      ps.setLong(4, identity.pid)
      ps.setInt(5, ToolRequest.Protocol)
      Using.resource(ps.executeQuery()) { rs =>
        rs.next()
        (rs.getString(1), rs.getInt(2))
      }
    }
    if (!locked(conn, lockKey)) sys.error(s"a new edge's lock $lockKey is held already")
    EdgeId(id)
  }

  private def locked(conn: Connection, lockKey: Int): Boolean =
    Using.resource(conn.prepareStatement("SELECT pg_try_advisory_lock(?, ?)")) { ps =>
      ps.setInt(1, SqlToolRequests.EdgeClass)
      ps.setInt(2, lockKey)
      Using.resource(ps.executeQuery())(rs => rs.next() && rs.getBoolean(1))
    }

  /** Runs `body` in a transaction of its own on `dataSource`, at `opener`'s maintenance:
    * committed on `Right`.
    */
  private def inTransaction[A](dataSource: DataSource, opener: Opener)(
      body: (grit.core.store.Tx^) ?=> Either[grit.core.store.StoreError, A]
  ): Either[DeskError, A] =
    try
      Using.resource(dataSource.getConnection()) { c =>
        c.setAutoCommit(false)
        val result =
          try opener.maintained(c).flatMap(tx => body(using tx))
          catch { case NonFatal(e) => c.rollback(); throw e }
        if (result.isRight) c.commit() else c.rollback()
        result.left.map(e => DeskError(e.toString))
      }
    catch { case NonFatal(e) => Left(DeskError(Option(e.getMessage).getOrElse(e.toString))) }
}
