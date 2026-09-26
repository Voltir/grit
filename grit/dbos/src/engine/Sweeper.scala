package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{CloseRef, PeriodRef, SettleRef, WorkflowId}
import grit.core.period.Purgeable
import grit.core.plugin.{PluginCursors, PluginName, PostRef}
import grit.core.store.{LifecycleStore, PeriodStore, StoreError, Tx}
import grit.dbos.sql.SqlEntryStore
import grit.dbos.workflow.{Closes, Posts, Settles}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.ListWorkflowsInput

/** One sweep of the lifecycle. Every decision it makes is recomputed from the database, and
  * every action it takes is a workflow under a deterministic id, so a missed or doubled sweep,
  * a crash, or two engines sweeping at once change nothing.
  */
private[engine] final class Sweeper(
    dataSource: DataSource,
    client: DBOSClient,
    periods: PeriodStore,
    lifecycle: LifecycleStore,
    cursors: PluginCursors,
    plugins: () -> Vector[(PluginName, Int)]
) {

  /** Closes and asks, then posts, then purges: see [[Engine.sweep]]. */
  def once(now: Instant): Either[StoreError, Swept] =
    for {
      settings <- read(lifecycle.current())
      open <- read(periods.open())
      (due, quiet) = open.partition(a => !a.due(settings).at.isAfter(now))
      closed <- due.foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, a) =>
        acc.flatMap(done => close(a.attempt(settings)).map(done + _))
      }
      asked <- quiet
        .filter(_.asks(settings).exists(!_.isAfter(now)))
        .foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, a) =>
          acc.flatMap(done => ask(a.question).map(done + _))
        }
      posted <- plugins().foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, p) =>
        acc.flatMap(done => post(p._1, p._2).map(done + _))
      }
      cutoff = now.minusMillis(settings.windows.retention.toMillis)
      expired <- read(periods.expired(cutoff))
      purged <- expired.foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, p) =>
        acc.flatMap(done => purge(p, now).map(done + _))
      }
    } yield closed + asked + posted + purged

  /** `expired`'s workflows deleted, then its raw entries, marking it purged at `now`.
    * Workflows go first: a crash between the two leaves entries for the next sweep, which
    * deletes the workflows again (DBOS deletes what it has, and takes the rest as done),
    * never workflow histories nobody would look for again.
    */
  private def purge(expired: Purgeable, now: Instant): Either[StoreError, Swept] =
    for {
      attempts <- attempted(expired.attempts.flatMap(named))
      _ <- attempted(
        client.deleteWorkflows((expired.turns.map(WorkflowId.value) ++ attempts).asJava, false)
      )
      _ <- write(periods.purge(expired.period, now))
    } yield Swept(purged = Vector(expired.period))

  /** The ids of every workflow DBOS has whose id starts with `prefix`. */
  private def named(prefix: String): Vector[String] =
    client
      .listWorkflows(new ListWorkflowsInput().withWorkflowIdPrefix(prefix))
      .asScala
      .toVector
      .map(_.workflowId())

  /** The next run posting to `plugin` at `version` from its cursor, when the cursor is behind
    * the newest closed period and no run from it is still going: enqueued as [[close]]
    * enqueues an attempt. A finished run that left the cursor where it was failed, and after
    * [[PostRef.Attempts]] of them the cursor is `stuck`. Starting the cursor clears the
    * plugin's documents when its version changed.
    */
  private def post(plugin: PluginName, version: Int): Either[StoreError, Swept] =
    write {
      for {
        cursor <- cursors.start(plugin, version)
        after <- periods.closedAfter(cursor, 1)
      } yield Option.when(after.nonEmpty)(cursor)
    }.flatMap {
      case None => Right(Swept.nothing)
      case Some(cursor) =>
        attempted {
          val runs = client
            .listWorkflows(
              new ListWorkflowsInput().withWorkflowIdPrefix(PostRef.prefix(plugin, version, cursor))
            )
            .asScala
            .toVector
          if (runs.exists(r => Option(r.status()).forall(_.isActive()))) Swept.nothing
          else if (runs.size >= PostRef.Attempts)
            Swept(stuck = Vector(PostRef(plugin, version, cursor, runs.size - 1).workflowId))
          else {
            val run = PostRef(plugin, version, cursor, runs.size)
            val _ = client.enqueueWorkflow[String, Exception](
              Posts.enqueueOptions(run),
              // As `enqueue`'s: the array is empty and DBOS only reads it.
              caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
            )
            Swept(posted = Vector(run))
          }
        }
    }

  /** `attempt` enqueued if DBOS has no workflow under its id. One it has, running or
    * finished, is left alone: an attempt is made once. One that finished with its period
    * still open on the same deadline did not close it, and is `stuck` until the deadline moves.
    */
  private def close(attempt: CloseRef): Either[StoreError, Swept] =
    attempted {
      val id = attempt.workflowId
      Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(id)).getStatus())
        .map(_.status()) match {
        case Some(state) if state.isActive() => Swept.nothing
        case Some(_) => Swept(stuck = Vector(id))
        case None =>
          enqueue(attempt)
          Swept(enqueued = Vector(attempt))
      }
    }

  /** `question` enqueued if DBOS has no workflow under its id; one it has, running or
    * finished, is left alone: a question is asked once. One that finished and left the
    * period to be asked still (a verdict it could not keep) is `stuck` until new activity.
    */
  private def ask(question: SettleRef): Either[StoreError, Swept] =
    attempted {
      val id = question.workflowId
      Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(id)).getStatus())
        .map(_.status()) match {
        case Some(state) if state.isActive() => Swept.nothing
        case Some(_) => Swept(stuck = Vector(id))
        case None =>
          val _ = client.enqueueWorkflow[String, Exception](
            Settles.enqueueOptions(question),
            // As `enqueue`'s: the array is empty and DBOS only reads it.
            caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
          )
          Swept(asked = Vector(question))
      }
    }

  // A repeated enqueue of the same id is a no-op. The array is empty and DBOS only reads
  // it; separation checking treats arrays as mutable.
  private def enqueue(attempt: CloseRef): Unit = {
    val _ = client.enqueueWorkflow[String, Exception](
      Closes.enqueueOptions(attempt),
      caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
    )
  }

  private def attempted[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  private def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try body(using Tx.fromConnection(conn))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        if (result.isRight) conn.commit() else conn.rollback()
        result
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  private def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try body(using Tx.fromConnection(conn))
        finally conn.rollback()
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
}

/** What a sweep did: the close attempts it `enqueued`, the periods whose raw entries and
  * workflows it `purged`, the posting runs it enqueued (`posted`), the questions whether a
  * quiet period is finished it enqueued (`asked`), and the workflows it found
  * `stuck`: a close attempt that finished without closing its period, whose deadline has not
  * moved since, or a plugin's last run from a cursor it failed to move [[PostRef.Attempts]]
  * times. A stuck workflow is not run again; a close is attempted anew once its deadline moves.
  */
final case class Swept(
    enqueued: Vector[CloseRef] = Vector.empty,
    purged: Vector[PeriodRef] = Vector.empty,
    posted: Vector[PostRef] = Vector.empty,
    stuck: Vector[WorkflowId] = Vector.empty,
    asked: Vector[SettleRef] = Vector.empty
) {
  def +(other: Swept): Swept =
    Swept(
      enqueued ++ other.enqueued,
      purged ++ other.purged,
      posted ++ other.posted,
      stuck ++ other.stuck,
      asked ++ other.asked
    )
}

object Swept {
  val nothing: Swept = Swept()
}
