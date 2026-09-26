package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{CloseRef, WorkflowId}
import grit.core.store.{LifecycleStore, PeriodStore, StoreError, Tx}
import grit.dbos.sql.SqlEntryStore
import grit.dbos.workflow.Closes

import dev.dbos.transact.DBOSClient

/** One sweep of the lifecycle. Every decision it makes is recomputed from the database, and
  * every action it takes is a workflow under a deterministic id, so a missed or doubled sweep,
  * a crash, or two engines sweeping at once change nothing.
  */
private[engine] final class Sweeper(
    dataSource: DataSource,
    client: DBOSClient,
    periods: PeriodStore,
    lifecycle: LifecycleStore
) {

  def once(now: Instant): Either[StoreError, Swept] =
    read {
      for {
        settings <- lifecycle.current()
        open <- periods.open()
      } yield open.filter(a => !a.due(settings.windows).at.isAfter(now)).map(_.attempt)
    }.flatMap { due =>
      due.foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, attempt) =>
        acc.flatMap(done => close(attempt).map(done + _))
      }
    }

  /** `attempt` enqueued if DBOS has no workflow under its id, or enqueued again when the one
    * it has finished without closing the period (which is still due): its deadline moved
    * while it waited, or it failed.
    */
  private def close(attempt: CloseRef): Either[StoreError, Swept] =
    attempted {
      val id = WorkflowId.value(attempt.workflowId)
      Option(client.retrieveWorkflow[String, Exception](id).getStatus()).map(_.status()) match {
        case Some(state) if state.isActive() => Swept.nothing
        case Some(_) =>
          client.deleteWorkflows(java.util.List.of(id), false)
          enqueue(attempt)
          Swept(Vector.empty, Vector(attempt))
        case None =>
          enqueue(attempt)
          Swept(Vector(attempt), Vector.empty)
      }
    }

  private def enqueue(attempt: CloseRef): Unit = {
    // A repeated enqueue of the same id is a no-op. The array is empty and DBOS only reads
    // it; separation checking treats arrays as mutable.
    val _ = client.enqueueWorkflow[String, Exception](
      Closes.enqueueOptions(attempt),
      caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
    )
  }

  private def attempted[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

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

/** What a sweep did: the close attempts it `enqueued` for the first time, and those it
  * `retried`, an earlier run of the same attempt having finished with the period still due.
  */
final case class Swept(enqueued: Vector[CloseRef], retried: Vector[CloseRef]) {
  def +(other: Swept): Swept = Swept(enqueued ++ other.enqueued, retried ++ other.retried)
}

object Swept {
  val nothing: Swept = Swept(Vector.empty, Vector.empty)
}
