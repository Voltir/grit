package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.jdk.CollectionConverters.*
import scala.util.Using

import grit.core.id.{PeriodRef, PeriodSeq, TurnSeq, WorkflowId}
import grit.core.period.{LifecycleSettings, Period, PeriodState, Purgeable}
import grit.core.plugin.{PluginCursors, PostRef}
import grit.core.retention.{Retention, Target, Tombstone}
import grit.core.speech.SpeechStore
import grit.core.store.{
  ConversationStore,
  EntryStore,
  ModelProfileStore,
  PeriodStore,
  PromptStore,
  StoreError,
  Tombstones,
  Tx,
  UsageLedger
}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.ListWorkflowsInput

/** The collector: deletes what due tombstones name, and nothing else (ADR 0014). Each
  * tombstone's workflows go first, then their transaction steps' outputs
  * (`dbos.tx_step_outputs`, which DBOS's own delete leaves), then its rows with the
  * tombstone's end in one transaction, so a crash between them leaves the rest for the next
  * sweep, whose workflows DBOS no longer has. A tombstone any of whose workflows is still
  * queued or running, or that waits on another, is deferred to a later sweep.
  */
private[engine] final class Collector(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore,
    ledger: UsageLedger,
    speech: SpeechStore,
    profiles: ModelProfileStore,
    prompts: PromptStore,
    cursors: PluginCursors,
    tombstones: Tombstones
) {
  import Collector.*

  /** Every due tombstone of each kind, at most [[Batch]] a kind, collected, spared or
    * deferred at `now` under `settings`; then the tombstones ended longer ago than the ledger
    * window forgotten.
    */
  def once(settings: LifecycleSettings, now: Instant): Either[StoreError, Swept] =
    for {
      swept <- Kinds.foldLeft[Either[StoreError, Swept]](Right(Swept.nothing)) { (acc, kind) =>
        acc.flatMap { done =>
          kind.retention(settings.windows) match {
            // A plugin's documents, kept as its terms declare: not collected by this sweep.
            case Retention.Declared => Right(done)
            case Retention.For(window) =>
              Transact
                .read(dataSource)(tombstones.due(kind, now.minusMillis(window.toMillis), Batch))
                .flatMap(_.foldLeft[Either[StoreError, Swept]](Right(done)) { (acc, t) =>
                  acc.flatMap(done => collect(t, now).map(done + _))
                })
          }
        }
      }
      _ <- Transact.write(dataSource)(
        tombstones.forget(now.minusMillis(settings.windows.ledger.toMillis))
      )
    } yield swept

  private def collect(tombstone: Tombstone, now: Instant): Either[StoreError, Swept] = {
    val target = tombstone.target
    for {
      named <- Transact.read(dataSource)(workflows(target))
      found <- Transact.attempted(listed(named))
      swept <-
        if (found.exists(_._2)) deferred(target, now)
        else
          for {
            _ <- Transact.attempted(
              if (found.nonEmpty)
                client.deleteWorkflows(found.map((id, _) => WorkflowId.value(id)).asJava, false)
            )
            _ <- Transact.write(dataSource)(stepOutputs(named))
            outcome <- Transact.write(dataSource)(rows(target, now))
            swept <- outcome match {
              case Outcome.Collected => Right(Swept(collected = Vector(target)))
              case Outcome.Spared => Right(Swept(spared = Vector(target)))
              case Outcome.Waiting => deferred(target, now)
            }
          } yield swept
    } yield swept
  }

  private def deferred(target: Target, now: Instant): Either[StoreError, Swept] =
    Transact
      .write(dataSource)(tombstones.deferred(target, now))
      .map(_ => Swept(deferred = Vector(target)))

  /** The workflows `target` names, as the database says now. */
  private def workflows(target: Target)(using Tx^): Either[StoreError, Named] =
    target match {
      case Target.Raw(period) =>
        periods.get(period).map {
          case Some(p) =>
            p.state match {
              case PeriodState.Closed(last, _, _, _, _, _) =>
                val purgeable = Purgeable(period, p.first, last)
                Named(purgeable.turns, purgeable.attempts, purgeable.holds)
              case PeriodState.Open => Named.none
            }
          case None => Named.none
        }
      case Target.PostRuns(plugin, version, cursor) =>
        Right(Named(Vector.empty, Vector(PostRef.prefix(plugin, version, cursor)), _ => true))
      case Target.Disabled(plugin) =>
        Right(Named(Vector.empty, Vector(PostRef.prefix(plugin)), _ => true))
      case Target.Restarted(plugin) =>
        // Its runs at every version but its cursor's, which are not to be run again.
        cursors
          .stored()
          .map(_.find(_._1 == plugin) match {
            case Some((_, version)) =>
              Named(
                Vector.empty,
                Vector(PostRef.prefix(plugin)),
                id => PostRef.fromWorkflowId(id).exists(_.version != version)
              )
            case None => Named.none
          })
      case Target.Superseded(_) | Target.Quiet(_) | Target.Document(_) => Right(Named.none)
    }

  /** Each workflow DBOS has among `named`, and whether it is queued or running. */
  private def listed(named: Named): Vector[(WorkflowId, Boolean)] = {
    def list(input: ListWorkflowsInput) =
      client.listWorkflows(input).asScala.toVector.map { w =>
        (WorkflowId(w.workflowId()), Option(w.status()).forall(_.isActive()))
      }
    val byId =
      if (named.ids.isEmpty) Vector.empty
      else list(new ListWorkflowsInput().withWorkflowIds(named.ids.map(WorkflowId.value).asJava))
    val byPrefix =
      if (named.prefixes.isEmpty) Vector.empty
      else list(new ListWorkflowsInput().withWorkflowIdPrefix(named.prefixes.asJava))
    (byId ++ byPrefix).distinctBy(_._1).filter((id, _) => named.keep(id))
  }

  /** The transaction-step outputs of the workflows `named` names that DBOS no longer has,
    * deleted. DBOS keeps them in `dbos.tx_step_outputs` with no key to the workflow, so
    * deleting a workflow leaves them; they are matched by id, as the workflows were, so a
    * sweep that died after deleting the workflows leaves them to the next.
    */
  private def stepOutputs(named: Named)(using tx: Tx^): Either[StoreError, Int] =
    if (named.ids.isEmpty && named.prefixes.isEmpty) Right(0)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      def strings(values: Vector[String]): String =
        ujson.Arr.from(values.map(ujson.Str(_))).render()
      Transact.attempted {
        val gone = Using.resource(
          conn.prepareStatement(
            """SELECT DISTINCT t.workflow_id FROM dbos.tx_step_outputs t
              | WHERE (t.workflow_id IN (SELECT jsonb_array_elements_text(?::jsonb))
              |        OR EXISTS (SELECT 1 FROM jsonb_array_elements_text(?::jsonb) AS p(prefix)
              |                    WHERE starts_with(t.workflow_id, p.prefix)))
              |   AND NOT EXISTS (SELECT 1 FROM dbos.workflow_status s
              |                    WHERE s.workflow_uuid = t.workflow_id)""".stripMargin
          )
        ) { ps =>
          ps.setString(1, strings(named.ids.map(WorkflowId.value)))
          ps.setString(2, strings(named.prefixes))
          Using.resource(ps.executeQuery()) { rs =>
            val ids = Vector.newBuilder[WorkflowId]
            while (rs.next()) ids += WorkflowId(rs.getString(1))
            ids.result().filter(named.keep)
          }
        }
        if (gone.isEmpty) 0
        else
          Using.resource(
            conn.prepareStatement(
              """DELETE FROM dbos.tx_step_outputs
                | WHERE workflow_id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
            )
          ) { ps =>
            ps.setString(1, strings(gone.map(WorkflowId.value)))
            ps.executeUpdate()
          }
      }
    }

  /** `target`'s rows deleted and its tombstone ended, at `now`. */
  private def rows(target: Target, now: Instant)(using Tx^): Either[StoreError, Outcome] =
    target match {
      case Target.Raw(period) =>
        for {
          _ <- periods.purge(period, now)
          _ <- tombstones.collected(target, now)
        } yield Outcome.Collected
      case Target.Superseded(period) =>
        // Its turns read in the transaction that drops it.
        periods.get(period).flatMap {
          case None => tombstones.collected(target, now).map(_ => Outcome.Collected)
          case Some(p) =>
            p.state match {
              case PeriodState.Closed(last, _, _, _, order, Some(_)) =>
                val c = period.conversationId
                for {
                  _ <- cursors.forgetPosted(order)
                  _ <- periods.drop(period)
                  _ <- ledger.forget(c, p.first, last)
                  _ <- speech.forget(c, p.first, last)
                  _ <- profiles.forget(Purgeable(period, p.first, last).turns)
                  _ <- prompts.forget(Purgeable(period, p.first, last).turns)
                  _ <- tombstones.collected(target, now)
                } yield Outcome.Collected
              // Its raw entries are still kept: they go first.
              case _ => Right(Outcome.Waiting)
            }
        }
      case Target.Quiet(period) =>
        val c = period.conversationId
        for {
          // Under the conversation's lock, as every writer of it: no period opens meanwhile.
          _ <- entries.lockNext(c)
          all <- periods.all(c)
          outcome <- quiet(period, all) match {
            case Quiet.Gone => tombstones.collected(target, now).map(_ => Outcome.Collected)
            case Quiet.Alive => tombstones.spare(target, now).map(_ => Outcome.Spared)
            case Quiet.Waiting => Right(Outcome.Waiting)
            case Quiet.Removed(last) =>
              for {
                _ <- all.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, p) =>
                  p.state match {
                    case PeriodState.Closed(_, _, _, _, order, _) =>
                      acc.flatMap(_ => cursors.forgetPosted(order))
                    case PeriodState.Open => acc
                  }
                }
                _ <- ledger.forget(c, TurnSeq.First, last)
                _ <- speech.forget(c, TurnSeq.First, last)
                _ <- profiles.forget(Purgeable(period, TurnSeq.First, last).turns)
                _ <- prompts.forget(Purgeable(period, TurnSeq.First, last).turns)
                _ <- conversations.remove(c)
                _ <- tombstones.collected(target, now)
              } yield Outcome.Collected
          }
        } yield outcome
      case Target.Restarted(plugin) =>
        for {
          _ <- cursors.retire(plugin)
          _ <- tombstones.collected(target, now)
        } yield Outcome.Collected
      case Target.PostRuns(_, _, _) => tombstones.collected(target, now).map(_ => Outcome.Collected)
      case Target.Disabled(plugin) =>
        for {
          _ <- cursors.remove(plugin)
          _ <- tombstones.collected(target, now)
        } yield Outcome.Collected
      // Never due in `once`: its kind's retention is declared.
      case Target.Document(_) => Right(Outcome.Waiting)
    }
}

private[engine] object Collector {

  /** The most tombstones of one kind a sweep collects. */
  val Batch = 100

  /** The kinds collected, in order. */
  private val Kinds: Vector[Target.Kind] =
    Target.Kind.values.toVector

  /** Workflows by exact id, and by what their ids start with, those `keep` keeps. */
  private final case class Named(
      ids: Vector[WorkflowId],
      prefixes: Vector[String],
      keep: WorkflowId -> Boolean
  )

  private object Named {
    val none: Named = Named(Vector.empty, Vector.empty, _ => true)
  }

  private enum Outcome {
    case Collected, Spared, Waiting
  }

  /** What a quiet tombstone on `period` finds, `all` its conversation's periods still kept:
    * the conversation gone already; a later period opened, so it is alive; a period whose raw
    * entries are still kept, which go first (their workflows are deleted only through their
    * own tombstones); or removed, its newest turn `last`.
    */
  private enum Quiet {
    case Gone, Alive, Waiting
    case Removed(last: TurnSeq)
  }

  private def quiet(period: PeriodRef, all: Vector[Period]): Quiet =
    if (all.exists(p => PeriodSeq.value(p.ref.seq) > PeriodSeq.value(period.seq))) Quiet.Alive
    else
      all.find(_.ref == period).map(_.state) match {
        case None => Quiet.Gone
        case Some(PeriodState.Open) => Quiet.Alive
        case Some(PeriodState.Closed(last, _, _, _, _, _)) =>
          val raw = all.exists(_.state match {
            case PeriodState.Closed(_, _, _, _, _, purged) => purged.isEmpty
            case PeriodState.Open => false
          })
          if (raw) Quiet.Waiting else Quiet.Removed(last)
      }
}
