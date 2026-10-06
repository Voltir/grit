package grit.job.run

import java.time.Instant

import grit.core.durable.Durable
import grit.core.id.{EntryId, JobName, TurnRef, WorkflowId}
import grit.core.inbox.InboundId
import grit.core.job.{Job, JobRun, Jobs, Report, Slot}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.store.{Db, Entry, Jot, Origin, Payload, StoreError, Tx}

/** A job's run (ADR 0029): the workflow `run`, one per turn of a slot's conversation, which the
  * inbox started with the slot's opening ([[grit.core.inbox.Inbox.startSlot]]).
  */
object Run {

  /** The run's steps, as DBOS records their names, in the order they run. */
  object Step {
    val ReadSlot = "read-slot"
    val Reply = "reply"
  }

  /** The workflow of a job's run, the turn whose workflow id is `id`:
    *   1. `read-slot` — which slot it runs, its schedule, and the version it was started at;
    *   1. `reply` — under its job's current version in `jobs`: the job's reply (or the line
    *      saying its parameters could not be read) as the turn's reply, awaited where its
    *      schedule reports, and the slot marked replied, in one transaction. A reply already
    *      kept is returned first, whatever the version. Under another version, or with its job
    *      gone, it writes nothing: superseded.
    * What it returns says which. A store that fails fails the run (the clock edge then ends the
    * slot `failed`).
    */
  def body(env: RunEnv^, jobs: Jobs)(id: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(id) match {
      case None => s"not a run: ${WorkflowId.value(id)}"
      case Some(turn) =>
        import RunJournal.given
        val (records, db, jot, clock) = (env.records, env.db, env.jot, env.clock)
        d.step(Step.ReadSlot)(() => readSlot(records, db, turn)) match {
          case SlotRead.Unreadable(why) => s"unreadable: $why"
          case read: SlotRead.Read =>
            d.step(Step.Reply)(() => reply(records, jot, jobs, turn, read, clock.now())) match {
              case RunEnd.Replied(_) => s"replied: ${EntryId.value(turn.replyId)}"
              case RunEnd.Superseded(current) =>
                s"superseded: started at v${read.version}, its job at v$current"
              case RunEnd.Jobless => s"jobless: no job ${JobName.value(read.job)}"
              case RunEnd.Failed(why) => s"failed: $why"
            }
        }
    }

  /** The line a run replies when its job cannot read its schedule's parameters, `why`. */
  def unreadParams(job: JobName, why: String): String =
    s"This run of ${JobName.value(job)} could not read its parameters: $why"

  private def readSlot(records: RunRecords, db: Db^, turn: TurnRef): SlotRead =
    db.read {
      for {
        conversation <- records.conversations.get(turn.conversationId)
        entries <- records.entries.ofTurn(turn)
        found =
          for {
            c <- conversation.toRight("its conversation is gone")
            slot <- Slot.of(c.origin).toRight("its conversation is no slot's")
            job <- c.origin match {
              case Origin.Task(name, _) => JobName.of(name)
              case Origin.Tui(_, _) | Origin.Slack(_, _, _) => Left("its conversation is no task's")
            }
            opening <- entries.headOption.toRight("its turn has no opening")
            version <- InboundId
              .source(opening.id)
              .flatMap((_, source) => Slot.version(source))
              .toRight(s"its opening ${EntryId.value(opening.id)} names no version")
          } yield (slot, job, version, opening.createdAt)
        read <- found match {
          case Left(why) => Right(SlotRead.Unreadable(why))
          case Right((slot, job, version, started)) =>
            records.schedules.read(slot.schedule).map {
              case None => SlotRead.Unreadable("its schedule is gone")
              case Some(s) => SlotRead.Read(slot, job, version, started, s.params, s.report)
            }
        }
      } yield read
    }.fold(e => SlotRead.Unreadable(s"the store failed: ${describe(e)}"), identity)

  private def reply(
      records: RunRecords,
      jot: Jot^,
      jobs: Jobs,
      turn: TurnRef,
      read: SlotRead.Read,
      now: Instant
  ): RunEnd =
    jot
      .write {
        records.entries.get(turn.replyId).flatMap {
          case Some(kept) => Right(RunEnd.Replied(textOf(kept)))
          case None =>
            jobs.named(read.job) match {
              case None => Right(RunEnd.Jobless)
              case Some(job) if job.version != read.version => Right(RunEnd.Superseded(job.version))
              case Some(job) =>
                val text = say(job, read)
                for {
                  next <- records.entries.lockNext(turn.conversationId)
                  _ <- records.entries.insert(
                    Entry(
                      turn.replyId,
                      turn.conversationId,
                      turn.turnSeq,
                      None,
                      next.seq,
                      Payload.Message(said(text, read)),
                      now
                    )
                  )
                  _ <- awaited(records, turn, read.report)
                  _ <- records.schedules.replied(read.slot, read.version, now)
                } yield RunEnd.Replied(text)
            }
        }
      }
      .fold(e => RunEnd.Failed(describe(e)), identity)

  /** `job`'s reply to the run `read`, or the line saying its parameters could not be read. */
  private def say[P <: caps.Pure](job: Job[P], read: SlotRead.Read): String =
    job
      .read(read.params)
      .fold(
        why => unreadParams(read.job, why),
        params => job.reply(JobRun(params, read.slot.nominal, read.started))
      )

  /** `text` as the run's reply, written by its job at the run's version, at no cost. */
  private def said(text: String, read: SlotRead.Read): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage.Zero,
      s"job:${JobName.value(read.job)}@v${read.version}"
    )

  private def awaited(records: RunRecords, turn: TurnRef, report: Report)(using
      Tx^
  ): Either[StoreError, Unit] =
    report match {
      case Report.Kept => Right(())
      case Report.Posted(to) => records.deliveries.await(turn, to.address)
    }

  private def textOf(entry: Entry): String = entry.payload match {
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      blocks.collect { case AssistantBlock.Text(t) => t }.mkString
    case _ => ""
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
