package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.model.Grade
import grit.core.spend.{Day, Spend, Spending, TurnSpend}

/** An in-memory [[UsageLedger]] for tests, keeping [[StoreContract]], and the [[Spending]]
  * read from it, keeping `SpendingContract`. It ignores the `Tx`. A row is recorded at
  * [[now]], as the SQL store's are at the transaction's time.
  */
final class InMemoryUsageLedger extends UsageLedger, Spending {

  /** When the rows recorded from now on are recorded. */
  // Replaced whole by the test's own calls, on its one thread; nothing else holds it.
  @caps.unsafe.untrackedCaptures
  var now: Instant = Instant.EPOCH

  @caps.unsafe.untrackedCaptures
  private var recordedAt = Map.empty[EntryId, Instant]

  /** When the row for `entry` was recorded; `None` for none. */
  def recordedOn(entry: EntryId): Option[Instant] = recordedAt.get(entry)

  def on(day: Day)(using Tx^): Either[StoreError, Spend] =
    Right(spent(rows.filter { row =>
      recordedAt.get(row._1).exists(at => !at.isBefore(day.from) && at.isBefore(day.until))
    }))

  def turn(turn: TurnRef)(using tx: Tx^): Either[StoreError, TurnSpend] =
    of(turn.workflowId).map(TurnSpend.of(turn, _))

  def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend] =
    Right(spent(rows.filter(_._6.conversationId == id)))

  private def spent(of: Vector[(EntryId, WorkflowId, String, Usage, Tokens, TurnRef)]): Spend =
    of.foldLeft(Spend.Zero)((s, row) => s + Spend(1, grit.core.message.Cost.of(row._4)))

  // Replaced whole by the fake's own methods, which the test calls on its one thread.
  @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[(EntryId, WorkflowId, String, Usage, Tokens, TurnRef)]

  /** Who served each row's response, by its entry, when its provider reported it. */
  @caps.unsafe.untrackedCaptures
  private var upstreams = Map.empty[EntryId, String]

  /** The grade of the pin each row's call was made under, by its entry, when it had one. */
  @caps.unsafe.untrackedCaptures
  private var grades = Map.empty[EntryId, Grade]

  def record(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      model: String,
      upstream: Option[String],
      grade: Option[Grade],
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit] =
    if (rows.exists(_._1 == entry)) Left(StoreError.DuplicateId(entry))
    else {
      rows = rows :+ (entry, workflow, model, usage, estimatedInput, turn)
      upstreams = upstream.fold(upstreams)(u => upstreams.updated(entry, u))
      grades = grade.fold(grades)(g => grades.updated(entry, g))
      recordedAt = recordedAt.updated(entry, now)
      Right(())
    }

  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]] =
    Right(rows.collect {
      case (entry, w, model, usage, estimate, _) if w == workflow =>
        UsageLedger.Row(entry, model, upstreams.get(entry), grades.get(entry), usage, estimate)
    })

  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit] = {
    rows = rows.filterNot { row =>
      val t = row._6
      t.conversationId == conversation &&
      TurnSeq.value(t.turnSeq) >= TurnSeq.value(from) && TurnSeq.value(t.turnSeq) <= TurnSeq.value(
        to
      )
    }
    Right(())
  }
}
