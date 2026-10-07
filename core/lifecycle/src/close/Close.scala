package grit.lifecycle.close

import java.time.Instant

import grit.core.durable.Durable
import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq, WorkflowId}
import grit.core.job.Slot
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{Balance, CloseReason, Closing, Edit, Flows}
import grit.core.retention.Target
import grit.core.store.{
  Entry,
  EntryTopics,
  Payload,
  Reads,
  Sealed,
  Speakers,
  StoreError,
  Tombstones,
  Tx
}
import grit.core.triage.Earning
import grit.core.visibility.Subject
import grit.lifecycle.transcript.PeriodTranscript

/** The close: one workflow per attempt to close a period ([[CloseRef]]), run on the turns'
  * queue under its conversation, so never beside one of its turns. Each step's output is
  * recorded, so a close resumed after a crash never calls a model twice.
  *
  *   1. `check` — the period, under its conversation's lock: already closed; abandoned when
  *      its deadline ([[grit.core.period.Deadline]], under the settings in force) is no longer
  *      the attempt's, because a turn came in, a turn's entries were written or the settings
  *      changed, so the sweep makes a new attempt on the new deadline; otherwise due, for its
  *      reason, with the balance it opened with and the cap its closing's balance is held
  *      to. The reason is `Ran`, whatever the deadline's, when the period's conversation is a
  *      job's run ([[grit.core.job.Slot.of]] its origin); otherwise `Unearned`, whatever the
  *      deadline's, when the period did not earn a written closing
  *      ([[grit.core.triage.Earning]], over what triage made of its heard messages). The
  *      attempt is taken to be enqueued once its deadline had come.
  *   1. `gate` — what of the closing is new beside the balance the period opened with and
  *      what its windows showed from other conversations ([[CloseGate]]); every part when
  *      the classifier does not answer; none, with no classifier call, when unearned or ran.
  *   1. `summarise` — the closing: its flows written by the catalog's summary pin
  *      ([[ClosingSummary]]), shown what the period drew from elsewhere as known, and the balance it opened with after the writer's edits and
  *      the topics' ([[grit.core.store.EntryTopics.edits]]), held to the cap
  *      ([[grit.core.period.Balance]]); when the model fails, writes nothing readable or
  *      is cut off at its token limit (its cost still kept), or when the gate found
  *      nothing new (and then no model is called), the period's per-turn summaries joined
  *      as the prose, and the balance carried with the topics' edits alone. A period grit
  *      only heard ([[grit.lifecycle.transcript.PeriodTranscript.overheard]]) that earned
  *      its closing is written by the catalog's heard pin, as reported speech, asking only its
  *      prose and outcome; one that did not is closed with no model call, its prose
  *      `Heard 4 messages; nothing kept.` and its balance carried. A run's period is closed
  *      with no model call: its prose the run's opening, its outcome the run's reply verbatim
  *      (`No reply.` without one), and its balance carried unchanged. A close never fails for
  *      a model.
  *   1. `seal` — under the lock again: the closing entry, its cost in the ledger, the period
  *      closed, and the tombstones on its raw entries, on the closing it replaces and on its
  *      conversation going quiet ([[grit.core.retention.Target]]), together; abandoned,
  *      writing nothing, when a turn came in meanwhile.
  */
object Close {

  /** The close's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Check = "check"
    val Gate = "gate"
    val Summarise = "summarise"
    val Seal = "seal"
  }

  /** The close's changes since its epoch began, each the name its `Durable.patch` takes. */
  object Patches {

    /** A job's run's period closes `Ran`, its closing written without a model (2026-10-06). */
    val RunClose = "run-close"
  }

  /** What `check` found. */
  enum Checked {

    /** The period closes now for `reason`; its turns start at `first`; it opened with
      * `known`, and its balance is held to `cap` bytes.
      */
    case Due(first: TurnSeq, reason: CloseReason, known: Balance, cap: Int)

    /** The period is closed already. */
    case Closed

    /** The attempt is over without closing, for `why`. */
    case Abandoned(why: String)
  }

  /** What a summary cost: its `model`, its `usage`, and the estimate of its request. */
  final case class Cost(model: String, usage: Usage, estimate: Tokens)

  /** What `summarise` wrote: the `closing` (`None` only when the period had no text at all
    * and no turns), what it cost when a model wrote it, and why no model did.
    */
  final case class Summarised(closing: Option[Closing], cost: Option[Cost], note: Option[String])

  /** The close workflow's body, for the attempt whose workflow id is `workflowId`. Returns
    * what the close did, for logs: the closing entry is in the store.
    */
  def body(env: CloseEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    CloseRef.fromWorkflowId(workflowId) match {
      case None => s"not a close: ${WorkflowId.value(workflowId)}"
      case Some(attempt) =>
        import CloseJournal.given
        val records = env.records
        val runs = d.patch(Patches.RunClose)
        d.transact(Step.Check, Subject.Conversation(attempt.period.conversationId))(
          check(records, attempt, runs)
        ) match {
          case Left(why) => s"failed: $why"
          case Right(Checked.Closed) => "already closed"
          case Right(Checked.Abandoned(why)) => s"abandoned: $why"
          case Right(Checked.Due(first, reason, known, cap)) =>
            val gated = d.step(Step.Gate) { () =>
              if (reason == CloseReason.Unearned) (Asked.NoPart, Some("unearned"))
              else if (reason == CloseReason.Ran) (Asked.NoPart, Some("ran"))
              else {
                val entries = own(env, attempt, first)
                CloseGate.asked(
                  env.classifier,
                  CloseGate.Transcript(
                    known,
                    elsewhere(env, attempt, entries),
                    transcript(env, attempt, entries)
                  )
                )
              }
            }
            val summarised = d.step(Step.Summarise) { () =>
              summarise(env, attempt, first, gated._1, known, cap, reason)
            }
            val noted = (gated._2 ++ summarised.note).map(n => s"; $n").mkString
            d.transact(Step.Seal, Subject.Conversation(attempt.period.conversationId))(
              seal(records, attempt, reason, summarised, env.clock.now())
            ) match {
              case Left(why) => s"failed: $why$noted"
              case Right(Sealed.Closed(entry)) => s"closed: ${EntryId.value(entry)}$noted"
              case Right(Sealed.Abandoned) =>
                s"abandoned: a turn came in while it was summarised$noted"
            }
        }
    }

  /** The `check` step; a run's period is `Ran` only when `runs` (the patch is taken). */
  private def check(records: CloseRecords, attempt: CloseRef, runs: Boolean)(using
      Tx^
  ): Either[String, Checked] =
    (for {
      _ <- records.entries.lockNext(attempt.period.conversationId)
      period <- records.periods.get(attempt.period)
      activity <- records.periods.activity(attempt.period)
      settings <- records.lifecycle.current()
      opening <- period.fold[Either[StoreError, Option[Balance]]](Right(None))(p =>
        records.periods
          .opening(TurnRef(attempt.period.conversationId, p.first))
          .map(o => Some(o.balance))
      )
      // Its own entries and what triage made of them, read under the lock, so a message
      // heard after this check is in a later attempt's.
      own <- period.fold[Either[StoreError, Vector[Entry]]](Right(Vector.empty))(p =>
        records.entries
          .list(attempt.period.conversationId)
          .map(_.filter { e =>
            val t = TurnSeq.value(e.turnSeq)
            t >= TurnSeq.value(p.first) && t <= TurnSeq.value(attempt.last)
          })
      )
      tags <- records.triage.of(own.collect { case e @ Entry(_, _, _, _, _, Payload.Heard(_), _) =>
        e.id
      })
      ran <-
        if (!runs) Right(false)
        else
          records.conversations
            .get(attempt.period.conversationId)
            .map(_.flatMap(c => Slot.of(c.origin)).isDefined)
    } yield (period, activity) match {
      case (Some(p), Some(a)) =>
        val current = a.attempt(settings)
        if (current == attempt)
          Checked.Due(
            p.first,
            if (ran) CloseReason.Ran
            else if (Earning.earns(own, tags)) a.due(settings).reason
            else CloseReason.Unearned,
            opening.getOrElse(Balance.empty),
            settings.balance
          )
        else if (a.last != attempt.last) Checked.Abandoned(s"turn ${TurnSeq.value(a.last)} came in")
        else Checked.Abandoned(s"its deadline moved to ${current.due}")
      case (Some(_), None) => Checked.Closed
      case (None, _) => Checked.Abandoned("no such period")
    }).left.map(describe)

  /** The entries of the attempt's period, from its turn `first` to its last. */
  private def own(
      env: CloseEnv^,
      attempt: CloseRef,
      first: TurnSeq
  ): Either[String, Vector[Entry]] =
    PeriodTranscript
      .entries(reads(env, attempt), env.records.entries, attempt.period, first, attempt.last)
      .left
      .map(describe)

  /** What the period's windows showed from other conversations
    * ([[PeriodTranscript.elsewhere]]); none when unread.
    */
  private def elsewhere(
      env: CloseEnv^,
      attempt: CloseRef,
      entries: Either[String, Vector[Entry]]
  ): Vector[String] =
    entries
      .flatMap(
        PeriodTranscript.elsewhere(reads(env, attempt), env.records.entries, _).left.map(describe)
      )
      .getOrElse(Vector.empty)

  /** The period as one transcript ([[PeriodTranscript.of]]); empty when unread. */
  private def transcript(
      env: CloseEnv^,
      attempt: CloseRef,
      entries: Either[String, Vector[Entry]]
  ): String = {
    val own = entries.getOrElse(Vector.empty)
    PeriodTranscript.of(own, names(env, attempt, own))
  }

  /** Who wrote `entries`; nobody named when that cannot be read, so each line is then its
    * role's.
    */
  private def names(env: CloseEnv^, attempt: CloseRef, entries: Vector[Entry]): Speakers =
    PeriodTranscript
      .speakers(reads(env, attempt), env.records.principals, entries)
      .getOrElse(Speakers.none)

  /** The store as the attempt's period's conversation reads it: work nobody asked for, kept at
    * that conversation's label.
    */
  private def reads(env: CloseEnv^, attempt: CloseRef): Reads^ =
    env.db.as(Subject.Conversation(attempt.period.conversationId))

  /** The `summarise` step, for a period closing for `reason`: the closing of the period's
    * turns `first` to the attempt's last, from the flows and edits the summary model writes,
    * asking for `asked`, applied to `known` and held to `cap`; or, when it fails, the fallback
    * prose ([[fallback]]) with `known` carried unedited. A run's ([[ran]]) and an unearned one
    * are written without a model.
    */
  private def summarise(
      env: CloseEnv^,
      attempt: CloseRef,
      first: TurnSeq,
      asked: Asked,
      known: Balance,
      cap: Int,
      reason: CloseReason
  ): Summarised = {
    val entries = own(env, attempt, first)
    // What the writer reads and cites into: one value, cut once (ClosingSummary.visible).
    val labelled = {
      val own = entries.getOrElse(Vector.empty)
      PeriodTranscript.labelled(own, names(env, attempt, own))
    }
    val period = attempt.period.seq
    // The topics' edits are the fold's, free and exact: made whether or not a model writes.
    val topics =
      EntryTopics.edits(
        known,
        entries.getOrElse(Vector.empty).flatMap(e => EntryTopics.events(e.payload))
      )
    def closed(prose: String, outcome: Option[String], edits: Vector[Edit]): Option[Closing] = {
      val edited = known.edit(edits ++ topics, period)
      val fitted = edited.balance.fit(cap, period)
      Flows
        .of(prose, outcome, edited.changes ++ fitted.changes)
        .map(Closing(_, fitted.balance))
    }
    // A period grit only heard is always written, under the heard pin, asking for its prose
    // and outcome alone: nothing said to each other stands as the conversation's own.
    val overheard = PeriodTranscript.overheard(entries.getOrElse(Vector.empty))
    val asking = if (overheard) asked.heard else asked
    val request =
      ClosingSummary.request(labelled, known, elsewhere(env, attempt, entries), asking, overheard)
    def carried(note: String) = Summarised(
      fallback(entries.getOrElse(Vector.empty), attempt, first)
        .flatMap(closed(_, None, Vector.empty)),
      None,
      Some(note)
    )
    if (reason == CloseReason.Ran)
      entries.fold(
        why =>
          Summarised(
            fallback(Vector.empty, attempt, first).flatMap(closed(_, None, Vector.empty)),
            None,
            Some(s"unread: $why")
          ),
        own => Summarised(ran(own, attempt, first, known), None, None)
      )
    else if (reason == CloseReason.Unearned)
      Summarised(
        fallback(entries.getOrElse(Vector.empty), attempt, first)
          .flatMap(closed(_, None, Vector.empty)),
        None,
        None
      )
    else if (asked.nothingNew && !overheard) carried("nothing new: carried")
    else {
      val written = for {
        catalog <- env.models.catalog()
        pin = if (overheard) catalog.heardPin else catalog.pin.summary
        reply <- env.models.provider(pin).complete(request).left.map(_.cause)
      } yield {
        val cost = Cost(reply.model, reply.usage, env.records.estimator.request(request))
        // A reply cut off at its token limit may have lost any part, a Resolved one
        // included, and its last line may be half written: none of it is read.
        if (reply.stop == StopReason.MaxTokens)
          carried("no summary: cut off at its token limit").copy(cost = Some(cost))
        else
          ClosingSummary
            .read(reply, known, asking, labelled)
            .flatMap(read => closed(read.prose, read.outcome, read.edits))
            .fold(carried(s"no summary: the summary had no prose (stop: ${reply.stop})"))(c =>
              Summarised(Some(c), Some(cost), None)
            )
      }
      written.fold(why => carried(s"no summary: $why"), identity)
    }
  }

  /** A run's closing, written without a model: its prose the run's opening (the first message
    * said to grit; without one, the [[fallback]]), its outcome the run's reply, its last
    * assistant message's text, verbatim ("No reply." without one), and `known` carried
    * unchanged.
    */
  private def ran(
      entries: Vector[Entry],
      attempt: CloseRef,
      first: TurnSeq,
      known: Balance
  ): Option[Closing] = {
    val opening = entries.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(Message.User(text)), _) => text
    }
    val reply = entries.reverse.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(Message.Assistant(blocks, _, _, _, _)), _) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
    }
    opening
      .orElse(fallback(entries, attempt, first))
      .flatMap(prose => Flows.of(prose, Some(reply.getOrElse("No reply.")), Vector.empty))
      .map(Closing(_, known))
  }

  /** A closing's prose written without a model: for a period grit only heard, how many
    * messages it heard ("Heard 4 messages; nothing kept."); otherwise the period's per-turn
    * summaries joined; without any, its user messages; without those, how many turns it had.
    */
  private def fallback(
      entries: Vector[Entry],
      attempt: CloseRef,
      first: TurnSeq
  ): Option[String] =
    if (PeriodTranscript.overheard(entries)) {
      val heard = entries.count(_.payload match {
        case Payload.Heard(_) => true
        case _ => false
      })
      Some(s"Heard $heard message${if (heard == 1) "" else "s"}; nothing kept.")
    } else {
      val summaries =
        entries.collect { case Entry(_, _, _, _, _, Payload.Summary(text), _) => text }
      val asked = entries.collect {
        case Entry(_, _, _, _, _, Payload.Message(Message.User(text)), _) => text
      }
      val turns = TurnSeq.value(attempt.last) - TurnSeq.value(first) + 1
      Vector(
        summaries.mkString(" "),
        asked.mkString(" / "),
        s"A period of $turns turns, with nothing kept."
      )
        .find(_.trim.nonEmpty)
    }

  /** The `seal` step, at `now`: the closing entry and its cost, and the period closed. */
  private def seal(
      records: CloseRecords,
      attempt: CloseRef,
      reason: CloseReason,
      summarised: Summarised,
      now: Instant
  )(using Tx^): Either[String, Sealed] =
    summarised.closing match {
      case None => Left("the period left nothing to close with")
      case Some(closing) =>
        (for {
          outcome <- records.periods.seal(attempt, reason, closing, now)
          _ <- (outcome, summarised.cost) match {
            case (Sealed.Closed(entry), Some(c)) =>
              records.ledger.record(
                entry,
                TurnRef(attempt.period.conversationId, attempt.last),
                attempt.workflowId,
                c.model,
                c.usage,
                c.estimate
              )
            case _ => Right(())
          }
          _ <- outcome match {
            case Sealed.Closed(_) => tombstones(records.tombstones, attempt.period, now)
            case Sealed.Abandoned => Right(())
          }
        } yield outcome).left.map(describe)
    }

  /** What a seal of `period` at `now` marks for deletion: its raw entries, the closing of the
    * period before it, which it replaces, and its conversation, quiet from now on unless
    * another period opens.
    */
  private def tombstones(tombstones: Tombstones, period: PeriodRef, now: Instant)(using
      Tx^
  ): Either[StoreError, Unit] =
    for {
      _ <- tombstones.write(Target.Raw(period), now)
      _ <- PeriodSeq.of(PeriodSeq.value(period.seq) - 1) match {
        case Some(before) =>
          tombstones.write(Target.Superseded(PeriodRef(period.conversationId, before)), now)
        case None => Right(())
      }
      _ <- tombstones.write(Target.Quiet(period), now)
    } yield ()

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
