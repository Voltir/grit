package grit.eval.harness.sent

import java.time.{Duration, Instant}

import grit.core.id.TurnSeq
import grit.core.message.{Cost, Tokens}
import grit.core.store.{Entry, Payload}

/** One turn of a task as [[TaskLines]] reads it: `turn`; its calls measured
  * ([[TurnSent.measured]]); whether its window was drawn at a boundary (`boundary`); the
  * earlier turns of its conversation that window left out (`dropped`, none for a carried
  * window); and when each call's reply was kept, in order (`kept`).
  */
final case class TaskTurn(
    turn: TurnSeq,
    calls: Vector[MeasuredCall],
    boundary: Boolean,
    dropped: Vector[TurnSeq],
    kept: Vector[Instant]
)

object TaskTurn {

  /** Turn `turn`, its `calls` measured, read against `window`, the window record it was drawn
    * from, and `entries`, every entry of its conversation. A window recorded before windows
    * were carried counts as drawn at a boundary; a turn with no window record as not.
    */
  def of(
      turn: TurnSeq,
      window: Option[Payload.Window],
      calls: Vector[MeasuredCall],
      entries: Vector[Entry]
  ): TaskTurn = {
    val turnOf = entries.map(e => e.seq -> e.turnSeq).toMap
    val keptAt = entries.map(e => e.id -> e.createdAt).toMap
    val boundary = window.exists(_.carry.forall(_.after.isEmpty))
    val dropped = window.filter(_ => boundary).fold(Vector.empty[TurnSeq]) { w =>
      val shown = (w.entries.flatMap(turnOf.get) ++ w.recalled).toSet
      entries
        .map(_.turnSeq)
        .distinct
        .filter(t => TurnSeq.value(t) < TurnSeq.value(turn) && !shown.contains(t))
    }
    TaskTurn(turn, calls, boundary, dropped, calls.flatMap(c => keptAt.get(c.reply)))
  }

  /** The turns of `conversation`, each read as [[of]] says. */
  def all(conversation: ConversationSent): Vector[TaskTurn] =
    conversation.turns.map(t => of(t.turn.turnSeq, t.window, t.measured, conversation.entries))
}

/** A task's turns, as a person reads them in a terminal: what they held in the cache. */
object TaskLines {

  /** `turns`, a conversation's, oldest first: a line for the task, then a line a turn.
    *
    * The task's line: its turns and calls, its cache as [[SentMarkdown.cacheOf]] says, and the
    * longest gap between one call's reply and the next's, with the turn it ends in (a gap past
    * five minutes outlives Anthropic's default cache). A turn's line: its calls; whether its
    * window was drawn at a boundary or carried, and a boundary's dropped turns, flagged
    * `DROPPED`; its first call's read share of its input and what that call's misses cost; and,
    * from the second turn on, its carry-over: of what the turn before's last call left in the
    * cache, how much its first call did not read ([[MeasuredCall.resent]]). A figure no ledger
    * row gives is said as `unmeasured`.
    */
  def lines(turns: Vector[TaskTurn]): Vector[String] = {
    val calls = turns.flatMap(_.calls)
    val kept = turns.flatMap(t => t.kept.map(_ -> t.turn))
    val gap = kept
      .zip(kept.drop(1))
      .map { case ((a, _), (b, t)) => (Duration.between(a, b), t) }
      .maxByOption(_._1)
      .fold("no two replies kept")((d, t) => s"${said(d)} (into turn ${TurnSeq.value(t)})")
    val task = s"task: ${turns.size} turns, ${calls.size} calls; cache " +
      SentMarkdown.cacheOf(calls).getOrElse("unmeasured") + s"; longest gap between replies $gap"
    val before = None +: turns.map(t => Some(t))
    task +: turns.zip(before).map((t, previous) => line(t, previous))
  }

  private def line(t: TaskTurn, previous: Option[TaskTurn]): String = {
    val first = t.calls.headOption.flatMap(_.row)
    val drawn =
      if (!t.boundary) "carried"
      else if (t.dropped.isEmpty) "boundary"
      else s"boundary, DROPPED turns ${t.dropped.map(TurnSeq.value).mkString(", ")}"
    val read = first.fold("first call unmeasured")(r =>
      s"first call read ${SentMarkdown.readShare(r.usage)}" +
        s" of ${Tokens.value(r.usage.input)}"
    )
    val missed = t.calls.headOption
      .flatMap(_.missed)
      .fold("misses unpriced")(usd => s"misses cost ${Cost.cents(usd)}")
    val carry = previous.map { p =>
      (p.calls.flatMap(_.row).lastOption, first) match {
        case (Some(left), Some(now)) =>
          val held = Tokens.value(left.usage.cacheRead) + Tokens.value(left.usage.cacheWrite)
          s"; carry-over: ${Tokens.value(MeasuredCall.resent(left.usage, now.usage))} of $held " +
            "the turn before left in the cache not read"
        case _ => "; carry-over unmeasured"
      }
    }
    s"turn ${TurnSeq.value(t.turn)} ($drawn): ${t.calls.size} calls; $read, $missed" +
      carry.getOrElse("")
  }

  /** `d` as minutes and seconds. */
  private def said(d: Duration): String = {
    val s = d.getSeconds.max(0L)
    if (s < 60) s"${s}s" else s"${s / 60}m ${s % 60}s"
  }
}
