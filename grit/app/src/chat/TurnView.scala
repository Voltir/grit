package grit.app.chat

import java.time.Duration

import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Message, Tokens}
import grit.core.provider.TokenEstimator
import grit.core.store.{Entry, Payload, UsageLedger}
import grit.dbos.engine.RecordedStep
import grit.turn.Turn

/** One turn as the turn panel shows it: where it is, what each step took, what it searched
  * for, what its window held and what it cost. Pure data, built by [[TurnView.of]] from
  * what the store, DBOS and the ledger recorded.
  *
  * @param turn the turn's position in the conversation, from 0
  * @param asked the user's message that started it
  * @param running the step it is in, while it runs
  * @param steps the steps it recorded, in order, named as [[Turn.Step.named]] shows them
  * @param query what assembly searched for, when it searched
  * @param window what the model saw, once the reply is recorded
  * @param spent what its model calls cost, once one was made
  * @param billed the input tokens the provider counted for the reply
  */
final case class TurnView(
    turn: TurnSeq,
    asked: String,
    running: Option[String],
    steps: Vector[TurnView.Step],
    query: Option[String],
    window: Option[TurnView.Window],
    spent: Option[Cost],
    billed: Option[Tokens]
) {

  /** Whether nothing more will change: finished, its last step recorded. A turn that
    * failed short of it is read again, which costs a poll's queries and nothing more.
    */
  def settled: Boolean = running.isEmpty && steps.exists(_.name == Turn.Step.AppendSummary)
}

object TurnView {

  /** A recorded step and how long it took, when DBOS kept both ends. */
  final case class Step(name: String, ms: Option[Long])

  /** What the reply's request held, estimated: the system prompt, the recent turns, the
    * turns search recalled (`recalledTurns`), and the turn's own messages.
    */
  final case class Window(
      system: Tokens,
      recent: Tokens,
      recalled: Tokens,
      message: Tokens,
      recalledTurns: Vector[TurnSeq]
  ) {
    def total: Tokens = system + recent + recalled + message
  }

  /** The latest turn in `entries`: the one its last user message started. */
  def latest(entries: Vector[Entry]): Option[TurnRef] =
    entries.reverseIterator
      .collectFirst { case e @ Entry(_, _, _, _, _, Payload.Message(Message.User(_)), _) => e }
      .map(e => TurnRef(e.conversationId, e.turnSeq))

  /** `turn`, from every entry of its conversation, the `steps` its workflow recorded,
    * whether it is `running`, and its ledger rows `costs`. The window is estimated with
    * `estimator`, as assembly estimated it, under the system prompt `system`.
    */
  def of(
      turn: TurnRef,
      entries: Vector[Entry],
      steps: Vector[RecordedStep],
      running: Boolean,
      costs: Vector[UsageLedger.Row],
      system: String,
      estimator: TokenEstimator
  ): TurnView = {
    val own = entries.filter(_.turnSeq == turn.turnSeq)
    val byId: Map[EntryId, Entry] = entries.map(e => e.id -> e).toMap
    val window =
      own.collectFirst { case Entry(_, _, _, _, _, w: Payload.Window, _) => w }.map { w =>
        val seen = w.entries.flatMap(byId.get)
        val (recalled, recent) = seen.partition(e => w.recalled.contains(e.turnSeq))
        Window(
          estimator.system(system),
          tokens(recent, estimator),
          tokens(recalled, estimator),
          tokens(own.filter(isUser), estimator),
          w.recalled
        )
      }
    val reply = own.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _) => m
    }
    val spent = Option.when(costs.nonEmpty)(Cost.total(costs.map(_.usage)))
    TurnView(
      turn.turnSeq,
      own
        .collectFirst { case Entry(_, _, _, _, _, Payload.Message(Message.User(t)), _) => t }
        .getOrElse(""),
      Option.when(running)(Turn.running(steps.map(_.name))),
      timed(steps),
      own.collectFirst { case Entry(_, _, _, _, _, Payload.Query(q), _) => q },
      window,
      spent,
      reply.map(_.usage.input)
    )
  }

  /** `recorded` as the panel shows them ([[Turn.Step.named]]), each timed from its start to
    * its end; a person's wait from the end of the ask before it, since DBOS keeps no start
    * for it that the panel can trust.
    */
  private def timed(recorded: Vector[RecordedStep]): Vector[Step] = {
    val names = Turn.Step.named(recorded.map(_.name))
    recorded.zip(names).zipWithIndex.map { case ((step, name), i) =>
      val from =
        if (Turn.Step.family(name).contains(Turn.Step.Wait))
          recorded
            .take(i)
            .zip(names)
            .findLast((_, n) => Turn.Step.family(n).contains(Turn.Step.Ask))
            .flatMap(_._1.completed)
        else step.started
      Step(name, from.zip(step.completed).map(Duration.between(_, _).toMillis))
    }
  }

  private def isUser(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) => true
    case _ => false
  }

  private def tokens(entries: Vector[Entry], estimator: TokenEstimator): Tokens =
    entries
      .collect { case Entry(_, _, _, _, _, Payload.Message(m), _) => estimator.message(m) }
      .foldLeft(Tokens.Zero)(_ + _)
}
