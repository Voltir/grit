package grit.app.chat

import java.time.Duration

import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Message, Tokens}
import grit.core.provider.TokenEstimator
import grit.core.store.{Entry, Payload, UsageLedger}
import grit.dbos.engine.RecordedStep
import grit.turn.Turn

/** One turn as the turn panel shows it: where it is, what each step took, what it searched
  * for, what its window held and what it cost. Pure data, built by [[TurnView.of]] from
  * what the store, DBOS and the ledger recorded.
  *
  * @param turn the turn's position in the conversation, from 0
  * @param running the step it is in, while it runs
  * @param steps the steps it recorded, in order
  * @param query what assembly searched for, when it searched
  * @param window what the model saw, once the reply is recorded
  * @param spent what its model calls cost, when the provider said
  * @param billed the input tokens the provider counted for the reply
  */
final case class TurnView(
    turn: TurnSeq,
    running: Option[String],
    steps: Vector[TurnView.Step],
    query: Option[String],
    window: Option[TurnView.Window],
    spent: Option[BigDecimal],
    billed: Option[Tokens]
) {

  /** Whether nothing more will change: finished, every step recorded. */
  def settled: Boolean = running.isEmpty && steps.size >= Turn.Step.all.size
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
    val priced = costs.flatMap(_.usage.costUsd)
    TurnView(
      turn.turnSeq,
      Option.when(running)(Turn.running(steps.map(_.name))),
      steps.map(s => Step(s.name, s.started.zip(s.completed).map(Duration.between(_, _).toMillis))),
      own.collectFirst { case Entry(_, _, _, _, _, Payload.Query(q), _) => q },
      window,
      Option.when(priced.nonEmpty)(priced.sum),
      reply.map(_.usage.input)
    )
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
