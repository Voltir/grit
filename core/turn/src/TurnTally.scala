package grit.turn

import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Cost, Message}
import grit.core.place.Place
import grit.core.store.{ConversationStore, EntryStore, Payload, StoreError, Tx, UsageLedger}

/** What a finished turn did: where, its model calls in its reply (`rounds`), the tools those
  * called by the names the model called them, in the order called, what all its model calls
  * cost, and what its body returned (`said`).
  */
final case class TurnTally(
    turn: TurnRef,
    place: Place,
    rounds: Int,
    tools: Vector[String],
    cost: Cost,
    said: String
) {

  /** "turn {workflow} at {place}: {rounds} rounds, tools {a}, {b}×{n}; {cost}; {said}", each
    * tool once in order of first call with ×n when called n > 1 times, "no tools" when none,
    * and "1 round" for one.
    */
  def line: String = {
    val called =
      if (tools.isEmpty) "no tools"
      else
        "tools " + tools.distinct
          .map(name =>
            tools.count(_ == name) match {
              case 1 => name
              case n => s"$name×$n"
            }
          )
          .mkString(", ")
    val counted = if (rounds == 1) "1 round" else s"$rounds rounds"
    s"turn ${WorkflowId.value(turn.workflowId)} at ${place.written}: $counted, $called; " +
      s"${cost.written}; $said"
  }
}

object TurnTally {

  /** `turn`, whose body returned `said`, as its stores hold it: its conversation's place;
    * `rounds`, the assistant messages it recorded (each round's reply that called tools, then
    * its reply or draft, counted once when a posted draft is also its reply); the tools those
    * called; and the cost of every row `ledger` holds for its workflow (query, rounds, topic
    * verdict, judge, summary). `Left` when a store fails, or [[StoreError.Invalid]] when its
    * conversation is gone.
    */
  def read(
      conversations: ConversationStore,
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      said: String
  )(using Tx^): Either[StoreError, TurnTally] =
    for {
      found <- conversations.get(turn.conversationId)
      conversation <- found.toRight(
        StoreError.Invalid(s"turn ${WorkflowId.value(turn.workflowId)}'s conversation is gone")
      )
      listed <- entries.list(turn.conversationId)
      rows <- ledger.of(turn.workflowId)
    } yield {
      val own = listed.filter(_.turnSeq == turn.turnSeq).map(_.payload)
      val exchanges = own.collect { case Payload.Exchange(reply) => reply }
      val answered = own.exists {
        case Payload.Message(_: Message.Assistant) | Payload.Draft(_) => true
        case _ => false
      }
      TurnTally(
        turn,
        conversation.origin.place,
        exchanges.size + (if (answered) 1 else 0),
        exchanges.flatMap(_.blocks.collect { case AssistantBlock.ToolCall(_, name, _) => name }),
        Cost.total(rows.map(_.usage)),
        said
      )
    }
}
