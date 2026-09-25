package grit.core.store

import grit.core.id.{EntryId, ToolCallId, TurnSeq}
import grit.core.message.Message
import grit.core.topic.TopicEvent

/** What an [[Entry]] holds. Each case is one kind of entry; context fetched during
  * assembly is a future case.
  */
enum Payload {

  /** A message sent to or received from the model. */
  case Message(message: grit.core.message.Message)

  /** A short summary of the turn the entry belongs to, written after its reply. Not a
    * message: the model sees it only if an assembler chooses to show it.
    */
  case Summary(text: String)

  /** The search query assembly wrote for the turn the entry belongs to. A record of what
    * was searched for: never shown to the model, never itself searched.
    */
  case Query(text: String)

  /** The window the reply of the turn the entry belongs to was written from: the entries
    * the model saw before the turn's own, in the order it saw them, and which earlier
    * turns among them search `recalled` rather than recency. A record of what the model
    * saw: never shown to the model, never searched.
    */
  case Window(entries: Vector[EntryId], recalled: Vector[TurnSeq])

  /** What happened to the conversation's topics during the turn the entry belongs to
    * ([[grit.core.topic.Topics]] folds them). A record: never shown to the model, never
    * searched.
    */
  case Topic(events: Vector[TopicEvent])

  /** A reply inside the tool loop of the turn the entry belongs to that called tools; each
    * call's [[Result]] follows it. The turn's own later model calls and its summary read it;
    * no window includes it, so a call is never shown without its result. Never searched.
    */
  case Exchange(reply: grit.core.message.Message.Assistant)

  /** A tool call's result inside the tool loop of the turn the entry belongs to, read as
    * [[Exchange]] is. `shown` is its call in one line, as a person is shown it: the tool's
    * name and what the call acts on, or only the name as sent when the call did not read.
    */
  case Result(result: grit.core.message.Message.ToolResult, shown: String)

  /** A tool call that asks a person first began to run, before its result was recorded: a
    * record that a crash may have cut it short. Never shown to the model, never searched.
    */
  case Attempt(call: ToolCallId)

  /** A tool call that asks a person first, waiting for their answer: `shown` is what they
    * are asked to approve. Answered through [[grit.core.inbox.Inbox.answer]]; the call's
    * result follows once they answer or the wait runs out. Never shown to the model, never
    * searched.
    */
  case Ask(call: ToolCallId, shown: String)
}
