package grit.core.store

import grit.core.id.{EntryId, TurnSeq}
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
}
