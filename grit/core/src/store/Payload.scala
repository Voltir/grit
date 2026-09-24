package grit.core.store

import grit.core.message.Message

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
}
