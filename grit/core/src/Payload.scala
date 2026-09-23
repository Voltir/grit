package grit.core

/** What an [[Entry]] holds. Each case is one kind of entry; turn summaries and
  * context fetched during assembly are future cases.
  */
enum Payload {
  case Message(message: grit.core.Message)
}
