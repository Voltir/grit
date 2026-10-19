package grit.knowledge.topic

/** What a topic's rater says of it, kept as the word its review keeps: `right`, `split`,
  * `merge` or `noise`.
  */
enum TopicVerdict extends caps.Pure {

  /** It is one matter, told right. */
  case Right

  /** It holds more than one matter. */
  case Split

  /** Another topic holds the same matter. */
  case Merge

  /** It is no matter worth keeping. */
  case Noise
}
