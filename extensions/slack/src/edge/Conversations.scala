package grit.slack.edge

/** Which conversation the Slack edge records a channel's message in, a deployment's choice:
  * the same for what it hears live and what a backfill hears.
  */
enum Conversations(val word: String) {

  /** Each thread its own conversation, its root its first message
    * ([[grit.core.store.Origin.Slack]]), as before rooms.
    */
  case PerThread extends Conversations("threads")

  /** A channel's top level one conversation ([[grit.core.store.Origin.Room]]), and each thread
    * under it one of its own ([[grit.core.store.Origin.Thread]]) (ADR 0060); a thread recorded
    * before rooms keeps its conversation, and a message heard in one is not heard again.
    */
  case PerRoom extends Conversations("rooms")
}

object Conversations {

  /** [[PerThread]]: a deployment records rooms only once it says so. */
  val Default: Conversations = PerThread

  /** The one whose [[Conversations.word]] is `word`. */
  def named(word: String): Option[Conversations] = values.find(_.word == word)
}
