package grit.core.store

import grit.core.id.{ConversationId, TurnSeq}
import grit.core.message.{AssistantBlock, Message}

/** A crude [[EntrySearch]] over an [[InMemoryEntryStore]], for tests of what calls a
  * search: an entry's score is how many of the query's distinct words its text contains,
  * ignoring case. Not BM25; the eval ranks with the real one.
  */
final class InMemoryEntrySearch(entries: EntryStore) extends EntrySearch {

  def search(conversation: ConversationId, before: TurnSeq, query: String, limit: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]] =
    entries.list(conversation).map { all =>
      val wanted = words(query)
      all
        .filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(before))
        .flatMap(e => text(e.payload).map(t => e -> wanted.count(words(t).contains)))
        .filter(_._2 > 0)
        .sortBy { case (e, n) => (-n, -e.seq) }
        .take(limit.max(0))
        .map { case (e, n) => EntrySearch.Hit(e.id, e.turnSeq, n.toDouble) }
    }

  private def words(text: String): Set[String] =
    text.toLowerCase.split("[^a-z0-9_-]+").filter(_.nonEmpty).toSet

  private def text(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(t)) => Some(t)
    case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
      Some(blocks.collect { case AssistantBlock.Text(t) => t }.mkString(" "))
    case Payload.Message(Message.ToolResult(_, content, _)) => Some(content)
    case Payload.Summary(t) => Some(t)
    case Payload.Query(_) | Payload.Window(_, _) | Payload.Topic(_) => None
  }
}
