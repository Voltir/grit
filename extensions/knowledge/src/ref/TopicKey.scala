package grit.knowledge.ref

import grit.core.id.{ConversationId, DocKey, EntryId}
import grit.core.stitch.Strand

/** A topic's key, `topic:strand-` then the root of the strand that opened it, or `topic:run-` then a run's first message; it is its
  * document's key ([[TopicKey.doc]]).
  */
opaque type TopicKey = DocKey

object TopicKey {

  /** The key of the topic the strand of `root` opened, or why not: `root` too long for a
    * document's key (over 187 characters) or holding a control character.
    */
  def strand(root: ConversationId): Either[String, TopicKey] =
    DocKey
      .of(s"$StrandPrefix${ConversationId.value(root)}")
      .left
      .map(why => s"a topic key of strand ${ConversationId.value(root)}: $why")

  /** The key of the topic `seen` opens: `topic:strand-` then its root for a whole strand,
    * `topic:run-` then its first message for a run; or why not, as [[strand]] says.
    */
  def of(seen: Strand.Digested): Either[String, TopicKey] = seen.id.first match {
    case None => strand(seen.root)
    case Some(first) =>
      DocKey
        .of(s"$RunPrefix${EntryId.value(first)}")
        .left
        .map(why => s"a topic key of run ${EntryId.value(first)}: $why")
  }

  /** The topic key `key` is, or `None` when it is no topic's. */
  def read(key: DocKey): Option[TopicKey] = {
    val text = DocKey.value(key)
    Option.when(
      Vector(StrandPrefix, RunPrefix).exists(p => text.startsWith(p) && text.length > p.length)
    )(key)
  }

  def doc(key: TopicKey): DocKey = key

  def value(key: TopicKey): String = DocKey.value(key)

  private val StrandPrefix = "topic:strand-"

  private val RunPrefix = "topic:run-"
}
