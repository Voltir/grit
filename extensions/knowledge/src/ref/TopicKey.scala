package grit.knowledge.ref

import grit.core.id.{ConversationId, DocKey}

/** A topic's key, `topic:strand-` then the root of the strand that opened it; it is its
  * document's key ([[TopicKey.doc]]).
  */
opaque type TopicKey = DocKey

object TopicKey {

  /** The key of the topic the strand of `root` opened, or why not: `root` too long for a
    * document's key (over 187 characters) or holding a control character.
    */
  def strand(root: ConversationId): Either[String, TopicKey] =
    DocKey
      .of(s"$Strand${ConversationId.value(root)}")
      .left
      .map(why => s"a topic key of strand ${ConversationId.value(root)}: $why")

  /** The topic key `key` is, or `None` when it is no topic's. */
  def read(key: DocKey): Option[TopicKey] = {
    val text = DocKey.value(key)
    Option.when(text.startsWith(Strand) && text.length > Strand.length)(key)
  }

  def doc(key: TopicKey): DocKey = key

  def value(key: TopicKey): String = DocKey.value(key)

  private val Strand = "topic:strand-"
}
