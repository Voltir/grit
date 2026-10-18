package grit.knowledge.ref

import grit.core.id.{ConversationId, DocKey}

/** A topic's key, `topic:` then what opened it: `strand-{root}` or `card-{card key}`; it is
  * its document's key ([[TopicKey.doc]]).
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

  /** The key of the topic the card under `card` opened. */
  def card(card: CardKey): TopicKey =
    // 11 characters and 64 hex digits: DocKey.of never refuses them.
    DocKey
      .of(s"$Card${CardKey.value(card)}")
      .fold(why => throw new IllegalStateException(why), identity)

  /** The topic key `key` is, or `None` when it is no topic's. */
  def read(key: DocKey): Option[TopicKey] = {
    val text = DocKey.value(key)
    if (text.startsWith(Strand)) Option.when(text.length > Strand.length)(key)
    else if (text.startsWith(Card)) CardKey.parse(text.drop(Card.length)).toOption.map(_ => key)
    else None
  }

  def doc(key: TopicKey): DocKey = key

  def value(key: TopicKey): String = DocKey.value(key)

  private val Strand = "topic:strand-"
  private val Card = "topic:card-"
}
