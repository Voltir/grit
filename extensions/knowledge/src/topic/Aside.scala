package grit.knowledge.topic

import java.time.Instant

import grit.core.document.{Document, DocumentKeeper}
import grit.core.id.DocKey
import grit.core.store.{StoreError, Tx}
import grit.knowledge.card.FloorList
import grit.knowledge.topic.TopicWrite.Kept

/** What a label's topics runs set aside: each strand and card their attach ask placed in no
  * topic, at the digest or version it was shown at, and when, so no later run asks of it again
  * until it changes or its run's [[TopicParams.asideFor]] has passed. Kept under `topic:aside`,
  * which is no topic's key ([[TopicKey.read]]), as [[TopicJson.aside]] writes it; an entry kept
  * before entries were dated is read as dated when the aside was last written, the latest it can
  * have been set aside, so it never leaves the aside early.
  */
private[topic] object Aside extends FloorList[Aside.Item](DocKey.literal("topic:aside")) {

  /** One strand or card set aside (`tag`), and when (`at`). */
  final case class Item(tag: Lineage.Tag, at: Instant) extends caps.Pure

  protected def encode(all: Vector[Item]): ujson.Value = TopicJson.aside(all)

  protected def decode(d: Document): Vector[Item] =
    TopicJson.readAside(d.data, d.written).toOption.getOrElse(Vector.empty)

  /** `tags`, set aside `at`, joined to the aside at its transaction's floor, each replacing an
    * entry of its tag, as [[joined]].
    */
  def setAside(keeper: DocumentKeeper, tags: Vector[Lineage.Tag], at: Instant)(using
      Tx^
  ): Either[StoreError, Kept] =
    joined(keeper, held => held.filterNot(e => tags.contains(e.tag)) ++ tags.map(Item(_, at)), at)
      .map(Kept(_, 0))
}
