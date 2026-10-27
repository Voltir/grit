package grit.knowledge.topic

import grit.core.schema.{Conforming, JsonSchema}

/** The topic dream's attach ask, for a live probe outside this package: its system text over
  * `strands` discussions and topics shown when `topics` (no cards), and its reply read back.
  */
object AttachAsk {

  def system(strands: Int, topics: Boolean): String =
    TopicDream.attachSystem(strands, topics, cards = false)

  def schema: JsonSchema = TopicDream.attachReply(discussions = true).schema

  /** `reply` read as the attach reply: its groups and aside, as written. */
  def read(reply: Conforming): Either[String, String] =
    TopicDream.attachReply(discussions = true).read(reply).map(_.toString)
}
