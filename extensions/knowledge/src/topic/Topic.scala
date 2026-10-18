package grit.knowledge.topic

import java.time.LocalDate

import grit.core.document.DocText
import grit.knowledge.ref.TopicKey
import grit.prose.form.Doc

/** A topic: its `lead`, what it is about now, and its `passages`, oldest first, each one day's
  * addition, at most [[Topic.Passages]]. Notes hold its links and claims.
  */
final case class Topic private (lead: Doc[TopicNote], passages: Vector[Passage]) extends caps.Pure

object Topic {

  /** 30: the most passages a topic keeps; past it the oldest go. */
  val Passages: Int = 30

  /** `lead` and `passages` by day, oldest first (one day's in the order given), the oldest past
    * [[Passages]] dropped.
    */
  def of(lead: Doc[TopicNote], passages: Vector[Passage]): Topic =
    new Topic(lead, passages.sortBy(_.day.toEpochDay).takeRight(Passages))

  /** Its data, `{"v":1, "lead", "passages"}`. */
  def write(t: Topic): ujson.Value = TopicJson.write(t)

  /** The topic of `key` that `v` holds, or why not: an unknown version, no lead, data that is
    * no topic's, two different claims of one id, or a claim relation naming a claim of this
    * topic that it does not hold. Passages past [[Passages]] are dropped, as [[of]] drops them.
    */
  def read(key: TopicKey, v: ujson.Value): Either[String, Topic] = TopicJson.read(key, v)

  /** Its document's text: `lead` first, so a reader that cuts the head keeps the lead whole
    * while it fits, then each passage newest first under its day as a heading, notes lowered
    * (a link as a link to its topic's key, a claim as its span's text); or why not: blank.
    */
  def text(t: Topic): Either[String, DocText] = TopicJson.text(t)
}

/** What one day's pass added to a topic: on `day`, written `from` what it lists. */
final case class Passage(day: LocalDate, from: Lineage, body: Doc[TopicNote]) extends caps.Pure
