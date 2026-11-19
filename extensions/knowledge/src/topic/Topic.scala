package grit.knowledge.topic

import java.time.LocalDate

import grit.core.document.DocText
import grit.core.id.DocumentVersionId
import grit.knowledge.ref.TopicKey
import grit.prose.form.Doc

/** A topic: its `lead`, what it is about now, and its `passages`, oldest first, each one day's
  * addition, at most [[Topic.Passages]]. Notes hold its links and claims.
  */
final case class Topic private (
    lead: Doc[TopicNote],
    passages: Vector[Passage],
    kind: Topic.Kind
) extends caps.Pure

object Topic {

  /** 30: the most passages a topic keeps; past it the oldest go. */
  val Passages: Int = 30

  /** `lead` and `passages` by day, oldest first (one day's in the order given), the oldest past
    * [[Passages]] dropped.
    */
  def of(lead: Doc[TopicNote], passages: Vector[Passage], kind: Kind = Kind.Subject): Topic =
    new Topic(lead, passages.sortBy(_.day.toEpochDay).takeRight(Passages), kind)

  /** What a topic tracks, fixed when it opens: a subject the team comes back to, or one
    * meeting, its head refined as later talk is written into it.
    */
  enum Kind extends caps.Pure {
    case Subject
    case Meeting(head: grit.knowledge.topic.Meeting)
  }

  /** Its data, `{"v":3, "kind", "lead", "passages"}`, its kind `"subject"` or `{"meeting": …}`,
    * each doc as [[grit.prose.json.ProseJson]] stores it.
    */
  def write(t: Topic): ujson.Value = TopicJson.write(t)

  /** The topic of `key` that `v` holds, a v2 topic read as a subject, or why not: data of a
    * later version than this reader's ([[Unread.Later]]), or [[Unread.Refused]]: version 1, no
    * version, no lead, a kind that is none, data
    * that is no topic's, two different claims of one id, or a claim relation naming a claim of
    * this topic that it does not hold. Passages past [[Passages]] are dropped, as [[of]] drops
    * them.
    */
  def read(key: TopicKey, v: ujson.Value): Either[Unread, Topic] = TopicJson.read(key, v)

  /** `passages` with `p` joined in for its day: one passage of that day, whose lineage is the
    * day's passages' then `p`'s, and whose body is `body` of each of theirs, oldest first, and
    * `p`'s, folded from the right.
    */
  private[topic] def joined(passages: Vector[Passage], p: Passage)(
      body: (Doc[TopicNote], Doc[TopicNote]) => Doc[TopicNote]
  ): Vector[Passage] = {
    val (same, rest) = passages.partition(_.day == p.day)
    rest :+ Passage(
      p.day,
      Lineage.of(same.flatMap(_.from.tags) ++ p.from.tags),
      same.map(_.body).foldRight(p.body)(body)
    )
  }

  /** `from`, the topic of `fromKey` at version `was`, folded into `into`, the topic of `key`:
    * `into`'s lead, and every passage of both under its own day, each of `from`'s naming `was`
    * in its lineage, after `into`'s body on a day both hold; `from`'s claims numbered past
    * every claim `into` holds, each relation to one of them, in either topic, following it; a
    * relation to a claim of `key` the result does not hold (one only `from`'s lead held, or a
    * dropped passage's) dropped; and the oldest passages past [[Passages]] dropped, so the
    * result reads back under `key` ([[read]]).
    */
  private[topic] def folded(
      key: TopicKey,
      into: Topic,
      fromKey: TopicKey,
      from: Topic,
      was: DocumentVersionId
  ): Topic = {
    val by = claims(into).map(c => ClaimId.number(c.id)).maxOption.getOrElse(BigInt(0))
    def moved(rs: Vector[Relation]): Vector[Relation] = rs.map(r =>
      if (r.to.topic == fromKey) r.copy(to = ClaimRef(key, ClaimId.past(r.to.claim, by))) else r
    )
    def followed(own: Boolean)(n: TopicNote): TopicNote = n match {
      case TopicNote.Claimed(c) =>
        TopicNote.Claimed(
          c.copy(id = if (own) c.id else ClaimId.past(c.id, by), relations = moved(c.relations))
        )
      case link => link
    }
    val passages =
      from.passages.foldLeft(into.passages.map(p => p.copy(body = p.body.map(followed(true))))) {
        (all, p) =>
          joined(
            all,
            Passage(
              p.day,
              Lineage.of(p.from.tags :+ Lineage.Tag.Topic(was)),
              p.body.map(followed(false))
            )
          )((a, b) => Doc(a.blocks ++ b.blocks))
      }
    of(key, into.lead.map(followed(true)), passages, into.kind)
  }

  /** The topic of `key` from `lead` and `passages`, as [[of]] makes it, each relation to a
    * claim of `key` it does not hold dropped (one of a passage past [[Passages]], say), so it
    * reads back under `key` ([[read]]) unless two different claims share an id.
    */
  private[topic] def of(
      key: TopicKey,
      lead: Doc[TopicNote],
      passages: Vector[Passage],
      kind: Kind
  ): Topic = {
    val t = of(lead, passages, kind)
    val held = claims(t).map(_.id).toSet
    def kept(n: TopicNote): TopicNote = n match {
      case TopicNote.Claimed(c) =>
        TopicNote.Claimed(
          c.copy(relations = c.relations.filterNot(r => r.to.topic == key && !held(r.to.claim)))
        )
      case link => link
    }
    new Topic(t.lead.map(kept), t.passages.map(p => p.copy(body = p.body.map(kept))), kind)
  }

  /** Every claim `t` holds, each once. */
  private def claims(t: Topic): Vector[Claim] =
    (t.lead.notes ++ t.passages.flatMap(_.body.notes)).collect { case TopicNote.Claimed(c) =>
      c
    }.distinct

  /** Why data does not read as a topic: it is a later reader's, of `version`, so this one leaves
    * it be; or it is refused, for `reason`, so a run withdraws it.
    */
  enum Unread extends caps.Pure {
    case Later(version: Long)
    case Refused(reason: String)

    /** Why, in words. */
    def why: String = this match {
      case Later(v) => s"a topic of version $v, later than this reader's"
      case Refused(reason) => reason
    }
  }

  /** Its document's text: a meeting's head line first, then `lead`, so a reader that cuts the
    * head keeps them whole
    * while it fits, then each passage newest first under its day as a heading, notes lowered
    * (a link as a link to its topic's key, a claim as its span's text); or why not: blank.
    */
  def text(t: Topic): Either[String, DocText] = TopicJson.text(t)
}

/** What one day's pass added to a topic: on `day`, written `from` what it lists. */
final case class Passage(day: LocalDate, from: Lineage, body: Doc[TopicNote]) extends caps.Pure
