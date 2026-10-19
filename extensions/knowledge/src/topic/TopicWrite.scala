package grit.knowledge.topic

import java.time.{Instant, LocalDate}

import grit.core.document.{DocRef, DocumentKeeper, Written}
import grit.core.durable.Journaled
import grit.core.id.{DocKey, DocumentVersion}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.ref.{Ref, TopicKey}
import grit.prose.form.{Doc, Mark, Span}
import grit.prose.markdown.Markdown

/** A topic's document as a topics run writes and reads it: [[Topic.write]]'s data with `units`,
  * the cards' units it was written from, and `review`, what the day that last wrote it added.
  */
private[topic] object TopicWrite {

  /** What the day that last wrote a topic added, as its review shows it: whether it `opened`
    * the topic, and the titles of the `cards` attached.
    */
  final case class Today(day: LocalDate, opened: Boolean, cards: Vector[String]) extends caps.Pure

  /** A topic's document read back: its topic, its cards' units, and its last day's [[Today]]
    * when it reads.
    */
  final case class Held(topic: Topic, units: Vector[Ref], today: Option[Today]) extends caps.Pure

  /** What a write did: the version written, `None` when the store held it already; and how
    * many refs it left out ([[kept]]).
    */
  final case class Kept(version: Option[DocumentVersion], skipped: Int) extends caps.Pure

  object Kept {

    /** Its record: `{"version": n | null, "skipped"}`. */
    given Journaled[Kept] = Journaled.json(
      k =>
        ujson.Obj(
          "version" -> k.version.fold[ujson.Value](ujson.Null)(v =>
            DocumentVersion.value(v).toDouble
          ),
          "skipped" -> k.skipped
        ),
      v =>
        for {
          o <- v.objOpt.toRight("a topic's keep is an object")
          version <- o.get("version") match {
            case Some(ujson.Null) => Right(None)
            case Some(n) =>
              n.numOpt
                .filter(_.isWhole)
                .flatMap(x => DocumentVersion.of(x.toLong))
                .map(Some(_))
                .toRight("no version")
            case None => Left("a topic's keep holds no version")
          }
          skipped <- o.get("skipped").flatMap(_.numOpt).map(_.toInt).toRight("no skipped")
        } yield Kept(version, skipped)
    )
  }

  /** `markdown` as a topic's prose: a span linking to a topic's key becomes a note linking to
    * that topic, as [[Topic.text]] lowers one.
    */
  def prose(markdown: String): Doc[TopicNote] =
    Markdown.parse(markdown).mapSpans { s =>
      s.marks
        .collectFirst { case Mark.Link(href) => DocKey.of(href).toOption.flatMap(TopicKey.read) }
        .flatten
        .fold[Span[TopicNote]](s)(to =>
          Span(
            s.text,
            s.marks.filterNot(_ == Mark.Link(TopicKey.value(to))),
            Vector(TopicNote.Link(to))
          )
        )
    }

  /** What `t` refers to: each strand its passages and claims were written from, `units` newest
    * first, each topic it links, and the unit each other link in it names ([[Ref.ofLink]]), each
    * once.
    */
  def refs(t: Topic, units: Vector[Ref]): Vector[Ref] = {
    val docs = t.lead +: t.passages.map(_.body)
    val notes = docs.flatMap(_.notes)
    val lineages = t.passages.map(_.from) ++ notes.collect { case TopicNote.Claimed(c) => c.from }
    val strands = lineages.flatMap(_.tags).collect { case Lineage.Tag.Strand(root, _, _) =>
      Ref.Strand(root)
    }
    val topics = notes.collect { case TopicNote.Link(to) => Ref.Topic(to) }
    val mentions = docs
      .flatMap(
        _.mapSpans(s =>
          Span(s.text, Set.empty, s.marks.toVector.collect { case Mark.Link(h) => Linked(h) })
        ).notes
      )
      .flatMap(l => Ref.ofLink(l.href))
    // Newest units first, so a topic past DocRef.Most refs drops its oldest.
    (strands ++ units.reverse ++ topics ++ mentions).distinct
  }

  /** The topic `key`'s document `data` holds, or why not ([[Topic.read]]). */
  def read(key: TopicKey, data: ujson.Value): Either[String, Held] =
    Topic.read(key, data).map { t =>
      val o = data.objOpt
      val units = o
        .flatMap(_.get("units"))
        .flatMap(_.arrOpt)
        .toVector
        .flatten
        .flatMap(u =>
          for {
            p <- u.objOpt
            kind <- p.get("kind").flatMap(_.strOpt)
            ref <- p.get("ref").flatMap(_.strOpt)
            r <- DocRef.of(kind, ref).toOption.flatMap(Ref.read)
          } yield r
        )
      val today = for {
        r <- o.flatMap(_.get("review")).flatMap(_.objOpt)
        day <- r
          .get("day")
          .flatMap(_.strOpt)
          .flatMap(d => scala.util.Try(LocalDate.parse(d)).toOption)
        opened <- r.get("opened").flatMap(_.boolOpt)
        cards <- r.get("cards").flatMap(_.arrOpt)
      } yield Today(day, opened, cards.toVector.flatMap(_.strOpt))
      Held(t, units, today)
    }

  /** `held` as the current document under `key` at `label` ([[DocumentKeeper.write]]), not in
    * any one room ([[Place.Everywhere]]), written `at`, holding a ref to each of [[refs]] that
    * makes one: one [[Ref.written]] refuses, or one past [[DocRef.Most]], is left out and
    * counted, its link still in the text. `Left` when the store refuses, or the topic's text is
    * blank.
    */
  def kept(keeper: DocumentKeeper, key: TopicKey, label: Label, held: Held, at: Instant)(using
      Tx^
  ): Either[StoreError, Kept] = {
    val written = refs(held.topic, held.units).map(Ref.written)
    val made = written.collect { case Right(r) => r }.distinct
    val skipped = written.count(_.isLeft) + (made.size - DocRef.Most).max(0)
    for {
      text <- Topic.text(held.topic).left.map(why => StoreError.Invalid(s"a topic's text: $why"))
      w <- keeper.write(
        TopicKey.doc(key),
        label,
        Integrity.Authority,
        Place.Everywhere,
        text,
        data(held),
        None,
        at,
        made.take(DocRef.Most).toSet
      )
    } yield w match {
      case Written.Versioned(v, _, _) => Kept(Some(v), skipped)
      case Written.Unchanged(_, _) => Kept(None, skipped)
    }
  }

  private def data(held: Held): ujson.Value = {
    val o = Topic.write(held.topic)
    val units = held.units.flatMap(u => Ref.written(u).toOption)
    o("units") = ujson.Arr.from(units.map(r => ujson.Obj("kind" -> r.kind, "ref" -> r.ref)))
    held.today.foreach(t =>
      o("review") = ujson.Obj(
        "day" -> t.day.toString,
        "opened" -> t.opened,
        "cards" -> ujson.Arr.from(t.cards.map(ujson.Str(_)))
      )
    )
    o
  }

  /** A link's target, as a note while its links are gathered. */
  private final case class Linked(href: String) extends caps.Pure
}
