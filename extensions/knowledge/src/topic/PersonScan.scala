package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneId}

import scala.util.Try

import grit.core.document.{Document, DocumentKeeper, Written}
import grit.core.durable.Journaled
import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.place.Place
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Integrity
import grit.knowledge.ref.TopicKey
import grit.std.either.Traversal.traverse

/** A topics run's people step: whose cards are due at its floor, by ids alone, and each card's
  * two keeps.
  */
private[topic] object PersonScan {

  /** A person due a card: its `key`, their `name` and `zone` as their newest strand gives them,
    * the `facts` code found, the strands of theirs a card's ask is shown (`asked`, the newest,
    * at most [[PersonCards.Asked]]) and its lineage (`seen`, newest first, at most
    * [[PersonCards.Lineages]]).
    */
  final case class Due(
      key: PersonKey,
      name: Strand.Name,
      zone: Option[ZoneId],
      facts: PersonCard.Facts,
      asked: Vector[Strand.Digested],
      seen: Vector[Lineage.Tag.Strand]
  ) extends caps.Pure

  /** What a people step came to: those `due`, at most the run's `people`, most fresh strands
    * first; ids, counts, days and names alone, about 5 KB each.
    */
  final case class People(due: Vector[Due]) extends caps.Pure

  /** The version of a card a keep left current. */
  final case class Carded(version: DocumentVersionId) extends caps.Pure

  /** The people step at `until`, for `day` in `zone`: the floor's cards read, each that does not
    * read, whose person last spoke more than [[PersonCards.KeepFor]] before `day`
    * ([[PersonCard.lapsed]]) or whose rooms are not all at the floor now withdrawn, and each
    * quoting a topic no longer current written again without it, believed as it was; then the
    * strands at the floor spoken in within [[PersonCards.Window]] before `until` (cut by `cut`),
    * at most [[PersonCards.Scanned]], newest touched first, and their speakers: of those with a
    * name, each [[PersonCard.due]] on `day`, most fresh strands first (ties by key), at most
    * `most`.
    */
  def people(
      most: Int,
      until: Instant,
      day: LocalDate,
      zone: ZoneId,
      cut: Strand.Cut,
      keeper: DocumentKeeper,
      strands: StrandShelf
  )(using tx: Tx^): Either[StoreError, People] = {
    val floor = Tx.floor(tx)
    for {
      documents <- keeper.newest(Int.MaxValue).map(_.filter(_.label == floor))
      topics = documents.flatMap(d =>
        TopicKey
          .read(d.key)
          .flatMap(k => TopicWrite.read(k, d.data).toOption.map(h => (k, d.version, h.topic)))
      )
      current = topics.map(_._2).toSet
      cards <- TopicScan.each(documents.flatMap(d => PersonKey.read(d.key).map(_ -> d))) {
        (key, d) => kept(key, d, current, day, keeper, until)
      }
      window <- strands.units(until.minusSeconds(PersonCards.Window.toSeconds), until, cut)
      newest = window.sortBy(s => (s.touched, s.root.toString)).reverse.take(PersonCards.Scanned)
      spoken <- strands.speakers(newest)
    } yield {
      val said = newest.zip(spoken)
      val held = cards.flatten.toMap
      val due = said
        .flatMap(_._2.filter(_.name.nonEmpty).map(_.person))
        .distinct
        .flatMap(person =>
          PersonKey.of(person).toOption.flatMap { key =>
            val theirs = said.filter(_._2.exists(_.person == person))
            val card = held.get(key)
            val fresh = card.fold(theirs.size)(_.fresh(theirs.map(_._1)))
            val named = theirs.flatMap(_._2.find(_.person == person)).find(_.name.nonEmpty)
            for {
              _ <- Option.when(PersonCard.due(card, fresh, day))(())
              me <- named
              name <- me.name
              facts <- PersonCard.Facts.of(person, said, topics, zone)
            } yield (
              fresh,
              Due(
                key,
                name,
                me.zone,
                facts,
                theirs.map(_._1).take(PersonCards.Asked),
                theirs
                  .map(_._1)
                  .take(PersonCards.Lineages)
                  .map(s => Lineage.Tag.Strand(s.root, s.room, s.digest, s.id.first))
              )
            )
          }
        )
        .sortBy((fresh, d) => (-fresh, DocKey.value(PersonKey.doc(d.key))))
        .map(_._2)
        .take(most.max(0))
      People(due)
    }
  }

  /** The card `key` holds in `d`, as the step leaves it: `None` once withdrawn. */
  private def kept(
      key: PersonKey,
      d: Document,
      current: Set[DocumentVersionId],
      day: LocalDate,
      keeper: DocumentKeeper,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Option[(PersonKey, PersonCard)]] = {
    val floor = Tx.floor(tx)
    PersonCard.read(key, d.data) match {
      case Right(c) if !c.lapsed(day) && c.from.forall(r => Tx.roomLabel(r) == floor) =>
        if (c.quotes.forall(q => current.contains(q.version))) Right(Some(key -> c))
        else {
          val left = c.without(current)
          written(key, left, DocumentKeeper.replacing(Some(d)), keeper, at).map(_ =>
            Some(key -> left)
          )
        }
      case _ => keeper.withdraw(PersonKey.doc(key), floor, at).map(_ => None)
    }
  }

  /** `d`'s card kept with the facts found on `day`, and its quotes read now: each topic quoted
    * only at the version `d.facts` names, believed at no more than every topic quoted, nor than
    * the card it replaces when that card's paragraph is carried over.
    */
  def facts(d: Due, day: LocalDate, keeper: DocumentKeeper, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Carded] = {
    val floor = Tx.floor(tx)
    for {
      before <- keeper.current(PersonKey.doc(d.key), floor)
      prior = before.flatMap(b => PersonCard.read(d.key, b.data).toOption)
      quoted <- TopicScan.each(d.facts.topics) { (k, v) =>
        keeper
          .current(TopicKey.doc(k), floor)
          .map(
            _.filter(_.version == v).flatMap(doc =>
              TopicWrite
                .read(k, doc.data)
                .toOption
                .map(h => (PersonCard.Quote.of(k, v, h.topic), doc.integrity))
            )
          )
      }
      card = PersonCard.written(
        prior,
        d.key,
        d.name,
        d.zone,
        d.facts,
        quoted.flatten.map(_._1),
        d.seen,
        day
      )
      carried =
        if (prior.exists(_.paragraph.nonEmpty)) DocumentKeeper.replacing(before)
        else DocumentKeeper.replacing(None)
      kept <- written(
        d.key,
        card,
        quoted.flatten.map(_._2).foldLeft(carried)(_.meet(_)),
        keeper,
        at
      )
    } yield kept
  }

  /** The card current under `key`, at `version`, kept with the paragraph `said` ([[PersonCard.told]]),
    * believed at no more than it; `Left` when another version is current.
    */
  def told(
      key: PersonKey,
      version: DocumentVersionId,
      said: Option[String],
      keeper: DocumentKeeper,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Carded] =
    keeper.current(PersonKey.doc(key), Tx.floor(tx)).flatMap {
      case Some(d) if d.version == version =>
        PersonCard
          .read(key, d.data)
          .left
          .map(why => StoreError.Invalid(s"a person's card: $why"))
          .flatMap(c => written(key, c.told(said), DocumentKeeper.replacing(Some(d)), keeper, at))
      case _ => Left(StoreError.Invalid("the person's card was written since its facts"))
    }

  private def written(
      key: PersonKey,
      card: PersonCard,
      integrity: Integrity,
      keeper: DocumentKeeper,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Carded] =
    for {
      text <- card.text.left.map(why => StoreError.Invalid(s"a person's card's text: $why"))
      w <- keeper.write(
        PersonKey.doc(key),
        Tx.floor(tx),
        integrity,
        Place.Everywhere,
        text,
        card.write,
        None,
        at
      )
    } yield w match {
      case Written.Versioned(v, _, _) => Carded(v)
      case Written.Unchanged(v, _) => Carded(v)
    }

  /** Its record: `{"due": [{"key", "name", "zone"?, "facts", "asked": [...], "seen": [...]}]}`,
    * the facts `{"topics": [{"key", "version"}], "peers", "rooms", "days", "lastSaid"}`.
    */
  given Journaled[People] = Journaled.json(
    p =>
      ujson.Obj("due" -> ujson.Arr.from(p.due.map { d =>
        val o = ujson.Obj(
          "key" -> DocKey.value(PersonKey.doc(d.key)),
          "name" -> Strand.Name.value(d.name),
          "facts" -> ujson.Obj(
            "topics" -> ujson.Arr.from(
              d.facts.topics.map((k, v) =>
                ujson
                  .Obj("key" -> TopicKey.value(k), "version" -> DocumentVersionId.value(v).toDouble)
              )
            ),
            "peers" -> ujson.Arr.from(d.facts.peers.map(n => ujson.Str(Strand.Name.value(n)))),
            "rooms" -> d.facts.rooms,
            "days" -> d.facts.days,
            "lastSaid" -> d.facts.lastSaid.toString
          ),
          "asked" -> ujson.Arr.from(d.asked.map(Strand.Digested.write)),
          "seen" -> TopicJson.lineage(Lineage.of(d.seen))
        )
        d.zone.foreach(z => o("zone") = z.getId)
        o
      })),
    v =>
      for {
        o <- v.objOpt.toRight("a people step's record is an object")
        due <- o
          .get("due")
          .flatMap(_.arrOpt)
          .toRight("a people step holds no due")
          .flatMap(ds => traverse(ds.toVector)(readDue))
      } yield People(due)
  )

  given Journaled[Carded] = Journaled.json(
    c => ujson.Obj("version" -> DocumentVersionId.value(c.version).toDouble),
    v =>
      v.objOpt
        .flatMap(_.get("version"))
        .toRight("a card's keep holds no version")
        .flatMap(TopicJson.version)
        .map(Carded(_))
  )

  private def readDue(v: ujson.Value): Either[String, Due] = {
    def name(t: String) = Strand.Name.of(t).toRight(s"a due person names no one: $t")
    for {
      o <- v.objOpt.toRight("a due person is no object")
      key <- o
        .get("key")
        .flatMap(_.strOpt)
        .flatMap(k => DocKey.of(k).toOption)
        .flatMap(PersonKey.read)
        .toRight("a due person holds no card's key")
      n <- o.get("name").flatMap(_.strOpt).toRight("a due person holds no name").flatMap(name)
      zone <- o.get("zone") match {
        case None => Right(None)
        case z =>
          z.flatMap(_.strOpt)
            .flatMap(t => Try(ZoneId.of(t)).toOption)
            .map(Some(_))
            .toRight("no zone")
      }
      f <- o.get("facts").flatMap(_.objOpt).toRight("a due person holds no facts")
      topics <- f
        .get("topics")
        .flatMap(_.arrOpt)
        .toRight("facts hold no topics")
        .flatMap(ts =>
          traverse(ts.toVector)(t =>
            for {
              to <- t.objOpt.toRight("a fact's topic is no object")
              k <- TopicJson.topicKey(to.get("key"), "a fact's topic")
              ver <- to
                .get("version")
                .toRight("a fact's topic holds no version")
                .flatMap(TopicJson.version)
            } yield (k, ver)
          )
        )
      peers <- f
        .get("peers")
        .flatMap(_.arrOpt)
        .toRight("facts hold no peers")
        .flatMap(ps =>
          traverse(ps.toVector)(p => p.strOpt.toRight("a peer: no string").flatMap(name))
        )
      rooms <- f.get("rooms").flatMap(_.numOpt).map(_.toInt).toRight("facts hold no rooms")
      days <- f.get("days").flatMap(_.numOpt).map(_.toInt).toRight("facts hold no days")
      lastSaid <- f
        .get("lastSaid")
        .flatMap(_.strOpt)
        .flatMap(Passage.day)
        .toRight("facts hold no lastSaid")
      asked <- o
        .get("asked")
        .flatMap(_.arrOpt)
        .toRight("a due person holds no asked")
        .flatMap(as => traverse(as.toVector)(Strand.Digested.read))
      seen <- TopicJson
        .readLineage(o.get("seen"))
        .map(_.tags.collect { case s: Lineage.Tag.Strand => s })
    } yield Due(key, n, zone, PersonCard.Facts(topics, peers, rooms, days, lastSaid), asked, seen)
  }

}
