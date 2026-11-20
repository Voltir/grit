package grit.knowledge.topic

import java.time.{LocalDate, ZoneId}

import scala.concurrent.duration.*
import scala.util.Try

import grit.core.document.DocText
import grit.core.id.{DocKey, DocumentVersionId, PrincipalId}
import grit.core.message.Cut
import grit.core.place.Place
import grit.core.stitch.Strand
import grit.knowledge.ref.TopicKey
import grit.prose.index.IndexText
import grit.std.either.Traversal.traverse

/** A person's card at one floor, as a topics run writes it from the strands they spoke in
  * there: who (their `key`, `name` and `zone`), what code found of them (`quotes`, the topics
  * they spoke in; `peers`, whom they share most strands with; how many `rooms` and `days` they
  * spoke in within [[PersonCards.Window]], a lower bound; the day they last spoke), the
  * `paragraph` inferred of their work when one was, and what it was written from (`lineage`,
  * the window's strands, at most [[PersonCards.Lineages]]; `from`, every room, at most
  * [[PersonCards.Rooms]]), last on `written`.
  */
final case class PersonCard private (
    key: PersonKey,
    name: Strand.Name,
    zone: Option[ZoneId],
    quotes: Vector[PersonCard.Quote],
    peers: Vector[Strand.Name],
    rooms: Int,
    days: Int,
    lastSaid: LocalDate,
    paragraph: Option[String],
    lineage: Lineage,
    from: Vector[Place],
    written: LocalDate
) extends caps.Pure {

  /** Its text, as a window shows it: its name's line, its paragraph, then the facts' lines, each
    * left out when it has nothing to say, within [[PersonCards.TextChars]].
    */
  def text: Either[String, DocText] =
    DocText.of(Cut.head(PersonCard.render(this), PersonCards.TextChars))

  /** It with `said` as its paragraph, cut at a word within [[PersonCards.ParagraphChars]]; `None`
    * (the model saw nothing of their work) keeps the paragraph it had.
    */
  def told(said: Option[String]): PersonCard =
    said.map(_.trim).filter(_.nonEmpty) match {
      case None => this
      case Some(p) => copy(paragraph = Some(Cut.head(p, PersonCards.ParagraphChars)))
    }

  /** It without the quotes of topics whose versions are not among `current`: what a topic
    * withdrawn, relabelled or written since leaves.
    */
  def without(current: Set[DocumentVersionId]): PersonCard =
    copy(quotes = quotes.filter(q => current.contains(q.version)))

  /** How many of `theirs`, strands its person spoke in, its lineage does not hold. */
  def fresh(theirs: Vector[Strand.Digested]): Int =
    theirs.count(s => !PersonCard.holds(lineage, s.id))

  /** Whether its person last spoke more than [[PersonCards.KeepFor]] before `day`. */
  def lapsed(day: LocalDate): Boolean =
    lastSaid.isBefore(day.minusDays(PersonCards.KeepFor.toDays))

  /** Its data, read back by [[PersonCard.read]]. */
  def write: ujson.Value = PersonCard.data(this)
}

object PersonCard {

  /** A topic a card quotes: its `key` and `version`, the first sentence of its lead within
    * [[PersonCards.QuoteChars]] (`said`), and for a meeting with one, its `day`.
    */
  final case class Quote(
      key: TopicKey,
      version: DocumentVersionId,
      said: String,
      day: Option[LocalDate]
  ) extends caps.Pure

  object Quote {

    /** `topic`, kept under `key` at `version`, as a card quotes it. */
    def of(key: TopicKey, version: DocumentVersionId, topic: Topic): Quote = {
      val lead = IndexText.render(topic.lead.unnoted).linesIterator.map(_.trim).find(_.nonEmpty)
      val sentence = lead.fold("")(l => l.split("(?<=[.!?])\\s+", 2).headOption.getOrElse(l))
      val day = topic.kind match {
        case Topic.Kind.Meeting(m) => m.when.map(_.day)
        case Topic.Kind.Subject => None
      }
      Quote(key, version, Cut.head(sentence, PersonCards.QuoteChars), day)
    }
  }

  /** What code finds of a person at a floor, by ids alone: the topics they spoke in (by key and
    * version, in the order given, at most [[PersonCards.Quotes]]), the people they share most
    * strands with (most first, ties by id, at most [[PersonCards.Peers]], those with a name),
    * how many rooms and days (in the run's zone, by the last they said in each strand) they
    * spoke in, and the day they last spoke.
    */
  final case class Facts(
      topics: Vector[(TopicKey, DocumentVersionId)],
      peers: Vector[Strand.Name],
      rooms: Int,
      days: Int,
      lastSaid: LocalDate
  ) extends caps.Pure

  object Facts {

    /** What `spoke`, strands with their speakers, shows of `person`, the topics among `topics`
      * (by key and version, newest first) whose passages were written from a strand they spoke
      * in; `None` when they spoke in none.
      */
    def of(
        person: PrincipalId,
        spoke: Vector[(Strand.Digested, Vector[Strand.Spoke])],
        topics: Vector[(TopicKey, DocumentVersionId, Topic)],
        zone: ZoneId
    ): Option[Facts] = {
      val theirs = spoke.filter(_._2.exists(_.person == person))
      val mine = theirs.flatMap(_._2.filter(_.person == person))
      Option.when(theirs.nonEmpty) {
        val ids = theirs.map(_._1.id).toSet
        val spokeIn = topics.filter((_, _, t) =>
          t.passages.exists(_.from.tags.exists {
            case Lineage.Tag.Strand(root, _, _, first) => ids.contains(Strand.Id(root, first))
            case _ => false
          })
        )
        val others = theirs.flatMap(_._2.filter(o => o.person != person && o.name.nonEmpty))
        val peers = others
          .map(_.person)
          .distinct
          .flatMap(p =>
            others
              .findLast(_.person == p)
              .flatMap(_.name)
              .map(n => (p, others.count(_.person == p), n))
          )
          .sortBy((p, shared, _) => (-shared, PrincipalId.value(p)))
          .map(_._3)
        val days = mine.map(m => LocalDate.ofInstant(m.last, zone))
        Facts(
          spokeIn.map((k, v, _) => (k, v)).take(PersonCards.Quotes),
          peers.take(PersonCards.Peers),
          theirs.map(_._1.room).distinct.size,
          days.distinct.size,
          days.maxOption.getOrElse(LocalDate.MIN)
        )
      }
    }
  }

  /** `person`'s card as written on `day` over `before` (none for a first), named `name` and
    * placed in `zone`, from `facts` and the `quotes` of their topics, its lineage `seen`'s
    * strands, newest first, at most [[PersonCards.Lineages]], and its rooms those it had and
    * `seen`'s, but a strand of a room past [[PersonCards.Rooms]], which is left out of both;
    * its paragraph `before`'s.
    */
  def written(
      before: Option[PersonCard],
      key: PersonKey,
      name: Strand.Name,
      zone: Option[ZoneId],
      facts: Facts,
      quotes: Vector[Quote],
      seen: Vector[Strand.Digested],
      day: LocalDate
  ): PersonCard = {
    val rooms = (before.fold(Vector.empty[Place])(_.from) ++ seen.map(_.room)).distinct
    val kept = rooms.take(PersonCards.Rooms).toSet
    val strands = seen.filter(s => kept.contains(s.room)).take(PersonCards.Lineages)
    PersonCard(
      key,
      name,
      zone,
      quotes.take(PersonCards.Quotes),
      facts.peers.take(PersonCards.Peers),
      facts.rooms,
      facts.days,
      facts.lastSaid,
      before.flatMap(_.paragraph),
      Lineage.of(strands.map(s => Lineage.Tag.Strand(s.root, s.room, s.digest, s.id.first))),
      rooms.take(PersonCards.Rooms),
      day
    )
  }

  /** Whether a person whose card is `card` (none when they have none) is due on `day`, having
    * spoken in `fresh` strands it does not hold: at least [[PersonCards.Strands]] of them, and
    * the card written at least [[PersonCards.Every]] before.
    */
  def due(card: Option[PersonCard], fresh: Int, day: LocalDate): Boolean =
    fresh >= PersonCards.Strands &&
      card.forall(c => !c.written.isAfter(day.minusDays(PersonCards.Every.toDays)))

  /** The card `key`'s data `v` holds, or why not. */
  def read(key: PersonKey, v: ujson.Value): Either[String, PersonCard] = {
    def str(o: collection.Map[String, ujson.Value], k: String) =
      o.get(k).flatMap(_.strOpt).toRight(s"a person's card holds $k: no string")
    def int(o: collection.Map[String, ujson.Value], k: String) =
      o.get(k)
        .flatMap(_.numOpt)
        .filter(n => n.isWhole && n >= 0)
        .map(_.toInt)
        .toRight(s"a person's card holds $k: no whole number")
    def day(o: collection.Map[String, ujson.Value], k: String) =
      str(o, k).flatMap(t => Passage.day(t).toRight(s"a person's card holds $k: no day"))
    def name(t: String) = Strand.Name.of(t).toRight(s"a person's card names no one: $t")
    for {
      o <- v.objOpt.toRight("a person's card is no object")
      _ <- Either.cond(
        o.get("v").flatMap(_.numOpt).contains(1.0),
        (),
        "a person's card of no version 1"
      )
      n <- str(o, "name").flatMap(name)
      zone <- o.get("zone") match {
        case None => Right(None)
        case z =>
          str(o, "zone").flatMap(t =>
            Try(ZoneId.of(t)).toOption.map(Some(_)).toRight(s"no zone: $t")
          )
      }
      quotes <- o
        .get("quotes")
        .flatMap(_.arrOpt)
        .toRight("a person's card holds no quotes")
        .flatMap(qs => traverse(qs.toVector)(quote))
      peers <- o
        .get("peers")
        .flatMap(_.arrOpt)
        .toRight("a person's card holds no peers")
        .flatMap(ps =>
          traverse(ps.toVector)(p => p.strOpt.toRight("a peer: no string").flatMap(name))
        )
      rooms <- int(o, "rooms")
      days <- int(o, "days")
      lastSaid <- day(o, "lastSaid")
      paragraph <- o.get("paragraph") match {
        case None => Right(None)
        case _ => str(o, "paragraph").map(Some(_))
      }
      lineage <- TopicJson.readLineage(o.get("lineage"))
      from <- o
        .get("from")
        .flatMap(_.arrOpt)
        .toRight("a person's card holds no rooms")
        .flatMap(rs =>
          traverse(rs.toVector)(r => r.strOpt.toRight("a room: no string").flatMap(Place.read))
        )
      written <- day(o, "written")
    } yield PersonCard(
      key,
      n,
      zone,
      quotes,
      peers,
      rooms,
      days,
      lastSaid,
      paragraph,
      lineage,
      from,
      written
    )
  }

  private def quote(v: ujson.Value): Either[String, Quote] =
    for {
      o <- v.objOpt.toRight("a quote is no object")
      key <- TopicJson.topicKey(o.get("key"), "a quote's key")
      version <- o.get("version").toRight("a quote holds no version").flatMap(TopicJson.version)
      said <- o.get("said").flatMap(_.strOpt).toRight("a quote holds no said")
      day <- o.get("day") match {
        case None => Right(None)
        case d =>
          d.flatMap(_.strOpt).flatMap(Passage.day).map(Some(_)).toRight("a quote's day: no day")
      }
    } yield Quote(key, version, said, day)

  private def data(c: PersonCard): ujson.Value = {
    val o = ujson.Obj(
      "v" -> 1,
      "person" -> DocKey.value(PersonKey.doc(c.key)),
      "name" -> Strand.Name.value(c.name),
      "quotes" -> ujson.Arr.from(c.quotes.map { q =>
        val qo = ujson.Obj(
          "key" -> TopicKey.value(q.key),
          "version" -> DocumentVersionId.value(q.version).toDouble,
          "said" -> q.said
        )
        q.day.foreach(d => qo("day") = d.toString)
        qo
      }),
      "peers" -> ujson.Arr.from(c.peers.map(p => ujson.Str(Strand.Name.value(p)))),
      "rooms" -> c.rooms,
      "days" -> c.days,
      "lastSaid" -> c.lastSaid.toString,
      "lineage" -> TopicJson.lineage(c.lineage),
      "from" -> ujson.Arr.from(c.from.map(r => ujson.Str(r.written))),
      "written" -> c.written.toString
    )
    c.zone.foreach(z => o("zone") = z.getId)
    c.paragraph.foreach(p => o("paragraph") = p)
    o
  }

  private def holds(lineage: Lineage, id: Strand.Id): Boolean = lineage.tags.exists {
    case Lineage.Tag.Strand(root, _, _, first) => Strand.Id(root, first) == id
    case _ => false
  }

  // Draft wording, for Nick (grit/568): every line of a card's text.
  private def render(c: PersonCard): String = {
    val name = Strand.Name.value(c.name)
    val (meetings, subjects) = c.quotes.partition(_.day.nonEmpty)
    val talks = Option.when(c.quotes.nonEmpty) {
      val held = Option.when(meetings.nonEmpty)(
        meetings
          .map(m => s"${m.said} on ${m.day.fold("")(_.toString)}")
          .mkString("(meetings: ", "; ", ")")
      )
      "Talks about: " + (subjects.map(_.said).mkString("; ") +: held.toVector)
        .filter(_.nonEmpty)
        .mkString(" ")
    }
    val works = Option.when(c.peers.nonEmpty)(
      c.peers.map(Strand.Name.value).mkString("Works most with: ", ", ", "")
    )
    val active =
      s"Active in ${c.rooms} rooms on ${c.days} days lately; last spoke ${c.lastSaid}." +
        c.zone.fold("")(z => s" Time zone: ${z.getId}.")
    (Vector(
      s"$name, as grit sees them from the team's talk"
    ) ++ c.paragraph ++ talks ++ works :+ active)
      .mkString("\n")
  }
}

/** A person's card's key, `person:` then their principal id: it is its document's key
  * ([[PersonKey.doc]]).
  */
opaque type PersonKey = DocKey

object PersonKey {

  /** The key of `person`'s card, or why not: too long for a document's key, or holding a
    * control character.
    */
  def of(person: PrincipalId): Either[String, PersonKey] =
    DocKey.of(Prefix + PrincipalId.value(person))

  /** The card key `key` is, or `None` when it is no card's. */
  def read(key: DocKey): Option[PersonKey] = {
    val text = DocKey.value(key)
    Option.when(text.startsWith(Prefix) && text.length > Prefix.length)(key)
  }

  def doc(key: PersonKey): DocKey = key

  given CanEqual[PersonKey, PersonKey] = CanEqual.derived

  private val Prefix = "person:"
}

/** What a topics run's `people` step reads, and when a card is written and forgotten: it reads
  * the strands spoken in within [[Window]] (30 days), at most [[Scanned]] (200), newest
  * touched first; a person is due at least [[Every]] (7 days) after their card was written,
  * once they spoke in at least [[Strands]] (5) strands it does not hold, at most [[PerRun]] (4)
  * a run; a card goes once its person last spoke more than [[KeepFor]] (90 days) before.
  */
object PersonCards {
  val Window: FiniteDuration = 30.days
  val Scanned: Int = 200
  val Every: FiniteDuration = 7.days
  val Strands: Int = 5
  val PerRun: Int = 4
  val KeepFor: FiniteDuration = 90.days

  /** 32: the most rooms a card is written from; a strand of a room past them is left out. */
  val Rooms: Int = 32

  /** 32: the most strands a card's lineage holds, the newest. */
  val Lineages: Int = 32

  /** 8: the most topics a card quotes. */
  val Quotes: Int = 8

  /** 160: the most characters of a topic's sentence a card quotes. */
  val QuoteChars: Int = 160

  /** 5: the most people a card names as worked with. */
  val Peers: Int = 5

  /** 300: the most characters of a card's paragraph. */
  val ParagraphChars: Int = 300

  /** 1,500: the most characters of a card's text. */
  val TextChars: Int = 1500

  /** 6: the most strands, their newest, a card's ask is shown. */
  val Asked: Int = 6

  /** 1,500: the most characters of each strand a card's ask is shown. */
  val AskChars: Int = 1500
}
