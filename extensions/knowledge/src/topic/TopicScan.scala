package grit.knowledge.topic

import java.time.{Instant, LocalDate}

import grit.core.document.{DocRef, Document, DocumentKeeper}
import grit.core.durable.Journaled
import grit.core.id.{ConversationId, DocKey, DocumentVersionId, EntryId}
import grit.core.need.{Exported, Needs}
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label
import grit.knowledge.card.{CardShelf, UnitKind}
import grit.knowledge.ref.{CardKey, Ref, TopicKey}

/** A topics run's scan: by ids alone, what its asks are made of. */
private[topic] object TopicScan {

  /** A topic a run attaches to: its `key`, its current `version` (`None` for one this run
    * opens), the `strands` and `cards` attached, and the `kind` it opens as.
    */
  final case class Touch(
      key: TopicKey,
      version: Option[DocumentVersionId],
      strands: Vector[Strand.Digested],
      cards: Vector[CardShelf.Seen],
      kind: Topic.Kind = Topic.Kind.Subject
  ) extends caps.Pure {
    def opened: Boolean = version.isEmpty
  }

  /** What a scan came to: the topics `attached` to by ref; the `strands` and `cards` the
    * run's sorts place; every topic kept at the floor (`topics`, by key and version, newest
    * first); for each strand to place, by its id, the topics it shares something with
    * (`near`, at most [[TopicDream.Structural]]: those whose passages were written from the
    * same strand, then those holding a card its messages link, then those written from a strand
    * of its room, each newest first); how many passages it `withdrew`, and how many strands and
    * cards it found `unchanged` or `left` past the run's most; and how many of the floor's
    * topics it withdrew for data this reader refuses (`unreadable`), not passages, and how many
    * it left as they are for data of a later reader's version (`later`); and the topics a run
    * sorts against the others to merge (`single`, [[TopicScan.single]] of the floor's topics
    * after its withdrawals, at most `merges`); and the floor's `meetings`, newest first.
    */
  final case class Scanned(
      attached: Vector[Touch],
      strands: Vector[Strand.Digested],
      cards: Vector[CardShelf.Seen],
      topics: Vector[(TopicKey, DocumentVersionId)],
      near: Vector[(Strand.Id, Vector[TopicKey])],
      withdrew: Int,
      unchanged: Int,
      left: Int,
      linked: Vector[Linked],
      unreadable: Int,
      later: Int,
      single: Vector[(TopicKey, DocumentVersionId)],
      meetings: Vector[Live] = Vector.empty
  ) extends caps.Pure {

    /** The card units `strands` link, and the cards paired with them, each once. */
    def of(strands: Vector[Strand.Digested]): (Vector[Ref], Vector[CardShelf.Seen]) = {
      val ids = strands.map(_.id).toSet
      val mine = linked.filter(l => ids.contains(l.strand))
      (mine.flatMap(_.units).distinct, mine.flatMap(_.cards).distinctBy(_.version))
    }

    /** The topics the strand `id` shares something with ([[near]]), in that order. */
    def nearOf(id: Strand.Id): Vector[TopicKey] =
      near.collectFirst { case (r, ks) if r == id => ks }.getOrElse(Vector.empty)

    /** The meetings a strand said on `day` is pinned, at most [[Meetings]], newest first: those
      * whose day is from [[Meeting.Grace]] days before `day` to 14 after. One with no day has
      * none to be near, and is never pinned: pinned on its newest passage, each join renewed the
      * pin.
      */
    def meetingsNear(day: LocalDate): Vector[TopicKey] =
      meetings
        .filter(m =>
          m.day.exists(d =>
            !d.isBefore(day.minusDays(Meeting.Grace)) && !d.isAfter(day.plusDays(14))
          )
        )
        .map(_.key)
        .take(Meetings)

    /** The meetings whose day is more than [[Meeting.Grace]] days before `day`: passed, they
      * take no talk said on `day`, whatever their state.
      */
    def passedBy(day: LocalDate): Set[TopicKey] =
      meetings.filter(_.day.exists(_.isBefore(day.minusDays(Meeting.Grace)))).map(_.key).toSet

    /** What the topic `key` may be folded with, as the scan read it: a subject when it is none
      * of the floor's `meetings` (one opened since the scan among them).
      */
    def foldable(key: TopicKey): Topic.Foldable =
      meetings.find(_.key == key).fold(Topic.Foldable.Subject)(m => Topic.Foldable.meeting(m.when))
  }

  /** Of each topic of `topics`, the current versions of at most [[Holds]] of the cards of the
    * units it refers to written by `until`, most recently written first: those a write ask
    * is shown besides the cards attached that day.
    */
  final case class Holding(topics: Vector[(TopicKey, Vector[DocumentVersionId])])
      extends caps.Pure {

    /** The versions of the cards `topic` holds: none for one not among them. */
    def of(topic: TopicKey): Vector[DocumentVersionId] =
      topics.collectFirst { case (k, vs) if k == topic => vs }.getOrElse(Vector.empty)
  }

  /** A strand in play that links a card's unit: its `strand`, the card `units` its messages link
    * (at most [[Links]]), and the `cards` paired with it, each a card of one of those units no
    * strand before it in the run links, whatever its age or whether it was set aside: it goes
    * where the strand goes, never sorted.
    */
  final case class Linked(strand: Strand.Id, units: Vector[Ref], cards: Vector[CardShelf.Seen])
      extends caps.Pure

  /** A meeting kept at the floor, as a sort may be pinned it: its `key` and `version`, and
    * `when` it is proposed, planned or held for, when it has a day.
    */
  final case class Live(key: TopicKey, version: DocumentVersionId, when: Option[Meeting.When])
      extends caps.Pure {
    def day: Option[LocalDate] = when.map(_.day)
  }

  /** 8: the most meetings a strand's sort is pinned ([[Scanned.meetingsNear]]). */
  val Meetings: Int = 8

  /** 20: the most cards one run sorts. */
  val Cards: Int = 20

  /** 10: the most card units a scan keeps of one strand's links. */
  val Links: Int = 10

  /** 20: the most cards a topic holds that its write ask is shown besides those attached. */
  val Holds: Int = 20

  /** The scan at `until`, for the run's `day`: its floor's topics read, one whose data is refused
    * withdrawn and one of a later version left as it is ([[Topic.Unread]]); from each, the
    * passages naming a strand whose room is at another label now, or dated more than `keepFor`
    * before `day`, withdrawn, the topic itself when none is left (a passage whose strand's
    * messages are gone is kept);
    * then the strands at the floor spoken in within `recent` before `until`, and the cards
    * changed within `cardRecent` before `until`: the floor's [[Aside]] kept to those among them,
    * and to its topics, at the digest or version set aside, and set aside within `asideFor`
    * before `until`; each already in a topic's lineage or the aside at its
    * digest or version, or referred to by a topic of a later version, unchanged, each a topic refers to attached to it, and of the rest at most
    * `most` strands and [[Cards]] cards of files kept at the floor to place, oldest first, the
    * others left (a card kept below the floor is its own label's run's to place); the cards read
    * back until [[Cards]] of them are such files in no lineage, the aside or a pairing, so
    * successive runs page through them. Each strand
    * attached or to place is [[Linked]] to the cards its messages link, read whatever their age
    * and taken out of the aside and of those to place.
    */
  def scanned(
      params: TopicParams,
      most: Int,
      until: Instant,
      day: LocalDate,
      cards: Option[Exported[CardShelf]],
      keeper: DocumentKeeper,
      strands: StrandShelf,
      needs: Needs,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Scanned] = {
    val floor = Tx.floor(tx)
    val shelf = cards.flatMap(e => needs.of(e).toOption)
    held(keeper, floor, at).flatMap((read: (Vector[Found], Int, Set[TopicKey])) =>
      pruned(keeper, read._1, floor, day.minusDays(params.keepFor.toDays), at).flatMap(
        (kept: Vector[Pruned]) =>
          strands
            .units(until.minusSeconds(params.recent.toSeconds), until, params.cut)
            .flatMap((recent: Vector[Strand.Digested]) =>
              Aside
                .read(keeper, floor)
                .flatMap { (all: Vector[Aside.Item]) =>
                  val since = until.minusSeconds(params.asideFor.toSeconds)
                  val held = all.filterNot(_.at.isBefore(since))
                  val topics = kept.flatMap(_.topic)
                  val keys = topics.map(_._1).toSet
                  val lineage = (topics.flatMap(_._3.topic.passages.flatMap(_.from.tags)) ++
                    held.map(_.tag)).toSet
                  def unplaced(c: CardShelf.Seen) = !lineage.contains(Lineage.Tag.Card(c.version))
                  referrers(
                    keeper,
                    keys,
                    read._3,
                    recent.filterNot(s => lineage.exists(isStrand(s)))
                  )(s =>
                    // A run has no ref: settled, it never grows, so none attaches by one.
                    Option.when(s.span == Strand.Span.Whole)(Ref.Strand(s.root))
                  ).flatMap((ss: Vector[(Strand.Digested, Option[TopicKey])]) =>
                    linking(strands, shelf, inPlay(most, ss), until).flatMap {
                      (linked: Vector[Linked]) =>
                        val paired = linked.flatMap(_.cards.map(_.version)).toSet
                        // Read back until Cards could be offered, so successive runs page through
                        // them: a directory's card, one kept at another label, or one placed, set
                        // aside or paired, is not.
                        changed(
                          shelf,
                          until.minusSeconds(params.cardRecent.toSeconds),
                          until,
                          Cards,
                          c => offered(c, floor) && unplaced(c) && !paired.contains(c.version)
                        ).flatMap((seen: Vector[CardShelf.Seen]) =>
                          referrers(keeper, keys, read._3, seen.filter(unplaced))(_.unit)
                            .flatMap((cs: Vector[(CardShelf.Seen, Option[TopicKey])]) => {
                              val p = Placed(ss, cs)
                              val versions = topics.map(_._2).toSet
                              aside(keeper, floor, all, held, recent, seen, linked, versions, at)
                                .flatMap { (now: Vector[Aside.Item]) =>
                                  Right(
                                    gathered(most, floor, kept, recent, seen, p, linked).copy(
                                      unreadable = read._2,
                                      later = read._3.size,
                                      single = single(
                                        topics.map((k, v, h) => (k, v, h.topic)),
                                        now,
                                        params.merges
                                      ),
                                      meetings = live(topics)
                                    )
                                  )
                                }
                            })
                        )
                    }
                  )
                }
            )
      )
    )
  }

  /** The meetings among `topics`, newest written first. */
  private def live(
      topics: Vector[(TopicKey, DocumentVersionId, TopicWrite.Held)]
  ): Vector[Live] =
    topics
      .sortBy((_, v, _) => -DocumentVersionId.value(v))
      .collect { case (k, v, h) =>
        h.topic.kind match {
          case Topic.Kind.Meeting(m) => Some(Live(k, v, m.when))
          case Topic.Kind.Subject => None
        }
      }
      .flatten

  /** Of `strands`, those a run takes up: those attached by ref, then at most `most` to place. */
  private def inPlay(
      most: Int,
      strands: Vector[(Strand.Digested, Option[TopicKey])]
  ): Vector[Strand.Digested] =
    strands.collect { case (s, Some(_)) => s } ++ strands
      .collect { case (s, None) => s }
      .take(most)

  /** Each of `strands` that links a card's unit, with the cards of those units `shelf` reads
    * written by `until`, each paired with the first strand linking it.
    */
  private def linking(
      strands: StrandShelf,
      shelf: Option[CardShelf],
      in: Vector[Strand.Digested],
      until: Instant
  )(using Tx^): Either[StoreError, Vector[Linked]] =
    shelf.fold[Either[StoreError, Vector[Linked]]](Right(Vector.empty)) { cards =>
      each(in)((s: Strand.Digested) =>
        strands
          .links(s)
          .map(ls => s.id -> ls.map(Ref.of).filter(isUnit).distinct.take(Links))
      ).flatMap { (units: Vector[(Strand.Id, Vector[Ref])]) =>
        val named = units.filter(_._2.nonEmpty)
        cards.linked(named.flatMap(_._2).toSet, until, Cards).map {
          (found: Vector[CardShelf.Seen]) =>
            val first = found
              .flatMap(c => named.find(_._2.exists(u => c.unit.contains(u))).map(n => c -> n._1))
            named
              .map((root, us) => Linked(root, us, first.collect { case (c, r) if r == root => c }))
        }
      }
    }

  /** The cards `topics`, each current at the floor, hold ([[Holding]]), through the cards' plugin
    * when it is among the needs; a topic holding none, or no longer current, left out.
    */
  def holding(
      topics: Vector[TopicKey],
      until: Instant,
      cards: Option[Exported[CardShelf]],
      keeper: DocumentKeeper,
      needs: Needs
  )(using tx: Tx^): Either[StoreError, Holding] =
    cards
      .flatMap(e => needs.of(e).toOption)
      .fold[Either[StoreError, Holding]](
        Right(Holding(Vector.empty))
      ) { shelf =>
        each(topics)(k =>
          keeper
            .current(TopicKey.doc(k), Tx.floor(tx))
            .map(
              _.flatMap(d => TopicWrite.read(k, d.data).toOption)
                .map(h => k -> h.units.filter(isUnit).toSet)
            )
        ).flatMap { (read: Vector[Option[(TopicKey, Set[Ref])]]) =>
          val held = read.flatten
          val units = held.flatMap(_._2).toSet
          // A unit has one current card, so this reads every one held.
          shelf.linked(units, until, units.size).map { (found: Vector[CardShelf.Seen]) =>
            Holding(
              held
                .map((k, us) =>
                  k -> found.filter(_.unit.exists(us.contains)).map(_.version).take(Holds)
                )
                .filter(_._2.nonEmpty)
            )
          }
        }
      }

  private def isUnit(r: Ref): Boolean = r match {
    case Ref.Drive(_) | Ref.GitHub(_, _, _) => true
    case _ => false
  }

  /** A topic at its run's floor: its key, its document and what that holds. */
  private final case class Found(key: TopicKey, document: Document, held: TopicWrite.Held)

  /** A topic after its scan's withdrawals: its key, version and what it holds, unless
    * withdrawn; and how many of its passages were.
    */
  private final case class Pruned(
      topic: Option[(TopicKey, DocumentVersionId, TopicWrite.Held)],
      withdrew: Int
  )

  /** The strands and cards in no topic's lineage at their digest or version, each with the
    * topic that refers to it, if any.
    */
  private final case class Placed(
      strands: Vector[(Strand.Digested, Option[TopicKey])],
      cards: Vector[(CardShelf.Seen, Option[TopicKey])]
  )

  /** The floor's topics, one whose data this reader refuses withdrawn, one of a later version
    * left as it is; its [[Aside]] is none. Also how many were withdrawn, and the keys left.
    */
  private def held(keeper: DocumentKeeper, floor: Label, at: Instant)(using
      Tx^
  ): Either[StoreError, (Vector[Found], Int, Set[TopicKey])] =
    keeper.newest(Int.MaxValue).flatMap { (documents: Vector[Document]) =>
      val read: Vector[(Document, Either[Topic.Unread, (TopicKey, TopicWrite.Held)])] =
        documents
          .filter(d => d.label == floor && d.key != Aside.key)
          .map(d =>
            d -> TopicKey
              .read(d.key)
              .toRight(Topic.Unread.Refused("no topic's key"))
              .flatMap(k => TopicWrite.read(k, d.data).map(k -> _))
          )
      val refused = read.collect { case (d, Left(Topic.Unread.Refused(_))) => d.key }
      each(refused)(k => keeper.withdraw(k, floor, at))
        .map(_ =>
          (
            read.collect { case (d, Right((k, h))) => Found(k, d, h) },
            refused.size,
            read
              .collect { case (d, Left(Topic.Unread.Later(_))) => TopicKey.read(d.key) }
              .flatten
              .toSet
          )
        )
    }

  /** Each of `found` with its passages dated before `since`, or naming a strand of a room whose
    * label is not `floor` now, withdrawn, the topic itself when none is left.
    */
  private def pruned(
      keeper: DocumentKeeper,
      found: Vector[Found],
      floor: Label,
      since: LocalDate,
      at: Instant
  )(using Tx^): Either[StoreError, Vector[Pruned]] =
    each(found) { (f: Found) =>
      val passages = f.held.topic.passages
      val gone = passages.filter(p =>
        p.day.isBefore(since) || p.from.tags.exists {
          case Lineage.Tag.Strand(_, room, _, _) => Tx.roomLabel(room) != floor
          case _ => false
        }
      )
      if (gone.isEmpty) Right(Pruned(Some((f.key, f.document.version, f.held)), 0))
      else if (gone.size == passages.size)
        keeper.withdraw(f.document.key, floor, at).map(_ => Pruned(None, gone.size))
      else {
        val left = f.held.copy(topic = Topic.of(f.held.topic.lead, passages.diff(gone)))
        TopicWrite
          .kept(keeper, f.key, floor, left, at)
          .flatMap(_ => keeper.current(f.document.key, floor))
          .map(now => Pruned(now.map(n => (f.key, n.version, left)), gone.size))
      }
    }

  /** The cards changed from `since` until `until`, up to the `n`th `counts` holds to, through
    * `shelf`; none when there is none (no such plugin, or it is not among the needs).
    */
  private def changed(
      shelf: Option[CardShelf],
      since: Instant,
      until: Instant,
      n: Int,
      counts: CardShelf.Seen -> Boolean
  )(using Tx^): Either[StoreError, Vector[CardShelf.Seen]] =
    shelf.fold(Right(Vector.empty))(_.changed(since, until, n, counts))

  /** The floor's aside, `all` of it, kept to those `held` (not expired) that are strands of
    * `recent` and cards of `seen` a sort may be shown ([[offered]]), at the digest or
    * version they are now, and topics at a version of `topics`, less the cards `linked`
    * pairs: the aside after. A card kept at another label or a directory's leaves it, since no
    * run at `floor` asks of it again, and so does a topic written since it was set aside.
    */
  private def aside(
      keeper: DocumentKeeper,
      floor: Label,
      all: Vector[Aside.Item],
      held: Vector[Aside.Item],
      recent: Vector[Strand.Digested],
      seen: Vector[CardShelf.Seen],
      linked: Vector[Linked],
      topics: Set[DocumentVersionId],
      at: Instant
  )(using Tx^): Either[StoreError, Vector[Aside.Item]] = {
    val paired = linked.flatMap(_.cards.map(_.version)).toSet
    val now = held.filter(_.tag match {
      case t @ Lineage.Tag.Strand(_, _, _, _) => recent.exists(s => isStrand(s)(t))
      case Lineage.Tag.Card(v) =>
        seen.exists(c => c.version == v && offered(c, floor)) && !paired.contains(v)
      case Lineage.Tag.Topic(v) => topics.contains(v)
    })
    if (now == all) Right(all) else Aside.kept(keeper, floor, now, at).map(_ => now)
  }

  /** Each of `as`, with the topic among `held` that refers to what `ref` reads of it, if any;
    * one a topic of `later` refers to left out, as that topic is.
    */
  private def referrers[A](
      keeper: DocumentKeeper,
      held: Set[TopicKey],
      later: Set[TopicKey],
      as: Vector[A]
  )(ref: A -> Option[Ref])(using Tx^): Either[StoreError, Vector[(A, Option[TopicKey])]] =
    each(as)((a: A) => ref(a).fold(Right(None))(referred(keeper, _, held ++ later)).map(a -> _))
      .map(_.filterNot(_._2.exists(later.contains)))

  /** The scan's result, from what its reads found. */
  private def gathered(
      most: Int,
      floor: Label,
      kept: Vector[Pruned],
      recent: Vector[Strand.Digested],
      seen: Vector[CardShelf.Seen],
      p: Placed,
      linked: Vector[Linked]
  ): Scanned = {
    val paired = linked.flatMap(_.cards.map(_.version)).toSet
    val topics = kept.flatMap(_.topic)
    val versions = topics.map((k, v, _) => k -> v).toMap
    val attached = (p.strands.flatMap(_._2) ++ p.cards.flatMap(_._2)).distinct.map(k =>
      Touch(
        k,
        versions.get(k),
        p.strands.collect { case (s, Some(to)) if to == k => s },
        p.cards.collect { case (c, Some(to)) if to == k => c }
      )
    )
    val strands = p.strands.collect { case (s, None) => s }
    // Most recent first as the shelf gives them: placed oldest first, as strands are.
    val cards = p.cards.collect {
      case (c, None) if !paired.contains(c.version) && offered(c, floor) => c
    }.reverse
    val newest = topics.sortBy((_, v, _) => -DocumentVersionId.value(v))
    val inPlay = strands.take(most)
    Scanned(
      attached,
      inPlay,
      cards.take(Cards),
      newest.map((k, v, _) => (k, v)),
      inPlay.map(s => s.id -> sharing(s, newest, linked)),
      kept.map(_.withdrew).sum,
      (recent.size - p.strands.size) +
        seen.count(c => !p.cards.exists(_._1.version == c.version) && !paired.contains(c.version)),
      (strands.size - most).max(0) + (cards.size - Cards).max(0),
      linked,
      0,
      0,
      Vector.empty
    )
  }

  /** Of `topics`, by key and version, those written from fewer than two discussions (strands,
    * by root) that `aside` does not hold at their version and another of `topics` may fold with
    * ([[Topic.Foldable]]), oldest written first, at most `merges`.
    */
  def single(
      topics: Vector[(TopicKey, DocumentVersionId, Topic)],
      aside: Vector[Aside.Item],
      merges: Int
  ): Vector[(TopicKey, DocumentVersionId)] = {
    val set = aside.collect { case Aside.Item(Lineage.Tag.Topic(v), _) => v }.toSet
    val kinds = topics.map((_, _, t) => Topic.Foldable.of(t.kind))
    topics
      .filter((_, v, t) =>
        !set.contains(v) &&
          // Itself among them, so another when two or more.
          kinds.count(Topic.Foldable.of(t.kind).foldsWith) >= 2 &&
          t.passages
            .flatMap(_.from.tags)
            .collect { case Lineage.Tag.Strand(root, _, _, first) => (root, first) }
            .distinct
            .size < 2
      )
      .sortBy((_, v, _) => DocumentVersionId.value(v))
      .take(merges.max(0))
      .map((k, v, _) => (k, v))
  }

  /** Of `newest`, the topics `s` shares something with, as [[Scanned.near]] orders them. */
  private def sharing(
      s: Strand.Digested,
      newest: Vector[(TopicKey, DocumentVersionId, TopicWrite.Held)],
      linked: Vector[Linked]
  ): Vector[TopicKey] = {
    def from(h: TopicWrite.Held) = h.topic.passages.flatMap(_.from.tags).collect {
      case t: Lineage.Tag.Strand => t
    }
    val units = linked.filter(_.strand == s.id).flatMap(_.units).toSet
    (newest.filter((_, _, h) => from(h).exists(t => Strand.Id(t.root, t.first) == s.id)) ++
      newest.filter((_, _, h) => h.units.exists(units.contains)) ++
      newest.filter((_, _, h) => from(h).exists(_.room == s.room)))
      .map(_._1)
      .distinct
      .take(TopicDream.Structural)
  }

  /** Whether a sort of a run at `floor` may be shown `c`: a file's card kept at `floor`.
    * A directory's card describes a container, not a matter, and one kept below `floor` is its
    * own label's run's to place: either attaches only by ref or a strand's link.
    */
  private def offered(c: CardShelf.Seen, floor: Label): Boolean =
    c.kind == UnitKind.File && c.label == floor

  private def isStrand(s: Strand.Digested)(t: Lineage.Tag): Boolean =
    t.strand.contains((s.id, s.digest))

  /** The topic among `held` that refers to `ref`, the most recently written, if any. */
  private def referred(
      keeper: DocumentKeeper,
      ref: Ref,
      held: Set[TopicKey]
  )(using Tx^): Either[StoreError, Option[TopicKey]] =
    Ref.written(ref) match {
      case Left(_) => Right(None)
      case Right(r) =>
        keeper
          .referring(r, Int.MaxValue)
          .map(_.iterator.flatMap(d => TopicKey.read(d.key)).find(held.contains))
    }

  private def each[A, B](
      as: Vector[A]
  )(f: A => Either[StoreError, B]): Either[StoreError, Vector[B]] =
    as.foldLeft[Either[StoreError, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(done => f(a).map(done :+ _))
    )

  /** Its record: `{"attached": [...], "strands": [...], "cards": [...], "topics": [...],
    * "near": [{"root", "topics": [...]}], "withdrew", "unchanged", "left", "linked": [...],
    * "single": [...]}`, and `"unreadable"` when some topic was withdrawn for data this reader
    * refuses, `"later"` when some was left for data of a later version; read as linking none
    * when `linked` is absent, offering none to merge when `single` is, and none unreadable or
    * later when `unreadable` or `later` is.
    */
  given Journaled[Scanned] = Journaled.json(
    s => {
      val record = ujson.Obj(
        "attached" -> ujson.Arr.from(s.attached.map(touch)),
        "strands" -> ujson.Arr.from(s.strands.map(Strand.Digested.write)),
        "cards" -> ujson.Arr.from(s.cards.map(card)),
        "topics" -> ujson.Arr.from(s.topics.map((k, v) => lead(k, v))),
        "near" -> ujson.Arr.from(s.near.map(nearJson)),
        "withdrew" -> s.withdrew,
        "unchanged" -> s.unchanged,
        "left" -> s.left,
        "linked" -> ujson.Arr.from(s.linked.map(linkedJson)),
        "single" -> ujson.Arr.from(s.single.map((k, v) => lead(k, v)))
      )
      // Written only when some were, so a scan with none records as it did before the count.
      if (s.meetings.nonEmpty) record("meetings") = ujson.Arr.from(s.meetings.map(liveJson))
      if (s.unreadable > 0) record("unreadable") = ujson.Num(s.unreadable.toDouble)
      if (s.later > 0) record("later") = ujson.Num(s.later.toDouble)
      record
    },
    v =>
      for {
        o <- v.objOpt.toRight("a topics scan is an object")
        attached <- list(o.get("attached"), "attached")(readTouch)
        strands <- list(o.get("strands"), "strands")(Strand.Digested.read)
        cards <- list(o.get("cards"), "cards")(readCard)
        topics <- list(o.get("topics"), "topics")(readLead)
        near <- list(o.get("near"), "near")(readNear)
        withdrew <- count(o.get("withdrew"), "withdrew")
        unchanged <- count(o.get("unchanged"), "unchanged")
        left <- count(o.get("left"), "left")
        // A scan recorded before strands were linked holds none.
        linked <- list(o.get("linked").orElse(Some(ujson.Arr())), "linked")(readLinked)
        // A scan recorded before a topic whose data no longer read was counted withdraws none.
        unreadable <- count(o.get("unreadable").orElse(Some(ujson.Num(0))), "unreadable")
        // A scan recorded before a later version's topic was left and counted leaves none.
        later <- count(o.get("later").orElse(Some(ujson.Num(0))), "later")
        // A scan recorded before topics were merged offers none.
        single <- list(o.get("single").orElse(Some(ujson.Arr())), "single")(readLead)
        // A scan recorded before meetings were pinned pins none.
        meetings <- list(o.get("meetings").orElse(Some(ujson.Arr())), "meetings")(readLive)
      } yield Scanned(
        attached,
        strands,
        cards,
        topics,
        near,
        withdrew,
        unchanged,
        left,
        linked,
        unreadable,
        later,
        single,
        meetings
      )
  )

  /** Its record: `[{"key", "cards": [...]}]`, each topic with the versions it holds. */
  given Journaled[Holding] = Journaled.json(
    h => ujson.Arr.from(h.topics.map(holdsJson)),
    v => list(Some(v), "holds")(readHolds).map(Holding(_))
  )

  private def nearJson(n: (Strand.Id, Vector[TopicKey])): ujson.Value =
    idJson(n._1, "topics" -> ujson.Arr.from(n._2.map(k => ujson.Str(TopicKey.value(k)))))

  private def readNear(v: ujson.Value): Either[String, (Strand.Id, Vector[TopicKey])] =
    for {
      o <- v.objOpt.toRight("a strand's near topics are an object")
      id <- readId(o, "near topics")
      topics <- list(o.get("topics"), "a strand's near topics")(t => topicKey(Some(t)))
    } yield (id, topics)

  /** A strand's id as `"root"`, and `"first"` for a run alone, so a whole strand's records
    * read as before; then `rest`.
    */
  private def idJson(id: Strand.Id, rest: (String, ujson.Value)*): ujson.Obj =
    ujson.Obj.from(
      (("root" -> ujson.Str(ConversationId.value(id.root))) +:
        id.first.toVector.map(f => "first" -> ujson.Str(EntryId.value(f)))) ++ rest
    )

  private def readId(o: collection.Map[String, ujson.Value], what: String) =
    for {
      root <- o.get("root").flatMap(_.strOpt).toRight(s"$what with no root")
      first <- o.get("first") match {
        case None => Right(None)
        case Some(f) => f.strOpt.map(t => Some(EntryId(t))).toRight(s"$what with a bad first")
      }
    } yield Strand.Id(ConversationId(root), first)

  private def liveJson(m: Live): ujson.Value = {
    val o = lead(m.key, m.version)
    m.when.foreach { w =>
      o("day") = w.day.toString
      w.start.foreach(t => o("start") = t.toString)
    }
    o
  }

  private def readLive(v: ujson.Value): Either[String, Live] = {
    def day(key: String) = v.objOpt.flatMap(_.get(key)) match {
      case None => Right(None)
      case Some(d) =>
        d.strOpt
          .flatMap(Passage.day)
          .map(Some(_))
          .toRight(s"a live meeting's $key is no date")
    }
    for {
      kv <- readLead(v)
      // A scan recorded before v10 also holds a "newest", now read by none.
      d <- day("day")
      start <- v.objOpt.flatMap(_.get("start")) match {
        case None => Right(None)
        case Some(t) =>
          t.strOpt
            .flatMap(s => scala.util.Try(java.time.LocalTime.parse(s)).toOption)
            .map(Some(_))
            .toRight("a live meeting's start is no time")
      }
      when <- (d, start) match {
        case (None, Some(_)) => Left("a live meeting has a start and no day")
        case _ => Right(d.map(Meeting.When(_, start)))
      }
    } yield Live(kv._1, kv._2, when)
  }

  private def holdsJson(h: (TopicKey, Vector[DocumentVersionId])): ujson.Value = ujson.Obj(
    "key" -> TopicKey.value(h._1),
    "cards" -> ujson.Arr.from(h._2.map(v => ujson.Num(DocumentVersionId.value(v).toDouble)))
  )

  private def readHolds(v: ujson.Value): Either[String, (TopicKey, Vector[DocumentVersionId])] =
    for {
      o <- v.objOpt.toRight("a topic's holding is an object")
      key <- topicKey(o.get("key"))
      cards <- list(o.get("cards"), "a topic's cards held")(version)
    } yield (key, cards)

  private def linkedJson(l: Linked): ujson.Value = idJson(
    l.strand,
    "units" -> ujson.Arr.from(l.units.flatMap(u => Ref.written(u).toOption).map(refJson)),
    "cards" -> ujson.Arr.from(l.cards.map(card))
  )

  private def readLinked(v: ujson.Value): Either[String, Linked] =
    for {
      o <- v.objOpt.toRight("a linked strand is an object")
      root <- readId(o, "a linked strand")
      units <- list(o.get("units"), "a linked strand's units")(u =>
        readRef(u).toRight(s"no unit: ${ujson.write(u)}")
      )
      cards <- list(o.get("cards"), "a linked strand's cards")(readCard)
    } yield Linked(root, units, cards)

  private def refJson(r: DocRef): ujson.Value =
    ujson.Obj("kind" -> r.kind, "ref" -> r.ref)

  private def readRef(v: ujson.Value): Option[Ref] =
    for {
      u <- v.objOpt
      kind <- u.get("kind").flatMap(_.strOpt)
      ref <- u.get("ref").flatMap(_.strOpt)
      r <- DocRef.of(kind, ref).toOption.flatMap(Ref.read)
    } yield r

  private def touch(t: Touch): ujson.Value = {
    val o = ujson.Obj(
      "key" -> TopicKey.value(t.key),
      "strands" -> ujson.Arr.from(t.strands.map(Strand.Digested.write)),
      "cards" -> ujson.Arr.from(t.cards.map(card))
    )
    t.version.foreach(v => o("version") = DocumentVersionId.value(v).toDouble)
    o
  }

  private def readTouch(v: ujson.Value): Either[String, Touch] =
    for {
      o <- v.objOpt.toRight("a touch is an object")
      key <- topicKey(o.get("key"))
      version <- o.get("version").fold(Right(None))(n => version(n).map(Some(_)))
      strands <- list(o.get("strands"), "a touch's strands")(Strand.Digested.read)
      cards <- list(o.get("cards"), "a touch's cards")(readCard)
    } yield Touch(key, version, strands, cards)

  private def card(c: CardShelf.Seen): ujson.Value = {
    val o = ujson.Obj(
      "version" -> DocumentVersionId.value(c.version).toDouble,
      "key" -> CardKey.value(c.key),
      "title" -> c.title,
      "kind" -> c.kind.word,
      "label" -> Label.written(c.label)
    )
    c.unit.flatMap(u => Ref.written(u).toOption).foreach(r => o("unit") = refJson(r))
    o
  }

  private def readCard(v: ujson.Value): Either[String, CardShelf.Seen] =
    for {
      o <- v.objOpt.toRight("a card seen is an object")
      version <- o.get("version").toRight("a card seen with no version").flatMap(version)
      key <- o
        .get("key")
        .flatMap(_.strOpt)
        .toRight("a card seen with no key")
        .flatMap(CardKey.parse)
      title <- o.get("title").flatMap(_.strOpt).toRight("a card seen with no title")
      // Recorded since version 3, before which a run is superseded, never replayed.
      label <- o
        .get("label")
        .flatMap(_.strOpt)
        .toRight("a card seen with no label")
        .flatMap(Label.read)
    } yield CardShelf.Seen(
      version,
      key,
      title,
      o.get("unit").flatMap(readRef),
      // A card seen before its kind was recorded reads as a file's.
      UnitKind.of(o.get("kind").flatMap(_.strOpt)),
      label
    )

  private def lead(k: TopicKey, v: DocumentVersionId): ujson.Value =
    ujson.Obj("key" -> TopicKey.value(k), "version" -> DocumentVersionId.value(v).toDouble)

  private def readLead(v: ujson.Value): Either[String, (TopicKey, DocumentVersionId)] =
    for {
      o <- v.objOpt.toRight("a lead is an object")
      key <- topicKey(o.get("key"))
      version <- o.get("version").toRight("a lead with no version").flatMap(version)
    } yield (key, version)

  private def topicKey(v: Option[ujson.Value]): Either[String, TopicKey] =
    v.flatMap(_.strOpt)
      .flatMap(t => DocKey.of(t).toOption)
      .flatMap(TopicKey.read)
      .toRight(s"no topic's key: ${v.fold("none")(ujson.write(_))}")

  private def version(v: ujson.Value): Either[String, DocumentVersionId] =
    v.numOpt
      .filter(_.isWhole)
      .flatMap(n => DocumentVersionId.of(n.toLong))
      .toRight(s"no document's version: ${ujson.write(v)}")

  private def count(v: Option[ujson.Value], key: String): Either[String, Int] =
    v.flatMap(_.numOpt)
      .filter(_.isWhole)
      .map(_.toInt)
      .toRight(s"a topics scan holds no count '$key'")

  private def list[A](v: Option[ujson.Value], what: String)(
      f: ujson.Value => Either[String, A]
  ): Either[String, Vector[A]] =
    v.flatMap(_.arrOpt)
      .toRight(s"a topics scan holds no $what")
      .flatMap(
        _.toVector.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, a) =>
          acc.flatMap(done => f(a).map(done :+ _))
        )
      )
}
