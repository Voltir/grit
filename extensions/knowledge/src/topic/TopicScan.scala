package grit.knowledge.topic

import java.time.Instant

import grit.core.document.{DocRef, Document, DocumentKeeper}
import grit.core.durable.Journaled
import grit.core.id.{ConversationId, DocKey, DocumentVersion}
import grit.core.need.{Exported, Needs}
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label
import grit.knowledge.card.{CardShelf, UnitKind}
import grit.knowledge.ref.{CardKey, Ref, TopicKey}

/** A topics run's scan: by ids alone, what its asks are made of. */
private[topic] object TopicScan {

  /** A topic a run attaches to: its `key`, its current `version` (`None` for one this run
    * opens), and the `strands` and `cards` attached.
    */
  final case class Touch(
      key: TopicKey,
      version: Option[DocumentVersion],
      strands: Vector[Strand.Digested],
      cards: Vector[CardShelf.Seen]
  ) extends caps.Pure {
    def opened: Boolean = version.isEmpty
  }

  /** What a scan came to: the topics `attached` to by ref; the `strands` and `cards` the
    * attach ask places, and the topics it shows (`leads`, by key and version: those attached to,
    * then at most [[Leads]] others, newest first); how
    * many passages it `withdrew`, and how many strands and cards it found `unchanged` or `left`
    * past the run's most.
    */
  final case class Scanned(
      attached: Vector[Touch],
      strands: Vector[Strand.Digested],
      cards: Vector[CardShelf.Seen],
      leads: Vector[(TopicKey, DocumentVersion)],
      withdrew: Int,
      unchanged: Int,
      left: Int,
      linked: Vector[Linked]
  ) extends caps.Pure {

    /** The card units `strands` link, and the cards paired with them, each once. */
    def of(strands: Vector[Strand.Digested]): (Vector[Ref], Vector[CardShelf.Seen]) = {
      val roots = strands.map(_.root).toSet
      val mine = linked.filter(l => roots.contains(l.root))
      (mine.flatMap(_.units).distinct, mine.flatMap(_.cards).distinctBy(_.version))
    }
  }

  /** A strand in play that links a card's unit: its `root`, the card `units` its messages link
    * (at most [[Links]]), and the `cards` paired with it, each a card of one of those units no
    * strand before it in the run links, whatever its age or whether it was set aside: it goes
    * where the strand goes, never shown to the attach ask.
    */
  final case class Linked(root: ConversationId, units: Vector[Ref], cards: Vector[CardShelf.Seen])
      extends caps.Pure

  /** 40: the most topics the attach ask shows besides those attached by ref. */
  val Leads: Int = 40

  /** 50: the most changed cards a scan reads. */
  val Cards: Int = 50

  /** 10: the most card units a scan keeps of one strand's links. */
  val Links: Int = 10

  /** The scan at `until`: its floor's topics read, one whose data does not read withdrawn;
    * from each, the passages naming a strand `strands` no longer reads at the floor (its room at
    * another label now, or its messages gone) withdrawn, the topic itself when none is left;
    * then the strands at the floor spoken in within `recent` before `until`, and the cards
    * changed within `cardRecent` before `until`: the floor's [[Aside]] kept to those among them at
    * the digest or version set aside; each already in a topic's lineage or the aside at its
    * digest or version unchanged, each a topic refers to attached to it, and of the rest at most
    * `most` strands and `most` cards to place, oldest first, the others left. Each strand
    * attached or to place is [[Linked]] to the cards its messages link, read whatever their age
    * and taken out of the aside and of those to place.
    */
  def scanned(
      params: TopicParams,
      most: Int,
      until: Instant,
      cards: Option[Exported[CardShelf]],
      keeper: DocumentKeeper,
      strands: StrandShelf,
      needs: Needs,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Scanned] = {
    val floor = Tx.floor(tx)
    val shelf = cards.flatMap(e => needs.of(e).toOption)
    held(keeper, floor, at).flatMap((all: Vector[Found]) =>
      pruned(keeper, strands, all, floor, until, at).flatMap((kept: Vector[Pruned]) =>
        strands
          .atFloor(until.minusSeconds(params.recent.toSeconds), until)
          .flatMap((recent: Vector[Strand.Digested]) =>
            changed(shelf, until.minusSeconds(params.cardRecent.toSeconds), until)
              .flatMap((seen: Vector[CardShelf.Seen]) =>
                Aside
                  .read(keeper, floor)
                  .flatMap((held: Lineage) =>
                    placed(keeper, kept.flatMap(_.topic), held, recent, seen).flatMap((p: Placed) =>
                      linking(strands, shelf, inPlay(most, p), until).flatMap(
                        (linked: Vector[Linked]) =>
                          aside(keeper, floor, held, recent, seen, linked, at)
                            .map(_ => gathered(most, kept, recent, seen, p, linked))
                      )
                    )
                  )
              )
          )
      )
    )
  }

  /** The strands of `p` a run takes up: those attached by ref, then at most `most` to place. */
  private def inPlay(most: Int, p: Placed): Vector[Strand.Digested] =
    p.strands.collect { case (s, Some(_)) => s } ++ p.strands
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
          .map(ls => s.root -> ls.map(Ref.of).filter(isUnit).distinct.take(Links))
      ).flatMap { (units: Vector[(ConversationId, Vector[Ref])]) =>
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
      topic: Option[(TopicKey, DocumentVersion, TopicWrite.Held)],
      withdrew: Int
  )

  /** The strands and cards in no topic's lineage at their digest or version, each with the
    * topic that refers to it, if any.
    */
  private final case class Placed(
      strands: Vector[(Strand.Digested, Option[TopicKey])],
      cards: Vector[(CardShelf.Seen, Option[TopicKey])]
  )

  /** The floor's topics, one whose data does not read withdrawn; its [[Aside]] is none. */
  private def held(keeper: DocumentKeeper, floor: Label, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Found]] =
    keeper.newest(Int.MaxValue).flatMap { (documents: Vector[Document]) =>
      val read: Vector[(Document, Either[String, (TopicKey, TopicWrite.Held)])] =
        documents
          .filter(d => d.label == floor && d.key != Aside.Key)
          .map(d =>
            d -> TopicKey
              .read(d.key)
              .toRight("no topic's key")
              .flatMap(k => TopicWrite.read(k, d.data).map(k -> _))
          )
      each(read.collect { case (d, Left(_)) => d.key })(k => keeper.withdraw(k, floor, at))
        .map(_ => read.collect { case (d, Right((k, h))) => Found(k, d, h) })
    }

  /** Each of `found` with its passages naming a strand `strands` does not read at `floor` as
    * of `until` withdrawn, the topic itself when none is left.
    */
  private def pruned(
      keeper: DocumentKeeper,
      strands: StrandShelf,
      found: Vector[Found],
      floor: Label,
      until: Instant,
      at: Instant
  )(using Tx^): Either[StoreError, Vector[Pruned]] = {
    val roots = found
      .flatMap(_.held.topic.passages.flatMap(_.from.tags))
      .collect { case Lineage.Tag.Strand(root, _, _) => root }
      .distinct
    strands.of(roots, until).flatMap { (read: Vector[Strand.Digested]) =>
      val present = read.filter(_.roomLabel == floor).map(_.root).toSet
      each(found) { (f: Found) =>
        val passages = f.held.topic.passages
        val gone = passages.filter(_.from.tags.exists {
          case Lineage.Tag.Strand(root, _, _) => !present.contains(root)
          case _ => false
        })
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
    }
  }

  /** The cards changed from `since` until `until`, at most [[Cards]], through `cards`' service;
    * none when there is no such plugin or it is not among the needs.
    */
  private def changed(
      shelf: Option[CardShelf],
      since: Instant,
      until: Instant
  )(using Tx^): Either[StoreError, Vector[CardShelf.Seen]] =
    shelf.fold(Right(Vector.empty))(_.changed(since, until, Cards))

  /** The floor's aside `held`, kept to the strands of `recent` and the cards of `seen` it holds
    * at the digest or version they are now, less the cards `linked` pairs: the aside after.
    */
  private def aside(
      keeper: DocumentKeeper,
      floor: Label,
      held: Lineage,
      recent: Vector[Strand.Digested],
      seen: Vector[CardShelf.Seen],
      linked: Vector[Linked],
      at: Instant
  )(using Tx^): Either[StoreError, Lineage] = {
    val paired = linked.flatMap(_.cards.map(_.version)).toSet
    val now = Lineage.of(held.tags.filter {
      case t @ Lineage.Tag.Strand(_, _, _) => recent.exists(s => isStrand(s)(t))
      case Lineage.Tag.Card(v) => seen.exists(_.version == v) && !paired.contains(v)
      case Lineage.Tag.Topic(_) => false
    })
    if (now == held) Right(held) else Aside.kept(keeper, floor, now, at).map(_ => now)
  }

  /** Of `recent` and `seen`, those in no lineage of `topics` or `aside`, each with the topic
    * among them that refers to it.
    */
  private def placed(
      keeper: DocumentKeeper,
      topics: Vector[(TopicKey, DocumentVersion, TopicWrite.Held)],
      aside: Lineage,
      recent: Vector[Strand.Digested],
      seen: Vector[CardShelf.Seen]
  )(using Tx^): Either[StoreError, Placed] = {
    val lineage = (topics.flatMap(_._3.topic.passages.flatMap(_.from.tags)) ++ aside.tags).toSet
    val held = topics.map(_._1).toSet
    val strands = recent.filterNot(s => lineage.exists(isStrand(s)))
    val cards = seen.filterNot(c => lineage.contains(Lineage.Tag.Card(c.version)))
    each(strands)((s: Strand.Digested) => referred(keeper, Ref.Strand(s.root), held).map(s -> _))
      .flatMap((ss: Vector[(Strand.Digested, Option[TopicKey])]) =>
        each(cards)((c: CardShelf.Seen) =>
          c.unit.fold(Right(None))(u => referred(keeper, u, held)).map(c -> _)
        ).map((cs: Vector[(CardShelf.Seen, Option[TopicKey])]) => Placed(ss, cs))
      )
  }

  /** The scan's result, from what its reads found. */
  private def gathered(
      most: Int,
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
    // A directory's card describes a container, not a matter: it attaches only by ref or a link.
    val cards = p.cards.collect {
      case (c, None) if !paired.contains(c.version) && c.kind == UnitKind.File => c
    }.reverse
    val byRef = attached.flatMap(t => t.version.map(t.key -> _))
    val leads = byRef ++ topics
      .sortBy((_, v, _) => -DocumentVersion.value(v))
      .map((k, v, _) => (k, v))
      .filterNot((k, _) => byRef.exists(_._1 == k))
      .take(Leads)
    Scanned(
      attached,
      strands.take(most),
      cards.take(most),
      leads,
      kept.map(_.withdrew).sum,
      (recent.size - p.strands.size) +
        seen.count(c => !p.cards.exists(_._1.version == c.version) && !paired.contains(c.version)),
      (strands.size - most).max(0) + (cards.size - most).max(0),
      linked
    )
  }

  private def isStrand(s: Strand.Digested)(t: Lineage.Tag): Boolean = t match {
    case Lineage.Tag.Strand(root, _, digest) => root == s.root && digest == s.digest
    case _ => false
  }

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

  /** Its record: `{"attached": [...], "strands": [...], "cards": [...], "leads": [...],
    * "withdrew", "unchanged", "left", "linked": [...]}`, read as linking none when `linked` is
    * absent.
    */
  given Journaled[Scanned] = Journaled.json(
    s =>
      ujson.Obj(
        "attached" -> ujson.Arr.from(s.attached.map(touch)),
        "strands" -> ujson.Arr.from(s.strands.map(Strand.Digested.write)),
        "cards" -> ujson.Arr.from(s.cards.map(card)),
        "leads" -> ujson.Arr.from(s.leads.map((k, v) => lead(k, v))),
        "withdrew" -> s.withdrew,
        "unchanged" -> s.unchanged,
        "left" -> s.left,
        "linked" -> ujson.Arr.from(s.linked.map(linkedJson))
      ),
    v =>
      for {
        o <- v.objOpt.toRight("a topics scan is an object")
        attached <- list(o.get("attached"), "attached")(readTouch)
        strands <- list(o.get("strands"), "strands")(Strand.Digested.read)
        cards <- list(o.get("cards"), "cards")(readCard)
        leads <- list(o.get("leads"), "leads")(readLead)
        withdrew <- count(o.get("withdrew"), "withdrew")
        unchanged <- count(o.get("unchanged"), "unchanged")
        left <- count(o.get("left"), "left")
        // A scan recorded before strands were linked holds none.
        linked <- list(o.get("linked").orElse(Some(ujson.Arr())), "linked")(readLinked)
      } yield Scanned(attached, strands, cards, leads, withdrew, unchanged, left, linked)
  )

  private def linkedJson(l: Linked): ujson.Value = ujson.Obj(
    "root" -> ConversationId.value(l.root),
    "units" -> ujson.Arr.from(l.units.flatMap(u => Ref.written(u).toOption).map(refJson)),
    "cards" -> ujson.Arr.from(l.cards.map(card))
  )

  private def readLinked(v: ujson.Value): Either[String, Linked] =
    for {
      o <- v.objOpt.toRight("a linked strand is an object")
      root <- o
        .get("root")
        .flatMap(_.strOpt)
        .map(ConversationId(_))
        .toRight("a linked strand with no root")
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
    t.version.foreach(v => o("version") = DocumentVersion.value(v).toDouble)
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
      "version" -> DocumentVersion.value(c.version).toDouble,
      "key" -> CardKey.value(c.key),
      "title" -> c.title,
      "kind" -> c.kind.word
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
    } yield CardShelf.Seen(
      version,
      key,
      title,
      o.get("unit").flatMap(readRef),
      // A card seen before its kind was recorded reads as a file's.
      UnitKind.of(o.get("kind").flatMap(_.strOpt))
    )

  private def lead(k: TopicKey, v: DocumentVersion): ujson.Value =
    ujson.Obj("key" -> TopicKey.value(k), "version" -> DocumentVersion.value(v).toDouble)

  private def readLead(v: ujson.Value): Either[String, (TopicKey, DocumentVersion)] =
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

  private def version(v: ujson.Value): Either[String, DocumentVersion] =
    v.numOpt
      .filter(_.isWhole)
      .flatMap(n => DocumentVersion.of(n.toLong))
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
