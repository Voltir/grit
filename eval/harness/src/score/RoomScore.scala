package grit.eval.harness.score

import grit.eval.Room
import grit.eval.harness.stats.Proportion

/** What placing one of a room's openings made, by the room's message ids: its `made`, and the
  * exchanges it was offered, each by its root's opening, why (`recent`, else lexical) and the
  * probability given it (`None`: the answer did not read, or nothing was asked).
  */
final case class RoomPlacing(id: String, made: RoomPlacing.Made, offered: Vector[RoomPlacing.Offer])

object RoomPlacing {

  /** Where an opening went. */
  enum Made {

    /** Nothing was offered, so nothing was asked: it begins. */
    case Unoffered

    /** It begins something new; `p` is what the likeliest exchange was given. */
    case Begins(p: Double)

    /** It follows the exchange `root` began, given `p`. */
    case Follows(root: String, p: Double)

    /** The classifier failed or its answer did not read, for `why`. */
    case Unread(why: String)

    /** It was not placed: the run stopped at its spend cap first. */
    case Skipped
  }

  final case class Offer(root: String, recent: Boolean, p: Option[Double])
}

/** A room's placings set against its truth ([[Room.truth]]), each proportion clustered by the
  * opening's topic (chatter its own cluster). An opening is `right` when it should begin and
  * began or was offered nothing, or should follow and followed an exchange whose root is of its
  * topic. Skipped openings are counted in `skipped` and nowhere else; ambiguous ones in
  * `ambiguous`, and in the strands they chain into.
  *
  * @param recall
  *   of the openings that should follow, those offered an exchange whose root is of their topic
  * @param strict
  *   of those, the ones offered their topic's first opening itself
  * @param chosen
  *   of the openings offered an exchange of their topic, those that followed one
  * @param confusion
  *   each (whether it should follow, what it got) pair's count, those that should follow first
  * @param wrongFollows
  *   openings that followed an exchange whose root is not of their topic, chatter's included
  * @param downstream
  *   of the wrong follows, those whose strand, as placings chained, an ambiguous opening had
  *   joined (or began) before it; none placing against the truth's strands
  * @param ambiguous
  *   each ambiguous opening placed, with what it got, in the order placed: scored apart, in no
  *   proportion, confusion, wrong follow or burst
  * @param bursts
  *   each opening in a burst ([[Room.Burst]]), in the order said
  * @param split
  *   each topic of two openings or more whose openings ended in more than one strand, with how
  *   many
  * @param mixed
  *   each strand of `strands` holding openings of more than one topic, by its root, with them
  * @param got
  *   each placing's opening, what it got, and whether that is right, in the order placed
  */
final case class RoomScore(
    openings: Int,
    skipped: Int,
    right: Proportion,
    recall: Proportion,
    strict: Proportion,
    chosen: Proportion,
    confusion: Vector[((Boolean, RoomScore.Got), Int)],
    wrongFollows: Vector[String],
    downstream: Vector[String],
    ambiguous: Vector[(String, RoomScore.Got)],
    bursts: Vector[RoomScore.Burst],
    split: Vector[(String, Int)],
    mixed: Vector[(String, Vector[String])],
    got: Vector[(String, RoomScore.Got, Boolean)]
)

object RoomScore {

  /** What one placed opening got, against its truth. */
  enum Got(val label: String) {

    /** It followed an exchange whose root is of its topic. */
    case FollowsRight extends Got("follows its topic")

    /** It followed an exchange whose root is of another topic or chatter, or it is chatter. */
    case FollowsWrong extends Got("follows another")

    case Begins extends Got("begins")
    case Unoffered extends Got("offered nothing")
    case Unread extends Got("unread")
    case Skipped extends Got("skipped")
  }

  /** An opening `id` in a burst after `previous`: whether it `continues` (the rule "follow the
    * previous" is right), whether its placing was `right`, and whether it followed the strand
    * `previous` ended in (`agrees` with the rule).
    */
  final case class Burst(
      id: String,
      previous: String,
      gapSeconds: Long,
      continues: Boolean,
      right: Boolean,
      agrees: Boolean
  )

  /** `placings` of `room`'s openings, in the order placed `against` the strands named, against
    * its truth; `strands`, each strand of two openings or more by its openings' ids, its root
    * first, as they stood after the last placing. The strand an opening ended in, which a
    * burst's agreement reads, is its placings' chain under [[Against.Placings]], its truth's
    * under [[Against.Truth]]. A placing of no opening of `room` is ignored.
    */
  def of(
      room: Room,
      placings: Vector[RoomPlacing],
      strands: Vector[Vector[String]],
      against: Against
  ): RoomScore = {
    val truths = Room.truth(room).map(t => t.opening.id -> t).toMap
    val topic: Map[String, Option[String]] = room.openings.map(m => m.id -> m.topic).toMap
    def ofTopic(root: String, t: Option[String]): Boolean =
      t.isDefined && topic.get(root).flatten == t
    val scored = placings.flatMap(p => truths.get(p.id).map(p -> _))
    val placed = scored.filterNot((p, _) => p.made == RoomPlacing.Made.Skipped)
    def cluster(t: Room.Truth): String = t.opening.topic.getOrElse(s"chatter:${t.opening.id}")

    def made(p: RoomPlacing, t: Room.Truth): Got = p.made match {
      case RoomPlacing.Made.Follows(root, _) =>
        if (ofTopic(root, t.opening.topic)) Got.FollowsRight else Got.FollowsWrong
      case RoomPlacing.Made.Begins(_) => Got.Begins
      case RoomPlacing.Made.Unoffered => Got.Unoffered
      case RoomPlacing.Made.Unread(_) => Got.Unread
      case RoomPlacing.Made.Skipped => Got.Skipped
    }
    def right(p: RoomPlacing, t: Room.Truth): Boolean = (t.follows, made(p, t)) match {
      case (None, Got.Begins | Got.Unoffered) => true
      case (Some(_), Got.FollowsRight) => true
      case _ => false
    }

    // Ambiguous openings are scored apart; they still chain into the strands others join.
    val judged = placed.filterNot((_, t) => t.opening.ambiguous)
    val follows = judged.filter((_, t) => t.follows.isDefined)
    val offered = follows.filter((p, t) => p.offered.exists(o => ofTopic(o.root, t.opening.topic)))

    // The strand each follower ended in, by its root: as placings chained, or as the truth has it.
    val rootOf: Map[String, String] = against match {
      case Against.Placings =>
        placed.foldLeft(Map.empty[String, String]) { case (roots, (p, _)) =>
          p.made match {
            case RoomPlacing.Made.Follows(root, _) =>
              roots.updated(p.id, roots.getOrElse(root, root))
            case _ => roots
          }
        }
      case Against.Truth => truths.toVector.flatMap((id, t) => t.follows.map(id -> _)).toMap
    }
    val bursts = judged.flatMap((p, t) =>
      t.burst.map { b =>
        val strand = rootOf.getOrElse(b.previous, b.previous)
        Burst(
          p.id,
          b.previous,
          b.gap.toSeconds,
          b.continues,
          right(p, t),
          p.made match {
            case RoomPlacing.Made.Follows(root, _) => rootOf.getOrElse(root, root) == strand
            case _ => false
          }
        )
      }
    )

    // As placings chained: each strand's root, and whether an ambiguous opening is in it yet.
    val downstream = against match {
      case Against.Truth => Vector.empty
      case Against.Placings =>
        placed
          .foldLeft((Map.empty[String, String], Set.empty[String], Vector.empty[String])) {
            case ((roots, unsure, found), (p, t)) =>
              val strand = p.made match {
                case RoomPlacing.Made.Follows(root, _) => roots.getOrElse(root, root)
                case _ => p.id
              }
              val wrong = !t.opening.ambiguous && made(p, t) == Got.FollowsWrong
              (
                p.made match {
                  case RoomPlacing.Made.Follows(_, _) => roots.updated(p.id, strand)
                  case _ => roots
                },
                if (t.opening.ambiguous) unsure + strand else unsure,
                if (wrong && unsure.contains(strand)) found :+ p.id else found
              )
          }
          ._3
    }

    val strandOf: Map[String, String] =
      strands.flatMap(s => s.headOption.toVector.flatMap(root => s.map(_ -> root))).toMap
    val byTopic = room.openings.flatMap(m => m.topic.map(_ -> m.id)).groupMap(_._1)(_._2)
    val split = byTopic.toVector
      .map((t, ids) => t -> ids.map(id => strandOf.getOrElse(id, id)).distinct.size)
      .filter(_._2 > 1)
      .sortBy(_._1)
    val mixed = strands.flatMap(s =>
      s.headOption.flatMap { root =>
        val topics = s.flatMap(id => topic.get(id).flatten).distinct
        Option.when(topics.size > 1)(root -> topics)
      }
    )

    RoomScore(
      scored.size,
      scored.size - placed.size,
      Proportion.of(judged.map((p, t) => cluster(t) -> right(p, t))),
      Proportion.of(follows.map((p, t) => cluster(t) -> offered.exists(_._1 == p))),
      Proportion.of(
        follows.map((p, t) => cluster(t) -> t.follows.exists(r => p.offered.exists(_.root == r)))
      ),
      Proportion.of(offered.map((p, t) => cluster(t) -> (made(p, t) == Got.FollowsRight))),
      judged
        .groupMapReduce((p, t) => (t.follows.isDefined, made(p, t)))(_ => 1)(_ + _)
        .toVector
        .sortBy((k, _) => (!k._1, k._2.ordinal)),
      judged.collect { case (p, t) if made(p, t) == Got.FollowsWrong => p.id },
      downstream,
      placed.collect { case (p, t) if t.opening.ambiguous => p.id -> made(p, t) },
      bursts,
      split,
      mixed,
      scored.map((p, t) => (p.id, made(p, t), right(p, t)))
    )
  }
}
