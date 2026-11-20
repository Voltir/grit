package grit.knowledge.topic

import java.time.LocalDate

import scala.concurrent.duration.*

import grit.core.stitch.Strand

/** What a topics run is told: at most how many topics it writes (`perRun`); how far back a
  * strand's last message (`recent`) and a card's write (`cardRecent`) may be for it to be looked
  * at; how long one set aside stays aside before it is offered again (`asideFor`); how long a
  * passage is kept after its day (`keepFor`); at most how many topics of one discussion it
  * sorts against the others to merge (`merges`), so at most how many it folds; the least
  * probability, from [[TopicDream.FoldAt]] (0.90) to 1, at which it folds one it proposes
  * (`foldAt`), none folded but only proposed when there is none; how it cuts a channel's top level into runs
  * (`cut`); at most how many strands it sorts (`sorts`); the `day` it writes for, the day of
  * its slot in its job's zone when none: a backfill names a past day, and its strands are read
  * as they stood at that day's end; and the last day it seeds on (`seedUntil`): a run for that
  * day or one before reads the cards written within [[TopicParams.SeedReach]] instead of
  * `cardRecent`, so each night offers the next batch of the cards no run has placed.
  */
final case class TopicParams(
    perRun: Int,
    recent: FiniteDuration,
    cardRecent: FiniteDuration,
    day: Option[LocalDate],
    seedUntil: Option[LocalDate],
    asideFor: FiniteDuration = TopicParams.AsideFor,
    keepFor: FiniteDuration = TopicParams.KeepFor,
    merges: Int = TopicParams.Merges,
    cut: Strand.Cut = Strand.Cut.Default,
    sorts: Int = TopicParams.Sorts,
    foldAt: Option[Double] = None
) extends caps.Pure

object TopicParams {

  /** 3,650 days: how far back a seeding run reads cards. */
  val SeedReach: FiniteDuration = 3650.days

  /** 7 days: how long a strand or card set aside stays aside, unless a schedule says. */
  val AsideFor: FiniteDuration = 7.days

  /** 90 days: how long a passage is kept after its day, unless a schedule says. */
  val KeepFor: FiniteDuration = 90.days

  /** 8: the most topics a run sorts against the others to merge; a schedule may lower it. */
  val Merges: Int = 8

  /** `p` as a run's [[TopicParams.foldAt]], or why not: `p` not from [[TopicDream.FoldAt]] to 1. */
  def folding(p: Double): Either[String, Double] =
    if (p >= TopicDream.FoldAt && p <= 1) Right(p) else Left(foldAtIs(p.toString))

  // Draft wording, for Nick.
  private def foldAtIs(p: String): String =
    s"foldAt, a number from ${TopicDream.FoldAt} to 1, not $p"

  /** 32: the most strands a run sorts; a schedule may lower it. */
  val Sorts: Int = 32

  /** Its record: `{"perRun", "recent", "cardRecent", "asideFor", "keepFor", "merges", "cut",
    * "sorts", "day"?, "seedUntil"?, "foldAt"?}`, the cut as [[Strand.Cut.write]] writes it, durations
    * ISO-8601, the days `yyyy-mm-dd`.
    */
  def write(p: TopicParams): ujson.Value = {
    def iso(d: FiniteDuration): String = java.time.Duration.ofSeconds(d.toSeconds).toString
    val o = ujson.Obj(
      "perRun" -> p.perRun,
      "recent" -> iso(p.recent),
      "cardRecent" -> iso(p.cardRecent),
      "asideFor" -> iso(p.asideFor),
      "keepFor" -> iso(p.keepFor),
      "merges" -> p.merges,
      "cut" -> Strand.Cut.write(p.cut),
      "sorts" -> p.sorts
    )
    p.day.foreach(d => o("day") = d.toString)
    p.seedUntil.foreach(d => o("seedUntil") = d.toString)
    p.foldAt.foreach(f => o("foldAt") = f)
    o
  }

  /** The parameters [[write]] wrote, `asideFor` [[AsideFor]], `keepFor` [[KeepFor]],
    * `merges` [[Merges]], `cut` [[Strand.Cut.Default]] and `sorts` [[Sorts]] when absent (kept
    * before they were), or why `v` holds none: `perRun` not a whole number at least 1, `merges`
    * not one from 0 to [[Merges]], `sorts` not one from 1 to [[Sorts]], `foldAt` (none when
    * absent) not a number from [[TopicDream.FoldAt]] to 1, no cut, `recent` under a
    * day and the cut's settle, a duration not positive ISO-8601, or a day not `yyyy-mm-dd`.
    */
  def read(v: ujson.Value): Either[String, TopicParams] = {
    val shown = ujson.write(v).take(100)
    def duration(o: collection.Map[String, ujson.Value], key: String) =
      o.get(key)
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(java.time.Duration.parse(t)).toOption)
        .filter(d => !d.isNegative && !d.isZero)
        .map(_.getSeconds.seconds)
        .toRight(s"a topics run's parameters hold $key, a positive ISO-8601 duration: $shown")
    for {
      o <- v.objOpt.toRight(s"a topics run's parameters are an object: $shown")
      perRun <- o
        .get("perRun")
        .flatMap(_.numOpt)
        .filter(n => n.isWhole && n >= 1 && n <= Int.MaxValue)
        .toRight(s"a topics run's parameters hold perRun, a whole number at least 1: $shown")
      recent <- duration(o, "recent")
      cardRecent <- duration(o, "cardRecent")
      asideFor <- if (o.contains("asideFor")) duration(o, "asideFor") else Right(AsideFor)
      keepFor <- if (o.contains("keepFor")) duration(o, "keepFor") else Right(KeepFor)
      merges <- o.get("merges") match {
        case None => Right(Merges)
        case Some(n) =>
          n.numOpt
            .filter(n => n.isWhole && n >= 0 && n <= Merges)
            .map(_.toInt)
            .toRight(
              s"a topics run's parameters hold merges, a whole number from 0 to $Merges: $shown"
            )
      }
      day <- date(o, "day", shown)
      seedUntil <- date(o, "seedUntil", shown)
      cut <- o
        .get("cut")
        .fold[Either[String, Strand.Cut]](Right(Strand.Cut.Default))(
          Strand.Cut.read(_).left.map(why => s"a topics run's parameters hold no cut: $why")
        )
      sorts <- o.get("sorts") match {
        case None => Right(Sorts)
        case Some(n) =>
          n.numOpt
            .filter(n => n.isWhole && n >= 1 && n <= Sorts)
            .map(_.toInt)
            .toRight(
              s"a topics run's parameters hold sorts, a whole number from 1 to $Sorts: $shown"
            )
      }
      foldAt <- o.get("foldAt") match {
        case None => Right(None)
        case Some(n) =>
          n.numOpt
            .toRight(foldAtIs(n.toString))
            .flatMap(folding)
            .map(Some(_))
            .left
            .map(why => s"a topics run's parameters hold $why: $shown")
      }
      // A run settles after its window ends, so the next night's window must still reach it.
      _ <- Either.cond(
        recent >= 1.day + cut.settle.getSeconds.seconds,
        (),
        s"a topics run's parameters hold recent at least a day and the cut's settle: $shown"
      )
    } yield TopicParams(
      perRun.toInt,
      recent,
      cardRecent,
      day,
      seedUntil,
      asideFor,
      keepFor,
      merges,
      cut,
      sorts,
      foldAt
    )
  }

  private def date(
      o: collection.Map[String, ujson.Value],
      key: String,
      shown: String
  ): Either[String, Option[LocalDate]] =
    o.get(key) match {
      case None => Right(None)
      case Some(d) =>
        d.strOpt
          .flatMap(Passage.day)
          .map(Some(_))
          .toRight(s"a topics run's $key is yyyy-mm-dd: $shown")
    }
}
