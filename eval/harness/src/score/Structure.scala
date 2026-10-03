package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.id.WorkflowId
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.prompt.Layer
import grit.core.review.{Reason, Verdict}
import grit.core.tool.ToolName
import grit.eval.harness.corpus.{Called, Drafted, Ended, Part, Said, Settled, Support, TurnCase}
import grit.eval.harness.label.Verdicts
import grit.eval.harness.stats.Proportion
import grit.turn.{TurnOffer, TurnRecord}

/** Where a turn's message was said. */
enum Where {
  case Slack, Tui, Task
}

object Where {
  def of(s: Said): Where = s match {
    case Said.Slack(_) => Slack
    case Said.Tui(_) => Tui
    case Said.Task(_) => Task
  }
}

/** Which of a corpus's turns a [[Structure]] reads: every one, those of one root, or those
  * said in one place.
  */
enum Slice {
  case All
  case Rooted(root: TurnOffer.Root)
  case At(where: Where)

  def holds(t: TurnCase): Boolean = this match {
    case All => true
    case Rooted(r) => t.root == r
    case At(w) => Where.of(t.said) == w
  }
}

object Slice {

  /** Every turn, then each root (addressed, heard), then each place (Slack, TUI, task). */
  val every: Vector[Slice] =
    All +: (Vector(TurnOffer.Root.Addressed, TurnOffer.Root.Heard).map(Rooted(_)) ++
      Where.values.toVector.map(At(_)))
}

/** `n` values' mean, and their median and 90th percentile, each the nearest rank (the
  * ⌈p·n⌉-th smallest).
  */
final case class Quantiles(n: Int, mean: Double, p50: Double, p90: Double)

object Quantiles {

  /** `None` with no value. */
  def of(values: Vector[Double]): Option[Quantiles] =
    Option.when(values.nonEmpty) {
      val sorted = values.sorted
      def rank(p: Double): Double =
        sorted.lift(math.max(0, math.ceil(p * sorted.size).toInt - 1)).getOrElse(0.0)
      Quantiles(values.size, values.sum / values.size, rank(0.5), rank(0.9))
    }
}

/** A turn that did not reply: failed at a step of family `step`, of a kind; or not finished,
  * DBOS's `status` for it.
  */
enum Unreplied {
  case Failed(step: String, why: Ended.Why)
  case Unfinished(status: String)
}

/** How a set of calls settled, by kind. */
final case class Settlings(ok: Int, failed: Int, expired: Int, abandoned: Int, unsettled: Int)

/** Calls to one tool: how many were made, in how many turns, and how they settled. */
final case class Calls(times: Int, turns: Int, settled: Settlings)

/** A tool of the turns' offered sets: how many turns offered it, and their calls to it. */
final case class ToolUse(offeredIn: Int, calls: Calls)

/** A window's tokens per turn, over the turns that recorded one: each part kind's (0 in a
  * turn that showed none of it), the gap lines', the turn's own messages', and the whole's.
  */
final case class Window(
    parts: VectorMap[Part.Kind, Quantiles],
    gaps: Quantiles,
    own: Quantiles,
    total: Quantiles
)

/** The first call to the main model (a tool loop's first round, else the reply), over the
  * turns that made one: grit's estimate of its input, the input the ledger recorded, and the
  * estimate over the recorded, over those whose recorded input is above 0.
  */
final case class Estimated(estimated: Quantiles, actual: Quantiles, ratio: Option[Quantiles])

/** What a model call was paid for, a tool loop's rounds as one. */
enum Paid {
  case Query, Topic, Rounds, Reply, Judge, Summary

  /** A ledger row that names no call of its turn. */
  case Unknown
}

object Paid {
  def of(role: Option[TurnRecord.Role]): Paid = role match {
    case Some(TurnRecord.Role.Query) => Query
    case Some(TurnRecord.Role.Topic) => Topic
    case Some(TurnRecord.Role.Round(_)) => Rounds
    case Some(TurnRecord.Role.Reply) => Reply
    case Some(TurnRecord.Role.Judge) => Judge
    case Some(TurnRecord.Role.Summary) => Summary
    case None => Unknown
  }
}

/** What became of heard roots' drafts, over the `n` turns that recorded an outcome: each
  * outcome's count; the rate posted; the rate held (an outcome neither passed nor posted,
  * each such outcome a reason held), each clustered by thread; and the judge's scores where
  * it judged.
  */
final case class Online(
    n: Int,
    outcomes: VectorMap[Drafted.Kind, Int],
    posted: Proportion,
    held: Proportion,
    grounded: Option[Quantiles],
    worth: Option[Quantiles]
)

/** The window parts a reply carries, over the `turns` with a supported reply (one that
  * replied, said something and recorded a window): their `parts`, how many of those are used
  * (support at or over [[Support.Used]]), how many of the turns used at least one, and each
  * turn's most supported part's support.
  */
final case class Used(turns: Int, parts: Int, used: Int, using: Int, top: Quantiles)

/** A slice of a corpus's recorded turns, structurally: what it was offered and called, what
  * its windows held, what it cost and how it ended; and for heard roots, what became of their
  * drafts and the review verdicts standing on them.
  *
  * @param passed
  *   the replies that said nothing, of the turns that replied, clustered by thread
  * @param rounds
  *   each turn's rounds of its tool loop, 0 for one with none; `None` with no turn
  * @param loops
  *   the turns with a tool loop
  * @param offered
  *   the size of each recorded offer's tool set; `None` with no offer
  * @param schema
  *   each recorded offer's tool definitions' tokens
  * @param tools
  *   each tool offered or called by name, in the order first offered, then first called
  * @param topic
  *   calls to the turn's own `topic` tool
  * @param unnamed
  *   calls naming neither an offered tool nor the `topic` tool
  * @param prompt
  *   each system prompt layer's tokens, in layer order, over the turns with an offer, 0 in an
  *   offer without it; only the layers some offer has
  * @param cost
  *   each turn's spend in USD by what it paid for, 0 in a turn with none of it; only what some
  *   turn paid for, in [[Paid]]'s order. A ledger row with no cost adds nothing; `unpriced`
  *   counts them
  * @param total
  *   each turn's spend in USD
  * @param verdicts
  *   the verdicts standing on a Slack turn's message, by why it was picked and the verdict
  * @param used
  *   `None` when no turn has a supported reply, where whether a part was used is undefined
  */
final case class Structure(
    n: Int,
    unreplied: VectorMap[Unreplied, Int],
    passed: Proportion,
    rounds: Option[Quantiles],
    loops: Int,
    offered: Option[Quantiles],
    schema: Option[Quantiles],
    tools: VectorMap[ToolName, ToolUse],
    topic: Calls,
    unnamed: Calls,
    prompt: VectorMap[Layer, Quantiles],
    window: Option[Window],
    estimate: Option[Estimated],
    cost: VectorMap[Paid, Quantiles],
    total: Option[Quantiles],
    unpriced: Int,
    online: Option[Online],
    verdicts: VectorMap[(Reason, Verdict), Int],
    used: Option[Used]
)

object Structure {

  /** The turns `slice` holds of `turns`, read beside the `verdicts` standing. */
  def of(turns: Vector[TurnCase], slice: Slice, verdicts: Verdicts): Structure = {
    val ts = turns.filter(slice.holds)
    val offers = ts.flatMap(_.offered)
    val calls = ts.map(t => t -> t.rounds.flatMap(_.calls))
    def callsOf(p: Called => Boolean): Calls = {
      val made = calls.map((t, cs) => t -> cs.filter(c => p(c.tool)))
      Calls(made.map(_._2.size).sum, made.count(_._2.nonEmpty), settlings(made.flatMap(_._2)))
    }
    val named = (offers.flatMap(_.tools) ++ calls.flatMap(_._2.map(_.tool)).collect {
      case Called.Tool(n) => n
    }).distinct
    val windows = ts.flatMap(_.window)
    val replied = ts.flatMap(t =>
      t.ended match {
        case Ended.Replied(_, passed) => Some(t -> passed)
        case Ended.Failed(_, _) | Ended.Unfinished(_) => None
      }
    )
    val supported = ts.flatMap(_.window.map(_.parts.flatMap(_.support)).filter(_.nonEmpty))
    Structure(
      ts.size,
      count(
        ts.flatMap(t =>
          t.ended match {
            case Ended.Failed(step, why) => Some(Unreplied.Failed(step, why))
            case Ended.Unfinished(status) => Some(Unreplied.Unfinished(status))
            case Ended.Replied(_, _) => None
          }
        )
      ),
      Proportion.of(replied.map((t, hit) => t.conversation -> hit)),
      Quantiles.of(ts.map(_.rounds.size.toDouble)),
      ts.count(_.rounds.nonEmpty),
      Quantiles.of(offers.map(_.tools.size.toDouble)),
      Quantiles.of(offers.map(o => Tokens.value(o.schema).toDouble)),
      VectorMap.from(
        named.map(n =>
          n -> ToolUse(offers.count(_.tools.contains(n)), callsOf(_ == Called.Tool(n)))
        )
      ),
      callsOf(_ == Called.Topic),
      callsOf(_ == Called.Unnamed),
      VectorMap.from(
        Layer.values.toVector
          .filter(l => offers.exists(_.prompt.contains(l)))
          .flatMap(l =>
            Quantiles
              .of(offers.map(o => o.prompt.get(l).fold(0.0)(t => Tokens.value(t).toDouble)))
              .map(l -> _)
          )
      ),
      window(windows),
      estimate(ts),
      VectorMap.from(
        Paid.values.toVector
          .filter(p => ts.exists(_.spend.exists(s => Paid.of(s.role) == p)))
          .flatMap(p =>
            Quantiles.of(ts.map(t => usd(t.spend.filter(s => Paid.of(s.role) == p)))).map(p -> _)
          )
      ),
      Quantiles.of(ts.map(t => usd(t.spend))),
      ts.flatMap(_.spend).count(_.usage.costUsd.isEmpty),
      online(ts),
      VectorMap.from(
        count(
          ts.flatMap(t =>
            t.said match {
              case Said.Slack(at) => verdicts.cases.get(at).map(r => (r.reason, r.verdict))
              case Said.Tui(_) | Said.Task(_) => None
            }
          )
        ).toVector.sortBy((k, _) => (k._1.ordinal, k._2.ordinal))
      ),
      Quantiles
        .of(supported.flatMap(_.map(_.value).maxOption))
        .map(top =>
          Used(
            supported.size,
            supported.map(_.size).sum,
            supported.map(_.count(_.used)).sum,
            supported.count(_.exists(_.used)),
            top
          )
        )
    )
  }

  /** `turns` by what each spent in USD, most first, a tie by workflow id; a ledger row with
    * no cost adds nothing.
    */
  def byCost(turns: Vector[TurnCase]): Vector[(TurnCase, BigDecimal)] =
    turns
      .map(t => t -> t.spend.flatMap(_.usage.costUsd).sum)
      .sortBy((t, c) => (-c, WorkflowId.value(t.workflow)))

  /** How many of `verdicts`' cases no turn of `turns` answers. */
  def unjoined(turns: Vector[TurnCase], verdicts: Verdicts): Int = {
    val said = turns
      .flatMap(t =>
        t.said match {
          case Said.Slack(at) => Some(at)
          case Said.Tui(_) | Said.Task(_) => None
        }
      )
      .toSet
    verdicts.cases.keySet.count(!said.contains(_))
  }

  private def usd(spend: Vector[grit.eval.harness.corpus.Spent]): Double =
    spend.flatMap(_.usage.costUsd).sum.toDouble

  private def count[K](keys: Vector[K]): VectorMap[K, Int] =
    VectorMap.from(keys.distinct.map(k => k -> keys.count(_ == k)))

  private def settlings(calls: Vector[grit.eval.harness.corpus.Call]): Settlings = {
    def n(p: Settled => Boolean) = calls.count(c => p(c.settled))
    Settlings(
      n { case Settled.Ok(_) => true; case _ => false },
      n { case Settled.Failed(_) => true; case _ => false },
      n(_ == Settled.Expired),
      n(_ == Settled.Abandoned),
      n(_ == Settled.Unsettled)
    )
  }

  private def window(windows: Vector[grit.eval.harness.corpus.Parts]): Option[Window] = {
    def tokens(ps: Vector[Part]) = ps.map(p => Tokens.value(p.tokens)).sum.toDouble
    for {
      gaps <- Quantiles.of(windows.map(w => Tokens.value(w.gaps).toDouble))
      own <- Quantiles.of(windows.map(w => Tokens.value(w.own).toDouble))
      total <- Quantiles.of(
        windows.map(w => tokens(w.parts) + Tokens.value(w.gaps) + Tokens.value(w.own))
      )
    } yield Window(
      VectorMap.from(
        Part.Kind.values.toVector
          .filter(k => windows.exists(_.parts.exists(_.kind == k)))
          .flatMap(k =>
            Quantiles.of(windows.map(w => tokens(w.parts.filter(_.kind == k)))).map(k -> _)
          )
      ),
      gaps,
      own,
      total
    )
  }

  private def estimate(ts: Vector[TurnCase]): Option[Estimated] = {
    val first = ts.flatMap(
      _.spend.find(s =>
        s.role match {
          case Some(TurnRecord.Role.Round(_)) | Some(TurnRecord.Role.Reply) => true
          case _ => false
        }
      )
    )
    val pairs =
      first.map(s => (Tokens.value(s.estimated).toDouble, Tokens.value(s.usage.input).toDouble))
    for {
      estimated <- Quantiles.of(pairs.map(_._1))
      actual <- Quantiles.of(pairs.map(_._2))
    } yield Estimated(
      estimated,
      actual,
      Quantiles.of(pairs.collect { case (e, a) if a > 0 => e / a })
    )
  }

  private def online(ts: Vector[TurnCase]): Option[Online] = {
    val drafted = ts.flatMap(t => t.speech.map(t -> _))
    Option.when(drafted.nonEmpty) {
      val kinds = drafted.map(_._2.outcome)
      def held(k: Drafted.Kind) = k != Drafted.Kind.Passed && k != Drafted.Kind.Posted
      def scores(f: Drafted => Option[Probability]) =
        Quantiles.of(drafted.flatMap((_, d) => f(d).map(Probability.value)))
      Online(
        drafted.size,
        VectorMap.from(
          Drafted.Kind.values.toVector.filter(kinds.contains).map(k => k -> kinds.count(_ == k))
        ),
        Proportion.of(drafted.map((t, d) => t.conversation -> (d.outcome == Drafted.Kind.Posted))),
        Proportion.of(drafted.map((t, d) => t.conversation -> held(d.outcome))),
        scores(_.grounded),
        scores(_.worth)
      )
    }
  }
}
