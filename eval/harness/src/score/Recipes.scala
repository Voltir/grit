package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.id.WorkflowId
import grit.core.message.Tokens
import grit.core.tool.ToolName
import grit.eval.harness.corpus.{Called, Part, Parts, TurnCase}
import grit.eval.harness.label.Locator
import grit.eval.harness.stats.Proportion

/** What one turn is given under a variant: the tools it is offered, what their definitions
  * cost as a request is costed, and its window by part (`None` when it was not rebuilt).
  */
final case class Given(tools: Vector[ToolName], schema: Tokens, window: Option[Parts])

/** A recorded turn, and what it is given as shipped and under a variant. */
final case class TurnPair(turn: TurnCase, shipped: Given, variant: Given)

/** A mean per turn, as shipped and under a variant. */
final case class Shift(shipped: Double, variant: Double)

/** A variant against shipped over `n` paired turns, each rate a [[Proportion]] clustered by
  * conversation (a thread).
  *
  * @param tools
  *   tools offered per turn; `None` with no turn
  * @param schema
  *   tool definitions' tokens per turn; `None` with no turn
  * @param saved
  *   tool definitions' tokens saved over every turn, shipped's less the variant's: below 0
  *   where the variant adds
  * @param called
  *   called-tool recall: of each turn's tools it called (each once, the turn's `topic` tool
  *   and unnamed calls apart), those the variant offers
  * @param windows
  *   the turns whose window was rebuilt both ways, which `parts`, `window` and both used
  *   recalls are over
  * @param parts
  *   each part kind's window tokens per turn (0 in a turn that shows none of it), every kind
  *   shown either way, in [[Part.Kind]]'s order
  * @param window
  *   the window's tokens per turn, gap lines and the turn's own messages with its parts;
  *   `None` with no window rebuilt both ways
  * @param used
  *   used-section recall: of the parts of each recorded window the turn's reply used
  *   ([[grit.eval.harness.corpus.Support.Used]]), those the variant's window holds
  *   ([[Locator.held]]); over no part where no reply used one, its rate then undefined
  * @param usedShipped
  *   the same of shipped's rebuilt window, which drift alone moves from the recorded
  * @param changed
  *   the turns given anything else under the variant, in the order paired
  */
final case class Recipe(
    n: Int,
    tools: Option[Shift],
    schema: Option[Shift],
    saved: Long,
    called: Proportion,
    windows: Int,
    parts: VectorMap[Part.Kind, Shift],
    window: Option[Shift],
    used: Proportion,
    usedShipped: Proportion,
    changed: Vector[WorkflowId]
)

object Recipe {

  /** `pairs` read as one variant against shipped. */
  def of(pairs: Vector[TurnPair]): Recipe = {
    def mean(xs: Vector[Double]) = xs.sum / xs.size
    def shift(f: Given => Double)(ps: Vector[TurnPair]) =
      Option.when(ps.nonEmpty)(
        Shift(mean(ps.map(p => f(p.shipped))), mean(ps.map(p => f(p.variant))))
      )
    val called = pairs.flatMap { p =>
      p.turn.rounds
        .flatMap(_.calls)
        .collect { case grit.eval.harness.corpus.Call(Called.Tool(name), _) => name }
        .distinct
        .map(name => p.turn.conversation -> p.variant.tools.contains(name))
    }
    val both = pairs.flatMap(p =>
      for {
        s <- p.shipped.window
        v <- p.variant.window
      } yield (p.turn, s, v)
    )
    val kinds =
      Part.Kind.values.toVector.filter(k =>
        both.exists((_, s, v) => (s.parts ++ v.parts).exists(_.kind == k))
      )
    def tokens(w: Parts, k: Part.Kind) =
      Tokens.value(w.parts.filter(_.kind == k).foldLeft(Tokens.Zero)(_ + _.tokens)).toDouble
    def whole(w: Parts) =
      Tokens.value(w.parts.foldLeft(w.gaps + w.own)(_ + _.tokens)).toDouble
    def used(pick: ((TurnCase, Parts, Parts)) => Parts) = Proportion.of(both.flatMap { b =>
      val (turn, _, _) = b
      turn.window.toVector
        .flatMap(_.parts)
        .filter(_.support.exists(_.used))
        .flatMap(Locator.of)
        .map(l => turn.conversation -> Locator.held(l, pick(b).parts))
    })
    Recipe(
      pairs.size,
      shift(_.tools.size.toDouble)(pairs),
      shift(g => Tokens.value(g.schema).toDouble)(pairs),
      pairs.map(p => Tokens.value(p.shipped.schema) - Tokens.value(p.variant.schema)).sum,
      Proportion.of(called),
      both.size,
      VectorMap.from(
        kinds.map(k =>
          k -> Shift(
            mean(both.map((_, s, _) => tokens(s, k))),
            mean(both.map((_, _, v) => tokens(v, k)))
          )
        )
      ),
      Option.when(both.nonEmpty)(
        Shift(mean(both.map((_, s, _) => whole(s))), mean(both.map((_, _, v) => whole(v))))
      ),
      used(_._3),
      used(_._2),
      pairs.filter(p => p.shipped != p.variant).map(_.turn.workflow)
    )
  }
}
