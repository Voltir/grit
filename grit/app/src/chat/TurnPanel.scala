package grit.app.chat

import grit.app.look.{Look, ProseLook, Theme}
import grit.core.id.TurnSeq
import grit.core.message.{Cost, Tokens}
import grit.core.topic.{Band, Verdict}
import grit.tui.model.block.Block
import grit.tui.model.surface.Style
import grit.tui.model.text.StyledText
import grit.turn.Turn

/** The turn panel's rows. Its turn tab, for a [[TurnView]]: the steps as a timeline, then
  * the query, the recalled turns, the window as a stacked bar against `budget`, and the
  * cost. Its session tab, for a [[SessionView]]: the conversation so far. Its topics tab,
  * for a [[TopicsView]]: the topics, and how one turn's message was placed. Every row but a
  * long model name fits in [[TurnPanel.Cols]] columns, so none wraps.
  */
final case class TurnPanel(look: Look, budget: Tokens) {
  import TurnPanel.*

  private def t: Theme = look.theme
  private def fg(c: grit.tui.model.surface.Color): Style = Style.fg(c)
  private def row(parts: (String, Style)*): Block =
    Block.styled(parts.foldLeft(StyledText.empty)((acc, p) => acc ++ StyledText.styled(p(0), p(1))))
  private val blank: Block = Block.styled(StyledText(""))

  /** The rows for `view`, or a placeholder before the conversation has a turn; `runningMs`
    * is how long the running step has run, and a `pinned` turn says how to let it go.
    */
  def blocks(view: Option[TurnView], runningMs: Long, pinned: Boolean = false): Vector[Block] =
    view match {
      case None => Vector(row(" no turn yet" -> fg(t.faint)))
      case Some(v) =>
        Vector(title(v, pinned), blank) ++ steps(v, runningMs) ++ Vector(blank) ++ query(v) ++
          models(v) ++ window(v) ++ cost(v)
    }

  /** The rows of the session tab for `view`, or a placeholder before the host has read
    * the conversation: its length, what it was billed and cost, what search recalled,
    * then each role that called a model, with the models that answered it.
    */
  def session(view: Option[SessionView]): Vector[Block] =
    view match {
      case None => Vector(row(" reading the conversation…" -> fg(t.faint)))
      case Some(v) =>
        val turns = if (v.turns == 1) "1 turn" else s"${v.turns} turns"
        val messages = if (v.messages == 1) "1 message" else s"${v.messages} messages"
        val recalled =
          if (v.recalled.isEmpty) Vector(row(" recalled " -> fg(t.faint), "none" -> fg(t.faint)))
          else
            Vector(
              row(
                " recalled " -> fg(t.faint),
                v.recalled.map(s => s"turn ${number(s)}").mkString(" · ") -> fg(t.user)
              ),
              row(s"          by ${v.recalls} of $turns" -> fg(t.faint))
            )
        val billed =
          if (Tokens.value(v.input + v.output) == 0) "nothing yet"
          else s"${count(v.input)} in · ${count(v.output)} out"
        Vector(
          row(" SESSION" -> (fg(t.ink) + Style.Bold), s"  · $turns" -> fg(t.faint)),
          blank,
          row(" said     " -> fg(t.faint), messages -> fg(t.ink)),
          row(" billed   " -> fg(t.faint), billed -> fg(t.ink))
        ) ++
          v.spent.map(c => row(" spent    " -> fg(t.faint), dollars(c) -> fg(t.ink))) ++
          Vector(blank) ++ recalled ++ Vector(blank) ++ roles(v.roles)
    }

  /** The rows of the topics tab for `view`, or a placeholder before the host has read the
    * conversation: each topic (the current one marked) with its messages, then how the
    * shown turn's message was placed: p(same) and its band, the classifier's choice, the
    * model's verdict, a flag when the two disagreed, and the heaviest weights.
    */
  def topics(view: Option[TopicsView]): Vector[Block] =
    view match {
      case None => Vector(row(" reading the conversation…" -> fg(t.faint)))
      case Some(v) =>
        val count = if (v.topics.size == 1) "1 topic" else s"${v.topics.size} topics"
        val rows =
          if (v.topics.isEmpty) Vector(row(" none yet" -> fg(t.faint)))
          else
            v.topics.map { r =>
              row(
                (if (r.current) " ● " else "   ") -> fg(t.grit),
                clip(r.name, TopicCols).padTo(TopicCols, ' ') -> fg(
                  if (r.current) t.ink else t.faint
                ),
                r.messages.toString.reverse.padTo(4, ' ').reverse -> fg(t.faint)
              )
            }
        Vector(row(" TOPICS" -> (fg(t.ink) + Style.Bold), s"  · $count" -> fg(t.faint)), blank) ++
          rows ++ v.placing.toVector.flatMap(p => blank +: placing(p))
    }

  private def placing(p: TopicsView.Placing): Vector[Block] = {
    def label(l: String): (String, Style) = s" ${l.padTo(9, ' ')}" -> fg(t.faint)
    def shares(items: Vector[(String, Double)]): Vector[Block] =
      items.map((n, w) =>
        row(
          "   " -> fg(t.faint),
          clip(n, TopicCols).padTo(TopicCols, ' ') -> fg(t.ink),
          share(w).reverse.padTo(5, ' ').reverse -> fg(t.ink)
        )
      )
    val band = p.band.map {
      case Band.Same => "same"
      case Band.Uncertain => "unsure"
      case Band.Changed => "changed"
    }
    val verdict = p.verdict.map {
      case Verdict.Current => "current"
      case Verdict.Earlier(n) => s"earlier: $n"
      case Verdict.New(n) => n.fold("new")(x => s"new: $x")
      case Verdict.Unreadable(_) => "unreadable"
    }
    Vector(row(s" TURN ${number(p.turn)}" -> (fg(t.ink) + Style.Bold))) ++
      Option.when(p.first)(row(label("jev"), "first message" -> fg(t.faint))) ++
      p.unclassified.toVector.flatMap(why =>
        Vector(
          row(label("jev"), "unclassified" -> fg(t.ink)),
          row(s"   ${clip(why, Cols - 5)}" -> fg(t.faint))
        )
      ) ++
      p.pSame.map(ps =>
        row(label("p(same)"), share(ps) -> fg(t.ink), band.fold("")(b => s" · $b") -> fg(t.faint))
      ) ++
      (if (p.choice.isEmpty) Vector.empty
       else row(label("jev chose")) +: shares(p.choice.take(TopicsView.Weights))) ++
      verdict.map(v => row(label("model"), clip(v, Cols - 12) -> fg(t.ink))) ++
      p.anomaly.map(a => row(s"   ${clip(a, Cols - 5)}" -> fg(t.faint))) ++
      Option.when(p.disagree)(
        row(" ⚑ jev and the model disagree" -> (fg(t.failure) + Style.Bold))
      ) ++
      p.placed.map(n => row(label("placed"), clip(n, Cols - 12) -> fg(t.grit))) ++
      Vector(row(label("weights"))) ++ shares(p.weights) ++
      Option.when(p.elsewhere > 0.005)(
        row(
          "   " -> fg(t.faint),
          "elsewhere".padTo(TopicCols, ' ') -> fg(t.faint),
          share(p.elsewhere).reverse.padTo(5, ' ').reverse -> fg(t.faint)
        )
      )
  }

  /** One row for each role that called a model: its calls and cost, then its models. */
  private def roles(roles: Vector[SessionView.Role]): Vector[Block] =
    if (roles.isEmpty) Vector(row(" models   " -> fg(t.faint), "none called yet" -> fg(t.faint)))
    else
      roles.flatMap { r =>
        val calls = if (r.calls == 1) "1 call" else s"${r.calls} calls"
        row(
          Seq(s" ${r.name.padTo(9, ' ')}" -> fg(t.faint), calls -> fg(t.ink)) ++
            r.spent.map(c => s" · ${dollars(c)}" -> fg(t.ink))*
        ) +: r.models.map(m => row(s"   $m" -> fg(t.grit)))
      }

  /** The rows of the opened turn: what was asked, what has been heard of the reply while
    * it streams (the reasoning, then the text), then all the panel shows.
    */
  def opened(
      view: Option[TurnView],
      runningMs: Long,
      hearing: Option[ChatScreen.Hearing]
  ): Vector[Block] =
    view.toVector.flatMap { v =>
      val heard =
        hearing.filter(h => h.turn == v.turn && v.running.nonEmpty).toVector.flatMap { h =>
          Vector(row(" ── heard so far " -> fg(t.faint))) ++
            Option.when(h.reasoning.nonEmpty)(row(s" ${h.reasoning}" -> fg(t.faint))) ++
            (if (h.text.isEmpty) Vector.empty
             else ProseLook(look).streaming(h.text, StyledText.styled("▍", fg(t.grit)))) ++
            Vector(blank)
        }
      Vector(
        Block.styled(
          StyledText.styled(s" ${Look.Runes.User} ", fg(t.user) + Style.Bold) ++
            StyledText.styled(v.asked, fg(t.ink) + Style.Bold)
        ),
        blank
      ) ++ heard
    } ++ blocks(view, runningMs)

  private def title(v: TurnView, pinned: Boolean): Block = {
    val where = v.running.fold(if (v.settled) "done" else "finishing")(Look.Runes.step(_))
    row(
      Seq(s" TURN ${number(v.turn)}" -> (fg(t.ink) + Style.Bold), s"  · $where" -> fg(t.faint)) ++
        Option.when(pinned)("  esc: latest" -> fg(t.faint))*
    )
  }

  /** One row per step of the turn: recorded ones in the order they ran, with their time
    * and a bar to scale; while it runs, the running one lit, then the steps every turn
    * takes after it, dim. A step only some turns take, and each step of a tool loop, shows
    * once it is recorded or running.
    */
  private def steps(v: TurnView, runningMs: Long): Vector[Block] = {
    val longest = math.max(1L, (v.steps.flatMap(_.ms) :+ runningMs).max)
    def bar(ms: Long): Int = math.max(1, math.round(ms.toDouble / longest * BarCells).toInt)
    val recorded = v.steps.map(_.name).filter(Turn.Step.family(_).nonEmpty)
    // A loop's record and tools run where its model calls do.
    def at(name: String): Int = Turn.Step.family(name) match {
      case Some(Turn.Step.RecordCall) | Some(Turn.Step.Ask) | Some(Turn.Step.Wait) |
          Some(Turn.Step.Tool) =>
        Turn.Step.all.indexOf(Turn.Step.CallModel)
      case family => family.fold(-1)(Turn.Step.all.indexOf)
    }
    val ahead = v.running.toVector.flatMap { now =>
      now +: Turn.Step.all.drop(at(now) + 1).filterNot(Turn.Step.optional.contains)
    }
    val shown = (recorded ++ ahead.filterNot(recorded.contains)).distinct
    shown.map { name =>
      val rune = Look.Runes.stepRune(name)
      val label = s"$rune ${name.padTo(NameCols, ' ')}"
      v.steps.find(_.name == name) match {
        case Some(done) =>
          val ms = done.ms.getOrElse(0L)
          row(
            s" $label" -> fg(t.ink),
            f"${seconds(ms)}%6s " -> fg(t.faint),
            "█" * bar(ms) -> fg(t.grit)
          )
        case None if v.running.contains(name) =>
          row(
            s" $rune " -> (fg(t.user) + Style.Bold),
            name.padTo(NameCols, ' ') -> (fg(t.ink) + Style.Bold),
            f"${seconds(runningMs)}%6s " -> fg(t.ink),
            "█" * bar(runningMs) -> fg(t.user)
          )
        case None => row(s" $label" -> fg(t.faint))
      }
    }
  }

  private def query(v: TurnView): Vector[Block] =
    v.query.toVector.map(q => row(" query    " -> fg(t.faint), q -> fg(t.ink)))

  /** The turn's pair, the upstream that served its reply, and `unprofiled` when grit knew
    * nothing of the pair; then each role whose pair differs.
    */
  private def models(v: TurnView): Vector[Block] =
    v.models.toVector.flatMap { m =>
      m.roles.zipWithIndex.map { case ((role, ref), i) =>
        val notes =
          if (i > 0) ""
          else (m.served.map(s => s" · $s") ++ Option.when(!m.profiled)(" · unprofiled")).mkString
        row(
          s" ${role.padTo(9, ' ')}" -> fg(t.faint),
          ref.toString -> fg(t.ink),
          notes -> fg(t.faint)
        )
      }
    }

  /** The window as one bar: system, what came from elsewhere, closing, recent, recalled and
    * the turn's own message, then what is left of `budget`, each a colour, with a legend
    * under it.
    */
  private def window(v: TurnView): Vector[Block] = v.window match {
    case None =>
      Vector(row(" window   " -> fg(t.faint), "recorded with the reply" -> fg(t.faint)))
    case Some(w) =>
      val scale = math.max(Tokens.value(w.total), Tokens.value(budget + w.system + w.message))
      val parts = Vector(
        (w.system, t.faint),
        (w.nearby, t.headerFg),
        (w.closing, t.thumb),
        (w.recent, t.grit),
        (w.recalled, t.user),
        (w.message, t.ink)
      ).map((n, c) => (cells(n, scale), c))
      val used = parts.map(_(0)).sum
      val bar = parts.filter(_(0) > 0).map((n, c) => "█" * n -> fg(c)) :+
        ("░" * math.max(0, WindowCells - used) -> fg(t.rail))
      val recalled =
        if (w.recalledTurns.isEmpty) row(" recalled " -> fg(t.faint), "none" -> fg(t.faint))
        else
          row(
            " recalled " -> fg(t.faint),
            w.recalledTurns.map(s => s"turn ${number(s)}").mkString(" · ") -> fg(t.user)
          )
      // Listed only when the window drew on elsewhere, so the panel stays as tall as before
      // for a window that did not.
      val nearby = Option
        .when(w.nearbyTurns.nonEmpty)(
          row(" nearby   " -> fg(t.faint), TurnView.Near.shown(w.nearbyTurns) -> fg(t.headerFg))
        )
        .toVector
      Vector(recalled) ++ nearby ++ Vector(
        blank,
        row(Seq(" window   " -> fg(t.faint)) ++ bar*),
        row(
          "          " -> fg(t.faint),
          s"${count(w.total)} of ${count(budget)} budget" -> fg(t.ink)
        ),
        legend("system", w.system, t.faint, "recent", w.recent, t.grit),
        legend("recalled", w.recalled, t.user, "message", w.message, t.ink),
        legend("closing", w.closing, t.thumb, "nearby", w.nearby, t.headerFg)
      )
  }

  private def legend(
      a: String,
      an: Tokens,
      ac: grit.tui.model.surface.Color,
      b: String,
      bn: Tokens,
      bc: grit.tui.model.surface.Color
  ): Block =
    row(
      "   " -> fg(t.faint),
      "■ " -> fg(ac),
      s"$a ${count(an)}".padTo(15, ' ') -> fg(t.faint),
      "■ " -> fg(bc),
      s"$b ${count(bn)}" -> fg(t.faint)
    )

  private def cost(v: TurnView): Vector[Block] = {
    val billed = v.billed.map(b => s"${count(b)} in")
    val spent = v.spent.map(dollars)
    val said = (billed ++ spent).mkString(" · ")
    Option
      .when(said.nonEmpty)(Vector(blank, row(" billed   " -> fg(t.faint), said -> fg(t.ink))))
      .getOrElse(Vector.empty)
  }

  private def cells(n: Tokens, scale: Long): Int =
    if (Tokens.value(n) <= 0) 0
    else math.max(1, math.round(Tokens.value(n).toDouble / scale * WindowCells).toInt)
}

object TurnPanel {

  /** The panel's width, its scrollbar included. */
  val Cols = 38

  /** The screen width from which the panel is shown beside the transcript. */
  val ShownFrom = 100

  private val NameCols = 16
  private val TopicCols = 28
  private val BarCells = 8
  private val WindowCells = 24

  /** A turn's number as people count: from 1. */
  def number(turn: TurnSeq): Long = TurnSeq.value(turn) + 1

  /** `ms` as seconds to a tenth, whatever the locale: `3.2s`. */
  def seconds(ms: Long): String = s"${ms / 1000}.${ms % 1000 / 100}s"

  /** A cost in dollars: `$0.00031`, or `≥ $0.00031` for a lower bound. */
  def dollars(cost: Cost): String = cost match {
    case Cost.Exact(usd) => plain(usd)
    case Cost.AtLeast(usd) => s"≥ ${plain(usd)}"
  }

  private def plain(usd: BigDecimal): String =
    s"$$${usd.bigDecimal.stripTrailingZeros.toPlainString}"

  /** A probability to two places, whatever the locale: `0.81`, `1.00`. */
  def share(p: Double): String = {
    val hundredths = math.round(math.min(1.0, math.max(0.0, p)) * 100)
    s"${hundredths / 100}.${(hundredths % 100).toString.reverse.padTo(2, '0').reverse}"
  }

  /** `text` cut to `cols` characters, the cut marked with `…`. */
  def clip(text: String, cols: Int): String =
    if (text.length <= cols) text else text.take(math.max(0, cols - 1)) + "…"

  /** A token count, short: `812`, `3.2k`. */
  def count(n: Tokens): String = {
    val v = Tokens.value(n)
    val tenths = v % 1000 / 100
    if (v < 1000) v.toString
    else if (tenths == 0) s"${v / 1000}k"
    else s"${v / 1000}.${tenths}k"
  }
}
