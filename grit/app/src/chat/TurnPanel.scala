package grit.app.chat

import grit.app.look.{Look, Theme}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.tui.model.block.Block
import grit.tui.model.surface.Style
import grit.tui.model.text.StyledText
import grit.turn.Turn

/** The turn panel's rows, for a [[TurnView]]: the steps as a timeline, then the query,
  * the recalled turns, the window as a stacked bar against `budget`, and the cost. Every
  * row fits in [[TurnPanel.Cols]] columns, so none wraps.
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
          window(v) ++ cost(v)
    }

  /** The rows of the opened turn: what was asked, then all the panel shows. */
  def opened(view: Option[TurnView], runningMs: Long): Vector[Block] =
    view.toVector.flatMap(v =>
      Vector(
        Block.styled(
          StyledText.styled(s" ${Look.Runes.User} ", fg(t.user) + Style.Bold) ++
            StyledText.styled(v.asked, fg(t.ink) + Style.Bold)
        ),
        blank
      )
    ) ++ blocks(view, runningMs)

  private def title(v: TurnView, pinned: Boolean): Block = {
    val where = v.running.fold(if (v.settled) "done" else "finishing")(Look.Runes.step(_))
    row(
      Seq(s" TURN ${number(v.turn)}" -> (fg(t.ink) + Style.Bold), s"  · $where" -> fg(t.faint)) ++
        Option.when(pinned)("  esc: latest" -> fg(t.rail))*
    )
  }

  /** One row per step of the turn: recorded ones with their time and a bar to scale,
    * the running one lit, the rest dim.
    */
  private def steps(v: TurnView, runningMs: Long): Vector[Block] = {
    val longest = math.max(1L, (v.steps.flatMap(_.ms) :+ runningMs).max)
    def bar(ms: Long): Int = math.max(1, math.round(ms.toDouble / longest * BarCells).toInt)
    Turn.Step.all.map { name =>
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
        case None => row(s" $label" -> fg(t.rail))
      }
    }
  }

  private def query(v: TurnView): Vector[Block] =
    v.query.toVector.map(q => row(" query    " -> fg(t.faint), q -> fg(t.ink)))

  /** The window as one bar: system, recent, recalled and the turn's own message, then
    * what is left of `budget`, each a colour, with a legend under it.
    */
  private def window(v: TurnView): Vector[Block] = v.window match {
    case None =>
      Vector(row(" window   " -> fg(t.faint), "recorded with the reply" -> fg(t.rail)))
    case Some(w) =>
      val scale = math.max(Tokens.value(w.total), Tokens.value(budget + w.system + w.message))
      val parts = Vector(
        (w.system, t.faint),
        (w.recent, t.grit),
        (w.recalled, t.user),
        (w.message, t.ink)
      ).map((n, c) => (cells(n, scale), c))
      val used = parts.map(_(0)).sum
      val bar = parts.filter(_(0) > 0).map((n, c) => "█" * n -> fg(c)) :+
        ("░" * math.max(0, WindowCells - used) -> fg(t.rail))
      val recalled =
        if (w.recalledTurns.isEmpty) row(" recalled " -> fg(t.faint), "none" -> fg(t.rail))
        else
          row(
            " recalled " -> fg(t.faint),
            w.recalledTurns.map(s => s"turn ${number(s)}").mkString(" · ") -> fg(t.user)
          )
      Vector(
        recalled,
        blank,
        row(Seq(" window   " -> fg(t.faint)) ++ bar*),
        row(
          "          " -> fg(t.faint),
          s"${count(w.total)} of ${count(budget)} budget" -> fg(t.ink)
        ),
        legend("system", w.system, t.faint, "recent", w.recent, t.grit),
        legend("recalled", w.recalled, t.user, "message", w.message, t.ink)
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
    val spent = v.spent.map(c => s"$$${c.bigDecimal.stripTrailingZeros.toPlainString}")
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

  private val NameCols = 15
  private val BarCells = 8
  private val WindowCells = 24

  /** A turn's number as people count: from 1. */
  def number(turn: TurnSeq): Long = TurnSeq.value(turn) + 1

  /** `ms` as seconds to a tenth, whatever the locale: `3.2s`. */
  def seconds(ms: Long): String = s"${ms / 1000}.${ms % 1000 / 100}s"

  /** A token count, short: `812`, `3.2k`. */
  def count(n: Tokens): String = {
    val v = Tokens.value(n)
    val tenths = v % 1000 / 100
    if (v < 1000) v.toString
    else if (tenths == 0) s"${v / 1000}k"
    else s"${v / 1000}.${tenths}k"
  }
}
