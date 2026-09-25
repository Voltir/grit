package grit.app.look

import grit.tui.model.surface.Color

import utest.*

/** Every theme against the same contrast targets, scored with APCA (the model WCAG 3 is
  * built on; SAPC 0.0.98G constants), which reads light text on dark grounds more honestly
  * than WCAG 2 ratios. A theme that drifts out of a band fails here, named with its score.
  */
object ThemeContrastTests extends TestSuite {

  /** APCA lightness contrast of `text` on `ground`, as a magnitude: 0 to about 106. */
  def lc(text: Color, ground: Color): Double = {
    def y(c: Color): Double = {
      def ch(v: Int) = math.pow(v / 255.0, 2.4)
      0.2126729 * ch(c.r) + 0.7151522 * ch(c.g) + 0.0721750 * ch(c.b)
    }
    def clamp(v: Double) = if (v < 0.022) v + math.pow(0.022 - v, 1.414) else v
    val t = clamp(y(text))
    val b = clamp(y(ground))
    val sapc =
      if (b > t) (math.pow(b, 0.56) - math.pow(t, 0.57)) * 1.14
      else (math.pow(b, 0.65) - math.pow(t, 0.62)) * 1.14
    if (math.abs(sapc) < 0.1) 0.0
    else math.abs((if (sapc > 0) sapc - 0.027 else sapc + 0.027) * 100)
  }

  /** A role pair, text on ground, and its band in Lc. */
  private final case class Target(
      role: String,
      pair: Theme -> (Color, Color),
      low: Double,
      high: Double
  )

  private val Targets: Vector[Target] = Vector(
    Target("body text", t => (t.ink, t.ground), 75, 90.5),
    Target("the user's text", t => (t.ink, t.slab), 75, 106),
    Target("faint text", t => (t.faint, t.ground), 45, 60.5),
    // The turn panel and dialogs sit on the slab; their labels are secondary, a step
    // below faint on the ground.
    Target("faint on the slab", t => (t.faint, t.slab), 42, 60.5),
    // The frames and keys of dialogs and the command palette, which sit on the slab.
    Target("grit on the slab", t => (t.grit, t.slab), 60, 106),
    Target("status line", t => (t.statusFg, t.statusBg), 60, 106),
    Target("user marker", t => (t.user, t.ground), 60, 106),
    Target("grit marker", t => (t.grit, t.ground), 60, 106),
    Target("header", t => (t.headerFg, t.headerBg), 60, 106),
    Target("failure", t => (t.failure, t.ground), 60, 106),
    Target("rails", t => (t.rail, t.ground), 15, 30.5),
    Target("scroll thumb", t => (t.thumb, t.ground), 30, 106)
  )

  val tests = Tests {

    test("the scorer agrees with APCA's published reference pairs") {
      // #888888 on #ffffff is Lc 63.1 and #ffffff on #888888 is Lc -68.5 (apca-w3 README).
      assert(math.abs(lc(Color.hex("#888888"), Color.hex("#ffffff")) - 63.1) < 0.2)
      assert(math.abs(lc(Color.hex("#ffffff"), Color.hex("#888888")) - 68.5) < 0.2)
    }

    test("every theme meets every target") {
      val misses = Theme.all.flatMap { theme =>
        Targets.flatMap { target =>
          val (text, ground) = target.pair(theme)
          val score = lc(text, ground)
          Option.when(score < target.low || score > target.high)(
            f"${theme.key} ${target.role}: Lc $score%.1f, wants ${target.low}%.0f-${target.high}%.0f"
          )
        }
      }
      assert(misses.isEmpty)
    }

    test("themes are found by key, the default among them") {
      Theme.named(" Tokyo-Night ") ==> Some(Theme.TokyoNight)
      Theme.named("sandstone") ==> None
      assert(Theme.all.contains(Theme.Default))
      Theme.all.map(_.key).distinct.size ==> Theme.all.size
    }
  }
}
