package grit.app.look

import grit.tui.model.surface.Color

/** A palette by role: what each colour is for, not what it looks like. Every theme meets
  * the same contrast targets, which `ThemeContrastTests` holds it to: body text bright but
  * short of glaring, markers and labels a step down, faint text still readable, and rules
  * visible but quiet.
  *
  * @param ground the screen, under everything
  * @param slab the ground of the user's messages and the status line's kin
  * @param ink body text
  * @param faint text to read but not to dwell on: the thinking line, metadata, the
  *   prompt's rules
  * @param user the user's marker, in the transcript and the prompt
  * @param grit grit's marker
  * @param rail separators and the scrollbar's rail
  * @param thumb the scrollbar's thumb
  */
final case class Theme(
    key: String,
    name: String,
    ground: Color,
    slab: Color,
    ink: Color,
    faint: Color,
    user: Color,
    grit: Color,
    headerBg: Color,
    headerFg: Color,
    statusBg: Color,
    statusFg: Color,
    rail: Color,
    thumb: Color,
    failure: Color
)

object Theme {

  /** Icy cyan and lilac on a blue-black ground: the default. */
  val Frost: Theme = Theme(
    "frost",
    "Frost",
    ground = Color.hex("#0d1319"),
    slab = Color.hex("#1e2a37"),
    ink = Color.hex("#d3e0ec"),
    faint = Color.hex("#8ca1b6"),
    user = Color.hex("#c1b0ff"),
    grit = Color.hex("#6fe3f2"),
    headerBg = Color.hex("#6fe3f2"),
    headerFg = Color.hex("#0d1319"),
    statusBg = Color.hex("#16202b"),
    statusFg = Color.hex("#b3c4d4"),
    rail = Color.hex("#465b72"),
    thumb = Color.hex("#6fe3f2"),
    failure = Color.hex("#ff95a8")
  )

  /** After folke's tokyonight, Night, raised to the contrast targets. */
  val TokyoNight: Theme = Theme(
    "tokyo-night",
    "Tokyo Night",
    ground = Color.hex("#1a1b26"),
    slab = Color.hex("#262b3f"),
    ink = Color.hex("#d0d8fa"),
    faint = Color.hex("#8f99c4"),
    user = Color.hex("#cbb0ff"),
    grit = Color.hex("#7dcfff"),
    headerBg = Color.hex("#8fb3ff"),
    headerFg = Color.hex("#1a1b26"),
    statusBg = Color.hex("#262b3f"),
    statusFg = Color.hex("#c0c8ea"),
    rail = Color.hex("#4f5880"),
    thumb = Color.hex("#7aa2f7"),
    failure = Color.hex("#ff9aae")
  )

  /** After folke's tokyonight, Storm: a lifted slate ground. */
  val TokyoStorm: Theme = Theme(
    "tokyo-storm",
    "Tokyo Storm",
    ground = Color.hex("#24283b"),
    slab = Color.hex("#2c3149"),
    ink = Color.hex("#d8e0fc"),
    faint = Color.hex("#949cc3"),
    user = Color.hex("#d0b6ff"),
    grit = Color.hex("#86d6ff"),
    headerBg = Color.hex("#d0b6ff"),
    headerFg = Color.hex("#1f2335"),
    statusBg = Color.hex("#1f2335"),
    statusFg = Color.hex("#c2c9ec"),
    rail = Color.hex("#58628c"),
    thumb = Color.hex("#bb9af7"),
    failure = Color.hex("#ffa0b4")
  )

  /** After folke's tokyonight, Moon: softer, pinker. */
  val TokyoMoon: Theme = Theme(
    "tokyo-moon",
    "Tokyo Moon",
    ground = Color.hex("#222436"),
    slab = Color.hex("#2a2e45"),
    ink = Color.hex("#d2dcfa"),
    faint = Color.hex("#8f98cc"),
    user = Color.hex("#d2b4ff"),
    grit = Color.hex("#86e1fc"),
    headerBg = Color.hex("#9cbcff"),
    headerFg = Color.hex("#1e2030"),
    statusBg = Color.hex("#1e2030"),
    statusFg = Color.hex("#b8c5f0"),
    rail = Color.hex("#535c88"),
    thumb = Color.hex("#c099ff"),
    failure = Color.hex("#ff9fa6")
  )

  /** Deep navy with electric cyan. */
  val Abyss: Theme = Theme(
    "abyss",
    "Abyss",
    ground = Color.hex("#0b1020"),
    slab = Color.hex("#161e36"),
    ink = Color.hex("#c9d6ff"),
    faint = Color.hex("#8797c4"),
    user = Color.hex("#bba5ff"),
    grit = Color.hex("#5ce1e6"),
    headerBg = Color.hex("#8db1ff"),
    headerFg = Color.hex("#0b1020"),
    statusBg = Color.hex("#101730"),
    statusFg = Color.hex("#b0bfe8"),
    rail = Color.hex("#43548a"),
    thumb = Color.hex("#5ce1e6"),
    failure = Color.hex("#ff93b3")
  )

  /** Violet-heavy. */
  val Nightshade: Theme = Theme(
    "nightshade",
    "Nightshade",
    ground = Color.hex("#150f1f"),
    slab = Color.hex("#241a36"),
    ink = Color.hex("#e0d4ff"),
    faint = Color.hex("#a090c6"),
    user = Color.hex("#e2adff"),
    grit = Color.hex("#a2beff"),
    headerBg = Color.hex("#cda3ff"),
    headerFg = Color.hex("#150f1f"),
    statusBg = Color.hex("#1d1429"),
    statusFg = Color.hex("#cdbff2"),
    rail = Color.hex("#604c84"),
    thumb = Color.hex("#d58cff"),
    failure = Color.hex("#ff95b4")
  )

  val all: Vector[Theme] = Vector(Frost, TokyoNight, TokyoStorm, TokyoMoon, Abyss, Nightshade)

  val Default: Theme = Frost

  /** The theme whose key is `key`, ignoring case and surrounding space. */
  def named(key: String): Option[Theme] = all.find(_.key == key.trim.toLowerCase)
}
