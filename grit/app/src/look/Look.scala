package grit.app.look

import grit.tui.components.editor.Editor
import grit.tui.model.block.Block
import grit.tui.model.surface.Style
import grit.tui.model.text.StyledText

/** The chat screen's styles, from `theme`. Colour only, never the terminal's dim or
  * italic: terminals draw those as they like, and a meaning carried by them can vanish.
  */
final case class Look(theme: Theme) {

  /** Under the whole screen, so no cell is left to the terminal's own colours. */
  def ground: Style = Style.bg(theme.ground) + Style.fg(theme.ink)

  def user(text: String): Block.Text =
    Block
      .styled(
        StyledText.styled("you> ", Style.fg(theme.user) + Style.Bold) ++ StyledText
          .styled(text, Style.fg(theme.ink))
      )
      .copy(ground = Style.bg(theme.slab))

  def assistant(text: String): Block.Text =
    Block.styled(
      StyledText.styled("grit> ", Style.fg(theme.grit) + Style.Bold) ++ StyledText
        .styled(text, Style.fg(theme.ink))
    )

  def thinking: Block.Text =
    Block.styled(StyledText.styled("grit is thinking…", Style.fg(theme.faint)))

  def failure(reason: String): Block.Text =
    Block.styled(StyledText.styled(s"! $reason", Style.fg(theme.failure)))

  def separator: Block.Separator = Block.Separator(Style.fg(theme.rail))

  def header: Style = Style.fg(theme.headerFg) + Style.Bold + Style.bg(theme.headerBg)
  def status: Style = Style.fg(theme.statusFg) + Style.bg(theme.statusBg)
  def scrollRail: Style = Style.fg(theme.rail)
  def scrollThumb: Style = Style.fg(theme.thumb)

  def prompt: Editor =
    Editor(
      "",
      0,
      focused = true,
      title = "message",
      body = Style.fg(theme.ink),
      chrome = Style.fg(theme.chrome),
      titleStyle = Style.fg(theme.ink) + Style.Bold
    )
}
