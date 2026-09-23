package grit.tui.components.widget

import grit.tui.components.View
import grit.tui.components.layout.{Region, Split}
import grit.tui.components.pane.{Anchor, Panes}
import grit.tui.model.surface.{PaneId, Placements, Pos, Style}

/** A content pane and the scrollbar beside it, as one thing.
  *
  * The pieces were always general -- `Panes` owns the document and its row index,
  * `Scrollbar` owns the geometry -- but the *composition* of them was not, and every app
  * that wanted a scrollable screen wrote the same six things out: the split, the bar
  * built from the pane's own index, the wheel redirect for a track that has no document,
  * the thumb grab, the row-to-`DocPos` mapping, and the re-anchor. That is what this
  * holds, and it is why `StdApp.scrolls` exists rather than an app claiming the press
  * itself.
  *
  * The recorded decision it carries is unchanged: **a thumb drag is not a text drag.**
  * The track sits in the placement map so a press on it resolves to the track, and then
  * never reaches `Panes.onPress`, which would begin a selection in a pane holding no
  * document. What changes is only who states it -- the library, once, instead of each
  * app.
  *
  * It is deliberately not a [[View]] itself. [[views]] produces one, but the grab and
  * the wheel redirect are routing, and routing a press is something only the layer that
  * owns the panes can do (rule 6). A component that pretended otherwise would be the
  * flattening `View.Route` exists to prevent.
  */
final case class ScrollPane(
    content: PaneId,
    bar: PaneId,
    rail: Style = Style.plain,
    thumb: Style = Style.plain
) {

  /** The layout: the document takes what is left, the track takes one column. */
  def split: Split = Split.of(content -> Region.Flex(0), bar -> Region.Fixed(1))

  /** The pair painted -- the pane's viewport, and the bar beside it -- as one [[View]],
    * so a caller nests this wherever a view goes and never names the two halves apart.
    */
  def views(panes: Panes): View = split.views(panes.view(content), scrollbar(panes))

  /** The bar as the pane's own state says it is: the whole wrapped document as content,
    * the pane's last painted height as the window, and the row it is reading from as the
    * offset. Nothing here computes anything about wrapping.
    */
  def scrollbar(panes: Panes): Scrollbar =
    Scrollbar.of(panes, content, panes.rendered(content).size.rows, rail, thumb)

  /** The pane a wheel over `id` should scroll: a track has no document of its own, but
    * the wheel over it scrolls what it tracks. None when `id` is not this track.
    */
  def wheelTarget(id: PaneId): Option[PaneId] =
    if (id == bar) { Some(content) }
    else { None }

  /** `panes` re-anchored to where a thumb held at screen `pos` points.
    *
    * Unchanged when the track was not painted, or when the row names no position in the
    * document -- the honest answer in both cases, and neither is an error: a track can
    * be starved to nothing, and a row past the end of a short document names nothing.
    */
  def grabbed(panes: Panes, pos: Pos, at: Placements): Panes =
    at(bar) match {
      case None => panes
      case Some(track) =>
        val target = scrollbar(panes).offsetAtRow(track.rows, pos.row - track.top)
        panes.rowIndex(content)(target) match {
          case Some(dp) => panes.withAnchor(content, Anchor.At(dp))
          case None => panes
        }
    }
}
