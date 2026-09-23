package grit.tui.components

import grit.tui.model.surface.{Frame, PaneId, Pos, Rect, Size}

/** The compositor's vocabulary: overlays over a painted [[grit.tui.model.surface.Frame]],
  * each one a pure `Frame -> Frame` that reads the frame's own placement map.
  *
  * A modal is a compositing operation, not a drawing -- it dims what is behind it rather
  * than replacing it -- and a popup floats over a rect neither of them owns. Neither is
  * a child of the layout tree; both are layers over the painted base, anchored to
  * whatever the base actually painted. That is why these are combinators on the frame
  * rather than container views: a layer reads *where things landed*, which only exists
  * after something painted.
  *
  * Every extension here is capture-checked pure: the frame in, the frame out, plain
  * data throughout, and no wrapping anywhere (rule 7) -- layers blit views that were
  * already laid out, and place() is arithmetic.
  */
package object overlay {

  extension (f: Frame) {

    /** `modal` composited over this frame when `when`: the app dimmed, the shadow and
      * frame drawn, and `body` blitted into the content rect under `pane` so the next
      * `onInput` can find it. Identity when `when` is false or the screen is too small
      * for a modal worth painting -- an absent modal paints nothing and records
      * nothing, which is the honest answer for what there is to route against.
      */
    def dimmedBy(
        modal: Modal,
        body: View,
        pane: PaneId,
        screen: Size,
        when: Boolean = true
    ): Frame =
      if (!when) { f }
      else {
        modal.place(screen) match {
          case None => f
          case Some(rect) =>
            val dimmed = modal.render(f.surface, rect)
            Frame(dimmed.blit(body.render(rect.size), Pos(rect.top, rect.left), pane))
        }
      }

    /** `popup` floated over this frame, anchored to where `anchor` last painted: above
      * it when there is room, flipped below and clamped when there is not. Identity
      * when there is no popup, nothing matched, or the anchor never painted -- an empty
      * completion list is not a small box, it is absent.
      */
    def floating(popup: Option[(Popup, PaneId)], anchor: PaneId, screen: Size): Frame =
      f.placements(anchor) match {
        case None => f
        case Some(box) =>
          popup match {
            case None => f
            case Some((p, pane)) =>
              p.place(box, screen) match {
                case None => f
                case Some(rect) =>
                  Frame(f.surface.blit(p.render(rect.size), Pos(rect.top, rect.left), pane))
              }
          }
      }

    /** The hardware cursor at `at`, or hidden. */
    def withCaret(at: Option[Pos]): Frame = f.copy(cursor = at)

    /** The cursor placed from where `pane` last painted: `local` maps the pane's own
      * rect to a pane-local cursor cell, and the combinator turns it into a screen
      * position. Identity (cursor hidden) when `when` is false -- a frozen app shows no
      * caret -- or when the pane never painted.
      */
    def caretIn(pane: PaneId, local: Rect -> Option[Pos], when: Boolean = true): Frame =
      withCaret {
        if (!when) { None }
        else {
          f.placements(pane).flatMap { rect =>
            local(rect).map(p => Pos(rect.top + p.row, rect.left + p.col))
          }
        }
      }
  }
}
