package grit.tui

/** The widget convention — a function from state to cells, not a framework.
  *
  * A widget is a pure function from its state to a [[grit.tui.model.surface.Surface]] sized
  * exactly to the region it will be blitted into. That is the whole contract:
  *
  *   - **Identity is its place in the tree.** An app paints a widget into a box of its
  *     screen's [[grit.tui.components.Node]] tree, and widget-local hit-testing is a
  *     geometry method on the widget's own state (see
  *     [[grit.tui.components.widget.Scrollbar]].offsetAtRow). No class hierarchy.
  *   - **Same state, same surface.** A widget that re-renders unchanged state must hand
  *     the diff painter two identical frames, so a tick that changed nothing writes
  *     zero bytes. Every widget's suite pins this with a real [[grit.tui.wire.paint.Painter]]
  *     diff.
  *   - **Chrome truncates, content wraps.** A widget may cut its text to its region
  *     (in display columns, never mid-glyph) but never reflows onto more rows: chrome
  *     that grew would change the transcript's height.
  *
  * Widgets live below `app` like every other model package: they import `surface` and
  * `text`, never `paint` or `term`.
  */
package object widget
