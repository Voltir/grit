package grit.tui.examples

import grit.tui.components.layout.Border
import grit.tui.wire.paint.*
import grit.tui.model.surface.*

/** One frame, painted and printed raw -- no terminal setup, no runtime. The same pure
  * view a real run paints, for eyeballing and for byte-level pty scripting (CLAUDE.md).
  *
  * ```./app grit.tui.examples.Snapshot 30 100```
  */
object Snapshot {

  def main(args: Array[String]): Unit = {
    val rows = args.lift(0).flatMap(_.toIntOption).getOrElse(24)
    val cols = args.lift(1).flatMap(_.toIntOption).getOrElse(80)
    // The frame a real run paints is the paintable screen, so this one is too: rule 2
    // translated here, exactly as the runtime translates it before an app sees a size.
    val frame = demo(Size.screen(Size(rows, cols)))

    // Raw painted bytes: absolute addressing, no bare LF, ?2026-wrapped, one write.
    System.out.print(Painter.paint(frame, None) + Ansi.reset + "\n")

    System.err.println(s"panes:  ${frame.surface.panes.mkString(", ")}")
    System.err.println(s"hit at center: ${Hit.paneAt(frame.surface, Pos(rows / 2, cols / 2))}")
  }

  /** A transcript, a reverse-video selection over it, and a modal blitted on top --
    * enough to see the placement map and the painter's rules at work.
    */
  def demo(size: Size): Frame = {
    val bar = Style(reverse = true)
    var s = Surface.blank(size)

    s = s.fill(Rect(0, 0, 1, size.cols), Cell(' ', bar))
    s = s.write(0, 2, " grit.tui -- one painted frame ", bar)

    val lines = Vector(
      "grit> the signature is the whole truth about a function.",
      "you> and capture checking makes that a compiler-enforced promise.",
      "grit> effects are data. a thunk launders capabilities past the checker.",
      "you> timers are cancellable; nothing self-re-arms without a handle.",
      "grit> a drag belongs to the pane it began in, and that pane clamps it.",
      "you> restore in reverse, idempotently, every time."
    )
    val bodyRows = math.max(1, size.rows - 4)
    s = lines.take(bodyRows).zipWithIndex.foldLeft(s) { case (acc, (line, i)) =>
      acc.write(2 + i, 1, line)
    }
    // a selection mid-line on the first body row
    s = s.write(2, 7, "the signature is the whole truth", Style(reverse = true))

    val statusRow = size.rows - 1
    s = s.write(statusRow, 0, " ?2026 synced -- absolute addressing -- last column untouched")

    // modal over the transcript, opaque, with its own pane placement
    val modal = box(Size(5, 30), "modal")
    s = s.blit(modal, Pos(size.rows / 2 - 1, (size.cols - modal.size.cols) / 2), PaneId.of("modal"))

    Frame(s, cursor = Some(Pos(statusRow, 52)))
  }

  /** A titled frame at `size` -- the library's, not a fifth hand-drawn copy. */
  private def box(size: Size, title: String): Surface =
    Border.draw(
      Surface.filled(size, Cell(' ')),
      Rect(0, 0, size.rows, size.cols),
      Border.Round,
      title
    )
}
