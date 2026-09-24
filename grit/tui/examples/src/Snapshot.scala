package grit.tui.examples

import grit.tui.model.surface.Size
import grit.tui.runtime.loop.Headless
import grit.tui.wire.paint.{Ansi, Painter}

/** [[Demo]]'s first frame, painted and printed raw -- no terminal setup, no runtime. The
  * same pure view a real run paints, for eyeballing and for byte-level pty scripting
  * (CLAUDE.md).
  *
  * ```./mill grit.tui.examples.runMain grit.tui.examples.Snapshot 30 100```
  */
object Snapshot {

  def main(args: Array[String]): Unit = {
    val rows = args.lift(0).flatMap(_.toIntOption).getOrElse(24)
    val cols = args.lift(1).flatMap(_.toIntOption).getOrElse(80)
    // The frame a real run paints is the paintable screen, so this one is too: rule 2
    // translated here, exactly as the runtime translates it before an app sees a size.
    val (frame, _) = Headless.start(Demo, Size.screen(Size(rows, cols))).painted
    // Raw painted bytes: absolute addressing, no bare LF, ?2026-wrapped, one write.
    System.out.print(Painter.paint(frame, None) + Ansi.reset + "\n")
  }
}
