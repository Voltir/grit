package grit.tui.examples

import grit.tui.model.block.Block
import grit.tui.model.input.{Button, Input, Mods, MouseEvent, MouseKind}
import grit.tui.model.select.Doc
import grit.tui.model.surface.{PaneId, Pos, Size}
import grit.tui.node.Loop
import grit.tui.runtime.std.Std

/** A timing harness, JMH-free: Demo2 (the std layer) against DemoNext (the one
  * tree) on the same 2,000-block transcript at 30x99. Each figure is the median over
  * `Runs` runs of the mean per operation inside the run, after a warmup.
  *
  * `./mill grit.tui.examples.runMain grit.tui.examples.NodeBench`
  */
object NodeBench {

  private val Runs = 31
  private val size = Size(30, 99)

  private val big: Doc = Doc(Vector.tabulate(2000) { i =>
    if (i % 4 == 3) Block.Separator(Palette.separator.ground)
    else
      Palette.assistant(
        s"$i: " + ("streamed words of a reply that wraps across the pane " * (1 + i % 5))
      )
  })

  private def median(xs: Vector[Double]): Double = xs.sorted.apply(xs.length / 2)

  /** Median over runs of the mean microseconds per op, `ops` ops per run. */
  private def time(ops: Int)(body: () => Unit): Double = {
    (0 until 5).foreach(_ => (0 until ops).foreach(_ => body()))
    median(Vector.fill(Runs) {
      val t0 = System.nanoTime()
      var i = 0
      while (i < ops) { body(); i += 1 }
      (System.nanoTime() - t0) / 1000.0 / ops
    })
  }

  private def mouse(kind: MouseKind, row: Int, col: Int): Input =
    Input.Mouse(
      MouseEvent(
        kind,
        if (kind == MouseKind.Release) Button.None else Button.Left,
        Pos(row, col),
        Mods.none
      )
    )

  private val drags: Vector[Input] =
    Vector.tabulate(50)(i => mouse(MouseKind.Drag, 6 + (i % 14), 10 + (i * 7) % 80))

  def main(args: Array[String]): Unit = {
    val _ = args
    val App = Demo2.DemoApp

    /* ---- the std layer ---- */
    val (o0, _) = App.init
    val o1 = o0.copy(panes = o0.panes.withDoc(PaneId.of("transcript"), big), streaming = false)
    val o2 = App.update(Std.Resized(size), o1)._1
    val oFrame = App.view(o2)(size)
    val oAt = oFrame.placements
    val oldFrame = time(20)(() => { val _ = App.view(o2)(size) })
    val oPressed =
      App.onInput(mouse(MouseKind.Press, 5, 10), o2, oAt).fold(o2)(m => App.update(m, o2)._1)
    val oldDrag = time(1)(() => {
      var s = oPressed
      drags.foreach(d => App.onInput(d, s, oAt).foreach(m => s = App.update(m, s)._1))
    }) / drags.length
    val oldTick = time(20)(() => {
      val s = App.update(Demo2.Msg.ToggleStream, o2)._1 // streaming on
      val t = App.update(Demo2.Msg.Tick, s)._1
      val _ = App.view(t)(size)
    })

    val keys: Vector[Input] =
      "typing a prompt".toVector.map(c => Input.Keyboard(grit.tui.model.input.Key.Printable(c)))
    val oldKey = time(1)(() => {
      var s = o2
      keys.foreach(k => App.onInput(k, s, oAt).foreach(m => s = App.update(m, s)._1))
    }) / keys.length

    // A resize to a new width: every block re-wraps in both designs.
    val narrow = Size(30, 79)
    val oldResize = time(3)(() => {
      val s = App.update(Std.Resized(narrow), o2)._1
      val _ = App.view(s)(narrow)
    })

    /* ---- the one tree ---- */
    val n0 = DemoNext.init._1.copy(transcript = big, streaming = false)
    val (_, l0) = Loop.paint(Loop.start[DemoNext.State, DemoNext.Msg](n0), DemoNext, size)
    val newFrame = time(20)(() => { val _ = Loop.paint(l0, DemoNext, size) })
    val (lp, _, _) = Loop.input(l0, mouse(MouseKind.Press, 5, 10), DemoNext)
    val newDrag = time(1)(() => {
      var l = lp
      drags.foreach(d => l = Loop.input(l, d, DemoNext)._1)
    }) / drags.length
    val newTick = time(20)(() => {
      val s = DemoNext.update(DemoNext.Msg.ToggleStream, l0.state)._1
      val t = DemoNext.update(DemoNext.Msg.Tick, s)._1
      val _ = Loop.paint(l0.copy(state = t), DemoNext, size)
    })
    // Every key after the first in a batch re-lays out the tree first (Loop.fresh).
    val newKey = time(1)(() => {
      var l = l0
      keys.foreach(k => l = Loop.input(l, k, DemoNext)._1)
    }) / keys.length
    val newResize = time(3)(() => { val _ = Loop.paint(l0, DemoNext, narrow) })
    // The burst as the runtime runs it: fifty drag events, then one paint.
    val newBurst = time(1)(() => {
      var l = lp
      drags.foreach(d => l = Loop.input(l, d, DemoNext)._1)
      val _ = Loop.paint(l, DemoNext, size)
    })

    println(f"2000 blocks at ${size.rows}x${size.cols}, median of $Runs runs (us)")
    println(f"  frame (view, layout, paint)     std ${oldFrame}%8.1f   node ${newFrame}%8.1f")
    println(f"  drag event (route + update)     std ${oldDrag}%8.1f   node ${newDrag}%8.1f")
    println(f"  stream tick (update + frame)    std ${oldTick}%8.1f   node ${newTick}%8.1f")
    println(f"  keystroke (route + update [+ relayout]) std ${oldKey}%8.1f   node ${newKey}%8.1f")
    println(f"  resize 99->79 cols (rewrap all) std ${oldResize}%8.1f   node ${newResize}%8.1f")
    println(f"  node: 50-event burst + 1 paint  ${newBurst}%8.1f")
  }
}
