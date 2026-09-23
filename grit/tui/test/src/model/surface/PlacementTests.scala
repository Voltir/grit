package grit.tui.model.surface

import utest.*

/** Placement-map and hit-testing tests. The map is the thing layoutz structurally
  * cannot have -- a String render has no per-cell identity -- and hit-testing is its
  * inverse, so the two are pinned together here: every placement test also asks
  * paneAt who is on top.
  */
object PlacementTests extends TestSuite {

  val tests = Tests {

    test("blit records a placement for the area it painted") {
      val s = Surface
        .blank(Size(4, 6))
        .blit(Surface.filled(Size(2, 2), Cell('#')), Pos(1, 2), PaneId.of("modal"))
      assert(s.panes == Vector(Placement(PaneId.of("modal"), Rect(1, 2, 2, 2))))
    }

    test("blit records the clipped rect, and nothing when nothing was painted") {
      val partial =
        Surface
          .blank(Size(2, 4))
          .blit(Surface.filled(Size(1, 3), Cell('#')), Pos(0, 2), PaneId.of("p"))
      assert(partial.panes == Vector(Placement(PaneId.of("p"), Rect(0, 2, 1, 2))))
      val off =
        Surface
          .blank(Size(2, 4))
          .blit(Surface.filled(Size(1, 3), Cell('#')), Pos(0, 9), PaneId.of("p"))
      assert(off.panes.isEmpty)
    }

    test("a plain blit records nothing") {
      val s = Surface.blank(Size(2, 2)).blit(Surface.filled(Size(1, 1), Cell('#')), Pos(0, 0))
      assert(s.panes.isEmpty)
    }

    test("later blits stack after earlier ones, so the map is innermost-last") {
      val s = Surface
        .blank(Size(3, 3))
        .blit(Surface.filled(Size(3, 3), Cell('a')), Pos(0, 0), PaneId.of("base"))
        .blit(Surface.filled(Size(1, 1), Cell('m')), Pos(1, 1), PaneId.of("modal"))
      assert(
        s.panes == Vector(
          Placement(PaneId.of("base"), Rect(0, 0, 3, 3)),
          Placement(PaneId.of("modal"), Rect(1, 1, 1, 1))
        )
      )
    }

    test("paneAt returns the topmost pane and pane-local coordinates") {
      val s = Surface
        .blank(Size(4, 8))
        .blit(Surface.filled(Size(2, 4), Cell('a')), Pos(1, 0), PaneId.of("base"))
        .blit(Surface.filled(Size(2, 2), Cell('m')), Pos(1, 2), PaneId.of("modal"))
      assert(Hit.paneAt(s, Pos(1, 3)) == Some((PaneId.of("modal"), Pos(0, 1))))
      assert(Hit.paneAt(s, Pos(1, 0)) == Some((PaneId.of("base"), Pos(0, 0))))
    }

    test("paneAt is None outside every pane") {
      val s =
        Surface
          .blank(Size(3, 3))
          .blit(Surface.filled(Size(1, 1), Cell('#')), Pos(0, 0), PaneId.of("p"))
      assert(Hit.paneAt(s, Pos(2, 2)).isEmpty)
    }

    test("blit carries a patch's own placements, translated") {
      val inner =
        Surface
          .blank(Size(1, 3))
          .blit(Surface.filled(Size(1, 1), Cell('x')), Pos(0, 2), PaneId.of("item"))
      val s = Surface.blank(Size(3, 6)).blit(inner, Pos(1, 0), PaneId.of("list"))
      assert(
        s.panes == Vector(
          Placement(PaneId.of("list"), Rect(1, 0, 1, 3)),
          Placement(PaneId.of("item"), Rect(1, 2, 1, 1))
        )
      )
      assert(Hit.paneAt(s, Pos(1, 2)) == Some((PaneId.of("item"), Pos(0, 0))))
    }

    test("carried placements are clipped to the destination grid") {
      val inner =
        Surface
          .blank(Size(2, 2))
          .blit(Surface.filled(Size(1, 1), Cell('x')), Pos(0, 1), PaneId.of("item"))
      // item's cell would land at col 6, off the 6-wide grid
      val s = Surface.blank(Size(2, 6)).blit(inner, Pos(0, 5), PaneId.of("patch"))
      assert(s.panes == Vector(Placement(PaneId.of("patch"), Rect(0, 5, 2, 1))))
    }

    test("placements round-trips off the surface and off the frame") {
      val s = Surface
        .blank(Size(6, 6))
        .blit(Surface.filled(Size(2, 2), Cell('#')), Pos(1, 1), PaneId.of("p"))
      assert(s.placements(PaneId.of("p")).contains(Rect(1, 1, 2, 2)))
      assert(s.placements.at(Pos(2, 2)) == Some((PaneId.of("p"), Pos(1, 1))))
      assert(Frame(s).placements.all == s.panes)
    }
  }
}
