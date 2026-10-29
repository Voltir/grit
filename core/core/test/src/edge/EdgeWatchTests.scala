package grit.core.edge

import scala.concurrent.duration.*

import grit.core.id.{DocKey, PluginName}
import grit.core.identity.TestAccounts
import grit.core.place.Place

import utest.*

/** A watch's bounds and what it waits for, as pure values. */
object EdgeWatchTests extends TestSuite {

  private def ok[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test("a watch lasts its within, at most the longest, and none when not positive") {
      assert(
        EdgeWatch.bounded(3.seconds) == 3.seconds,
        EdgeWatch.bounded(1.hour) == EdgeWatch.Longest,
        EdgeWatch.bounded(Duration.Zero - 1.second) == Duration.Zero
      )
    }
    test("a watch for a document holds a ref to wait for") {
      val held =
        WatchFor.Holding.of(ok(PluginName.of("projects")), ok(DocKey.of("grit/a")), Set.empty)
      assert(held == Left("a watch names a ref to hold"))
    }
    test("an edge built without a watch refuses each one as unavailable") {
      val got = EdgeWatch.Refusing.until(
        TestAccounts.account("slack:T1/U-ann"),
        Place.Everywhere,
        WatchFor.Committed(WatchFor.Worker.At(Place.Everywhere), java.time.Instant.EPOCH),
        1.second
      )
      assert(got == Left(WatchError.Unavailable("this edge was given no watch")))
    }
  }
}
