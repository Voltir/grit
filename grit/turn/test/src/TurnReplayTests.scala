package grit.turn

import grit.core.*
import utest.*

/** The versioning gate (ADR 0004): every history recorded under the current epoch must
  * replay under today's turn body. A step renamed, reordered, dropped or given an output
  * the old records cannot satisfy fails here, before it strands a turn in flight. Fix it
  * with `Durable.patch`, or change [[Turn.Epoch]] and start the new epoch's histories.
  */
object TurnReplayTests extends TestSuite {
  import TurnFixtures.*

  private def histories: Vector[(os.Path, Either[String, History])] = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    if (!os.isDir(dir)) Vector.empty
    else
      os.list(dir)
        .filter(_.ext == "json")
        .toVector
        .map(p => p -> History.read(ujson.read(os.read(p))))
  }

  val tests = Tests {
    test("the current epoch has histories to replay") {
      // Without them the gate below passes vacuously.
      assert(histories.nonEmpty)
    }

    test("every history of the current epoch replays under today's turn") {
      val failures = histories.flatMap { case (path, parsed) =>
        val outcome = parsed.flatMap { history =>
          if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
          else {
            val entries = new InMemoryEntryStore
            new InMemoryDurable().replay(history.id, history.steps)(
              turnBody(entries, new RecordingProvider)
            )
          }
        }
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }
  }
}
