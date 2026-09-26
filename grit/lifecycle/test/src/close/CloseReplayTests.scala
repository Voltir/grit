package grit.lifecycle.close

import grit.core.durable.{History, InMemoryDurable}
import grit.turn.Turn

import utest.*

/** The versioning gate for the close (ADR 0004): every close history recorded under the
  * engine's current epoch, [[Turn.Epoch]], must replay under today's close body. A step
  * renamed, reordered, dropped or given an output the old records cannot satisfy fails here,
  * before it strands a close in flight.
  */
object CloseReplayTests extends TestSuite {
  import CloseFixtures.*

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
    test("the current epoch has close histories to replay") {
      // Without them the gate below passes vacuously.
      assert(histories.nonEmpty)
    }

    test("every close history of the current epoch replays under today's close") {
      val failures = histories.flatMap { case (path, parsed) =>
        val outcome = parsed.flatMap { history =>
          if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
          else if (history.workflow != "close") Left(s"a ${history.workflow} history")
          else
            new InMemoryDurable().replay(history.id, history.steps)(
              new World().body(
                new Gate(None),
                new Summariser(_ => Right(replyOf("Summary: x"))),
                new SetClock(at(0))
              )
            )
        }
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }
  }
}
