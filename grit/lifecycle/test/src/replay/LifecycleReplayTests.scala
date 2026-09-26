package grit.lifecycle.replay

import grit.core.durable.{History, InMemoryDurable}
import grit.lifecycle.close.CloseFixtures
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.SettleFixtures
import grit.turn.Turn

import utest.*

/** The versioning gate for the close, settle and posting workflows (ADR 0004): every history of
  * either recorded under the engine's current epoch, [[Turn.Epoch]], must replay under
  * today's body. A step renamed, reordered, dropped or given an output the old records
  * cannot satisfy fails here, before it strands a workflow in flight.
  */
object LifecycleReplayTests extends TestSuite {
  import CloseFixtures.*
  import RecordLifecycleHistories.{Posted, postWorld}

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
    test("the current epoch has close, settle and posting histories to replay") {
      // Without them the gate below passes vacuously.
      val workflows = histories.flatMap(_._2.toOption.map(_.workflow)).toSet
      assert(workflows.contains("close"), workflows.contains("settle"), workflows.contains("post"))
    }

    test(
      "every close, settle and posting history of the current epoch replays under today's body"
    ) {
      val failures = histories.flatMap { case (path, parsed) =>
        val outcome = parsed.flatMap { history =>
          if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
          else
            history.workflow match {
              case "close" =>
                new InMemoryDurable().replay(history.id, history.steps)(
                  new World().body(
                    new Gate(None),
                    new Summariser(_ => Right(replyOf("Summary: x"))),
                    new SetClock(at(0))
                  )
                )
              case "settle" =>
                new InMemoryDurable().replay(history.id, history.steps)(
                  new SettleFixtures.World().body(new SettleFixtures.Weigher(None), 0)
                )
              case "post" =>
                val w = postWorld(0)
                new InMemoryDurable().replay(history.id, history.steps)(
                  Posting.body(
                    Vector(Posted),
                    PostEnv(w.periods, w.plugins.cursors, w.plugins.docs, new FakeJot)
                  )
                )
              case other => Left(s"a $other history")
            }
        }
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }
  }
}
