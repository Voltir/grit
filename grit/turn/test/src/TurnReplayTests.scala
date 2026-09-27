package grit.turn

import grit.core.durable.{History, InMemoryDurable}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}

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
            // The tools the recorder offered: a gated call asks before its step, so the
            // steps a history holds depend on which tools its calls bind to. What its offer
            // step names by id is read back from the rows the history keeps.
            keepAll(history.kept).flatMap { _ =>
              new InMemoryDurable().replay(history.id, history.steps)(
                tooledBody(
                  entries,
                  new RecordingProvider,
                  new InMemoryUsageLedger,
                  new grit.models.StubProvider(),
                  NoClassifier,
                  NoCheckout,
                  tools(NoCheckout),
                  5,
                  hosted = hostedTools
                )
              )
            }
          }
        }
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }

    test("every recorded classification is written back byte for byte") {
      // Topic events are recorded data: today's codec must read them and write the same text.
      val j = TurnJournal.classification
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == "classify" => (path.last, s.outcome)
        })
      }
      assert(outputs.nonEmpty)
      val failures = outputs.collect {
        case (file, InMemoryDurable.Outcome.Output(text))
            if j.decode(text).map(j.encode) != Right(text) =>
          file
        case (file, InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker) => file
      }
      assert(failures.isEmpty)
    }
  }
}
