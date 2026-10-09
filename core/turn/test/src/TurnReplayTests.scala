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

    test("every recorded weigh output is written back byte for byte") {
      // A weigh output is recorded data: null, kept and asked read back as they were written
      // before a failure kept its kind, and today's codec writes the same text back.
      val j = TurnJournal.weighed
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Weigh => (path.last, s.outcome)
        })
      }
      assert(outputs.map(_._1).toSet.size >= 4)
      val failures = outputs.collect {
        case (file, InMemoryDurable.Outcome.Output(text))
            if j.decode(text).map(j.encode) != Right(text) =>
          file
        case (file, InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker) => file
      }
      assert(failures.isEmpty)
    }

    test("every recorded offer is written back byte for byte") {
      // An offer is recorded data: one recorded before shapes reads as unshaped, and today's
      // codec writes the same text back, so recipes change no offer already recorded.
      val j = TurnJournal.recordedOffer
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Offer => (path.last, s.outcome)
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

    test("every recorded window is written back byte for byte") {
      // A window is recorded data: one recorded before notes, sections or documents existed
      // reads back, and today's codec writes the same text back, documents among them.
      val j = TurnJournal.window
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Assemble => (path.last, s.outcome)
        })
      }
      assert(outputs.exists(_._1 == "documented.json"))
      val failures = outputs.collect {
        case (file, InMemoryDurable.Outcome.Output(text))
            if j.decode(text).map(j.encode) != Right(text) =>
          file
        case (file, InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker) => file
      }
      failures ==> Vector.empty
    }

    test(
      "every recorded dispatch, expire, abandon and call-model output is written back byte for byte"
    ) {
      // A hosted call's and a model call's steps are recorded data, written by the phases a
      // turn shares with jobs: today's codecs read each and write the same text back, a
      // failure's kind among them. It pins the codecs, not the values today's body would
      // write: a change to what a step records reads and writes back the same here.
      import Turn.Step
      def again[A](j: grit.core.durable.Journaled[A], text: String): Boolean =
        j.decode(text).map(j.encode) == Right(text)
      def kind(name: String): Option[String] =
        if (
          Set(Step.CallModel, Step.CallModelAgain, Step.CallModelPlain)(name) ||
          name.startsWith(s"${Step.CallModel}:")
        )
          Some(Step.CallModel)
        else if (name.startsWith(s"${Step.Dispatch}:") || name.startsWith(s"${Step.Reach}:"))
          Some(Step.Dispatch)
        else if (name.startsWith(s"${Step.Expire}:")) Some(Step.Expire)
        else if (name.startsWith(s"${Step.Abandon}:")) Some(Step.Abandon)
        else None
      def roundTrips(kind: String, text: String): Boolean =
        if (kind == Step.CallModel) again(TurnJournal.reply, text)
        else if (kind == Step.Dispatch) again(TurnJournal.yesNo, text)
        else again(TurnJournal.requestState, text)
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(
          _.steps.flatMap(s => kind(s.name).map((path.last, _, s.outcome)))
        )
      }
      outputs.map(_._2).toSet ==> Set(Step.CallModel, Step.Dispatch, Step.Expire, Step.Abandon)
      assert(outputs.exists {
        case (_, _, InMemoryDurable.Outcome.Output(text)) => TurnJournal.failure(text).nonEmpty
        case _ => false
      })
      val failures = outputs.collect {
        case (file, k, InMemoryDurable.Outcome.Output(text)) if !roundTrips(k, text) =>
          s"$file: $k"
        case (file, k, InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker) =>
          s"$file: $k"
      }
      failures ==> Vector.empty
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
