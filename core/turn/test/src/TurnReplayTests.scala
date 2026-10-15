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

  private def histories: Vector[(os.Path, Either[String, History])] = historiesOf(Turn.Epoch)

  private def historiesOf(epoch: String): Vector[(os.Path, Either[String, History])] = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / epoch
    if (!os.isDir(dir)) Vector.empty
    else
      os.list(dir)
        .filter(_.ext == "json")
        .toVector
        .map(p => p -> History.read(ujson.read(os.read(p))))
  }

  /** The epoch before this one, whose histories the rename map is checked against. */
  private val Previous = "2026-10-14"

  /** The name the previous epoch's step `name` has in this one; `None` for a retired patch's
    * marker, which this epoch's turns no longer record.
    */
  private def renamed(name: String): Option[String] = {
    val Call = """call-model:(\d+)""".r
    val Record = """record-call:(\d+)""".r
    val Tool = """tool:(\d+):(\d+)""".r
    val Ask = """ask:(\d+):(\d+)""".r
    val Send = """dispatch:(\d+)""".r
    val Dispatch = """dispatch:(\d+):(\d+)""".r
    val Reach = """reach:(\d+):(.+)""".r
    val Expire = """expire:(\d+):(\d+)""".r
    val Abandon = """abandon:(\d+):(\d+)""".r
    name match {
      case "DBOS.patch-integrity" | "DBOS.patch-record-failure" => None
      case "call-model" => Some("ask:0")
      case Call(n) => Some(s"ask:$n")
      case Record(n) => Some(s"ask:$n:record")
      case Tool(n, j) => Some(s"call:$n:$j")
      case Ask(n, j) => Some(s"call:$n:$j:approve")
      case Send(n) => Some(s"call:$n:send")
      case Dispatch(n, j) => Some(s"call:$n:$j:dispatch")
      case Reach(n, place) => Some(s"call:$n:send:$place")
      case Expire(n, j) => Some(s"call:$n:$j:expire")
      case Abandon(n, j) => Some(s"call:$n:$j:abandon")
      case other => Some(other)
    }
  }

  /** Each history of `epoch`, by its file's name, as the names of its steps. */
  private def namesOf(epoch: String): Map[String, Vector[String]] =
    historiesOf(epoch).collect { case (path, Right(h)) => path.last -> h.steps.map(_.name) }.toMap

  val tests = Tests {
    test(
      "each history of the previous epoch, its steps renamed, is its scenario's in this one, less the retired patches' markers"
    ) {
      val (before, after) = (namesOf(Previous), namesOf(Turn.Epoch))
      // A history recorded before the integrity patch shipped did not take it, and ran its
      // loop ungated: under this epoch every round's model call follows its gate.
      val Asked = """ask:(\d+)""".r
      def now(names: Vector[String]): Vector[String] = {
        val renaming = names.flatMap(renamed)
        if (names.contains("DBOS.patch-integrity")) renaming
        else
          renaming.flatMap {
            case n @ Asked(round) => Vector(s"gate:$round", n); case n => Vector(n)
          }
      }
      (before.size, before.keySet == after.keySet) ==> (57, true)
      before.map((file, names) => file -> now(names)) ==> after
    }

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
            // steps a history holds depend on which tools its calls bind to.
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

    test("every recorded route is written back byte for byte") {
      // A route is recorded data: each role's pin as a profile writes it, a role with none
      // cleared absent, and a turn with none cleared its failure.
      val j = TurnJournal.routing
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Route => (path.last, s.outcome)
        })
      }
      assert(outputs.map(_._1).toSet.size >= 6)
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
      val j = TurnJournal.decidedOffer
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

    test("every recorded gate is written back byte for byte") {
      // A gate is recorded data: the round's request is rebuilt from it, never from rows.
      val j = TurnJournal.gated
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if Turn.Step.family(s.name).contains(Turn.Step.Gate) => (path.last, s.outcome)
        })
      }
      assert(outputs.exists(_._1 == "gated-withheld.json"))
      // One that hid an item, and kept its name until it was read.
      assert(outputs.exists(_._1 == "gated-hidden.json"))
      // One whose hidden item was asked through extract, and stayed hidden.
      assert(outputs.exists(_._1 == "gated-extracted.json"))
      val failures = outputs.collect {
        case (file, InMemoryDurable.Outcome.Output(text))
            if j.decode(text).map(j.encode) != Right(text) =>
          file
        case (file, InMemoryDurable.Outcome.Threw(_) | InMemoryDurable.Outcome.Marker) => file
      }
      failures ==> Vector.empty
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
      "every recorded send, dispatch, expire, abandon and model call output is written back byte for byte"
    ) {
      // A hosted call's and a model call's steps are recorded data, written by the phases a
      // turn shares with jobs: today's codecs read each and write the same text back, a
      // failure's kind among them. It pins the codecs, not the values today's body would
      // write: a change to what a step records reads and writes back the same here.
      import Turn.Step
      def again[A](j: grit.core.durable.Journaled[A], text: String): Boolean =
        j.decode(text).map(j.encode) == Right(text)
      val kinds = Set(Step.Ask, Step.Send, Step.Dispatch, Step.Expire, Step.Abandon)
      def kind(name: String): Option[String] = Step.family(name).filter(kinds.contains)
      def roundTrips(kind: String, text: String): Boolean =
        if (kind == Step.Ask) again(TurnJournal.reply, text)
        else if (kind == Step.Send || kind == Step.Dispatch) again(TurnJournal.yesNo, text)
        else again(TurnJournal.requestState, text)
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(
          _.steps.flatMap(s => kind(s.name).map((path.last, _, s.outcome)))
        )
      }
      outputs.map(_._2).toSet ==> kinds
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
