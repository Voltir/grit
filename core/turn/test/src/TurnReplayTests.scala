package grit.turn

import grit.core.durable.{History, InMemoryDurable, RecordedSizes}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}

import utest.*

/** The versioning gate (ADR 0004): every history recorded under the current epoch must
  * replay under today's turn body. A step renamed, reordered, dropped or given an output
  * the old records cannot satisfy fails here, before it strands a turn in flight. Fix it
  * with `Durable.patch`, or change [[Turn.Epoch]] and start the new epoch's histories.
  */
object TurnReplayTests extends TestSuite {
  import TurnFixtures.*

  private def histories: Vector[(String, Either[String, History])] = historiesOf(Turn.Epoch)

  private def historiesOf(epoch: String): Vector[(String, Either[String, History])] =
    History.recorded(epoch)

  /** The epoch before this one, whose histories the rename map is checked against. */
  private val Previous = "2026-10-16"

  /** The most bytes a step of each family may record in these histories, about twice the
    * most any records today, each with what it records. A step new to the turn has none until
    * it is given one here.
    */
  private val Budgets: Map[String, Int] = Map(
    Turn.Step.PinModels -> 64, // the pinned profile's id
    Turn.Step.Weigh -> 512, // the answers triage kept or asked, a few tags
    Turn.Step.Route -> 800, // each role's pin by its place, and the task call's answers and cost
    Turn.Step.Offer -> 1100, // the offer by reference: ids, tool names, places, its shape
    Turn.Step.RecordFailure -> 64, // whether the failure was kept
    Turn.Step.Stitched -> 64, // what a placement did, in a line
    Turn.Step.Classify -> 1024, // the agenda events and the classifier call's cost
    Turn.Step.RecordAgenda -> 64, // an entry id
    Turn.Step.Needs -> 320, // the needs gate's verdict and its call's cost
    Turn.Step.NeedsRecord -> 64, // whether the cost was kept
    Turn.Step.Query -> 640, // the query written, or why none was, and its call's cost
    Turn.Step.QueryRecord -> 64, // whether the cost was kept
    // What the search found, by reference and score: a full one of 60 fits, about 22 bytes each.
    Turn.Step.Search -> 1400,
    Turn.Step.Relevance -> 840, // a batch's verdicts, by reference, and its call's cost
    Turn.Step.RelevanceRecord -> 64, // whether the cost was kept
    Turn.Step.Assemble -> 640, // the window's entry seqs and notes
    Turn.Step.RecordWindow -> 128, // the loop's limits
    Turn.Step.Gate -> 800, // a round's level and the items it hid or lowered
    Turn.Step.Ask -> 1024, // a model reply: these fixtures' are a few lines
    Turn.Step.Record -> 512, // the reply's entry id and its calls' kinds
    Turn.Step.Call -> 128, // a result's entry id and whether it failed
    Turn.Step.Approve -> 64, // a request's id
    Turn.Step.Send -> 64, // whether the requests were sent
    Turn.Step.Dispatch -> 64, // whether the request was sent
    Turn.Step.Expire -> 64, // whether the wait was given up
    Turn.Step.Abandon -> 64, // whether the wait was given up
    "DBOS.recv" -> 64, // a person's answer, or an edge's ring
    "DBOS.sleep" -> 64, // when the wait ends
    Turn.Step.RecordVerdict -> 64, // an entry id
    Turn.Step.Append -> 64, // the reply's entry id
    Turn.Step.Judge -> 320, // the judgement and its call's cost
    Turn.Step.RecordSpeech -> 320, // what became of the draft
    Turn.Step.Summarise -> 1500, // the summary's message
    Turn.Step.AppendSummary -> 64 // the summary's entry id
  )

  /** The most bytes one history's steps may record in all, about twice the most today's do. */
  private val Whole = 6800

  /** The needs gate's families, new in this epoch: the previous one went from `record-topic`
    * straight to `ask:query`.
    */
  private val Gating: Set[String] = Set(Turn.Step.Needs, Turn.Step.NeedsRecord)

  /** The families a turn's window is drawn in before `assemble`. */
  private val Drawing: Set[String] = Gating ++ Set(
    Turn.Step.Query,
    Turn.Step.QueryRecord,
    Turn.Step.Search,
    Turn.Step.Relevance,
    Turn.Step.RelevanceRecord
  )

  /** The assembly stage `names` recorded: its drawing steps and `assemble`, in order. */
  private def stage(names: Vector[String]): Vector[String] =
    names.filter(n => n == Turn.Step.Assemble || Turn.Step.family(n).exists(Drawing.contains))

  /** Each history's assembly stage, by its file's name, where it is not `ask:needs`,
    * `ask:query` then `assemble` (no gate answered, nothing searched): a gate that answered, a
    * query written and searched for, or a query step that kept its cost.
    */
  private val Stages: Map[String, Vector[String]] = {
    val judged =
      (0 to 2).toVector.flatMap(n => Vector(s"ask:relevance-$n", s"ask:relevance-$n:record"))
    val unjudged = (0 to 2).toVector.map(n => s"ask:relevance-$n")
    val searched = Vector("ask:query", "ask:query:record", "search")
    val written = Vector("ask:needs", "ask:query", "ask:query:record", "assemble")
    Map(
      // Gated and searched for 25, judged in three batches by a classifier that answered each.
      "queried.json" -> (Vector("ask:needs", "ask:needs:record") ++ searched ++ judged :+
        "assemble"),
      // Searched for one earlier turn, with no classifier: the gate and each batch asked nothing.
      "recalled.json" -> (Vector("ask:needs") ++ searched ++ unjudged :+ "assemble"),
      // Said to grit, its message rejected by the gate: drawn with no query, its writer unasked.
      "unneeded.json" -> Vector("ask:needs", "ask:needs:record", "ask:query", "assemble"),
      // A query written under the route's query pin, and drawn on: no search.
      "routed.json" -> written,
      "routed-floored.json" -> written,
      "routed-no-summary.json" -> written,
      "routed-second-rung.json" -> written,
      "routed-tasked.json" -> written
    )
  }

  /** Each history of `epoch`, by its file's name, as the names of its steps. */
  private def namesOf(epoch: String): Map[String, Vector[String]] =
    historiesOf(epoch).collect { case (path, Right(h)) => path -> h.steps.map(_.name) }.toMap

  val tests = Tests {
    test(
      "each history of the previous epoch is its scenario's in this one, less the needs gate's steps"
    ) {
      val (before, after) = (namesOf(Previous), namesOf(Turn.Epoch))
      def ungated(names: Vector[String]): Vector[String] =
        names.filterNot(n => Turn.Step.family(n).exists(Gating.contains))
      // The shapes the gate's rejection made, which no history before it could have.
      val rejected = Set("unneeded.json", "heard-unneeded.json")
      (before.size, before.keySet == after.keySet.diff(rejected)) ==> (57, true)
      before ==> after.collect {
        case (file, names) if !rejected.contains(file) => file -> ungated(names)
      }
    }

    test(
      "each history's assembly stage is its scenario's: a written query searched for and judged, else drawn"
    ) {
      val reached = namesOf(Turn.Epoch).filter((_: String, names: Vector[String]) =>
        names.contains(Turn.Step.Assemble)
      )
      Stages.keySet.diff(reached.keySet) ==> Set.empty
      reached.map((file: String, names: Vector[String]) => file -> stage(names)) ==>
        reached.map((file: String, _: Vector[String]) =>
          file -> Stages.getOrElse(
            file,
            Vector(Turn.Step.Needs, Turn.Step.Query, Turn.Step.Assemble)
          )
        )
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
        outcome.left.toOption.map(why => s"$path: $why")
      }
      assert(failures.isEmpty)
    }

    test("every recorded weigh output is written back byte for byte") {
      // A weigh output is recorded data: null, kept and asked read back as they were written
      // before a failure kept its kind, and today's codec writes the same text back.
      val j = TurnJournal.weighed
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Weigh => (path, s.outcome)
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
          case s if s.name == Turn.Step.Route => (path, s.outcome)
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

    test(
      "every step of the epoch records within its family's budget, every history within the whole's"
    ) {
      val read = histories.collect { case (path, Right(h)) => path -> h }
      assert(read.size == histories.size)
      val family = (name: String) => Turn.Step.family(name).getOrElse(name)
      RecordedSizes.breaches(read, family, Budgets, Whole) ==> Vector.empty
    }

    test("every recorded offer is written back byte for byte") {
      // An offer is recorded data: one recorded before shapes reads as unshaped, and today's
      // codec writes the same text back, so recipes change no offer already recorded.
      val j = TurnJournal.recordedOffer
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Turn.Step.Offer => (path, s.outcome)
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
          case s if Turn.Step.family(s.name).contains(Turn.Step.Gate) => (path, s.outcome)
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
          case s if s.name == Turn.Step.Assemble => (path, s.outcome)
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
          _.steps.flatMap(s => kind(s.name).map((path, _, s.outcome)))
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
      // Agenda events are recorded data: today's codec must read them and write the same text.
      val j = TurnJournal.classification
      val outputs = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == "classify" => (path, s.outcome)
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
