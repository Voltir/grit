package grit.job.replay

import grit.core.durable.{History, InMemoryDurable, RecordedSizes}
import grit.job.run.Run
import grit.job.run.RunFixtures.*
import grit.turn.Turn

import utest.*

/** The versioning gate for the run workflow (ADR 0004): every run history recorded under the
  * engine's current epoch, [[Turn.Epoch]], must replay under today's body. A step renamed,
  * reordered, dropped or given an output the old records cannot satisfy fails here, before it
  * strands a run in flight.
  */
object JobReplayTests extends TestSuite {

  private def histories: Vector[(String, Either[String, History])] = historiesOf(Turn.Epoch)

  /** The most bytes a step of each family may record in these histories, about twice the
    * most any records today, each with what it records. A step new to the run has none until
    * it is given one here.
    */
  private val Budgets: Map[String, Int] = Map(
    "read-slot" -> 448, // the slot read: its job, version, params and report
    "ask" -> 768, // an ask's reply as the move shaped it, and its calls' costs
    "ask:record" -> 64, // whether the ask's cost was recorded
    "call" -> 384, // what the call sent, or why nothing was
    "call:answer" -> 128, // the answer read, at its level
    "call:expire" -> 64, // the wait's state when given up
    "call:abandon" -> 64, // the wait's state when given up
    "keep" -> 64, // what was kept
    "DBOS.recv" -> 64, // an edge's ring
    "DBOS.sleep" -> 64, // when the wait ends
    "reply" -> 256 // the run's reply
  )

  /** The most bytes one history's steps may record in all, about twice the most today's do. */
  private val Whole = 1600

  private def historiesOf(epoch: String): Vector[(String, Either[String, History])] =
    History.recorded(epoch)

  /** Each history of `epoch`, by its file's name, as the names of its steps. */
  private def namesOf(epoch: String): Map[String, Vector[String]] =
    historiesOf(epoch).collect { case (path, Right(h)) => path -> h.steps.map(_.name) }.toMap

  /** The name the previous epoch's step `name` has in this one: a move's `move:{name}` named
    * for what it is, by the moves the recorder's jobs make (`a` and `j` ask, `c` calls, `note`
    * keeps).
    */
  private def renamed(name: String): String = {
    val Move = """move:([^:]+)(:.*)?""".r
    val kinds = Map("a" -> "ask", "j" -> "ask", "c" -> "call", "note" -> "keep")
    name match {
      case Move(move, phase) => s"${kinds.getOrElse(move, "move")}:$move${Option(phase).mkString}"
      case other => other
    }
  }

  /** Whether `history` made a move: a step `grit.act.moves.ActName` names. */
  private def moved(history: History): Boolean =
    history.steps.exists(s => Seq("ask:", "call:", "keep:").exists(s.name.startsWith))

  /** Whether `history` kept: it holds [[Noting]]'s keep, by the name it was recorded under,
    * which a step renamed since must still replay.
    */
  private def kept(history: History): Boolean = history.steps.exists(_.name == "keep:note")

  /** What [[probes]] at version 1 replies when `history` is replayed under today's body, or why
    * the replay failed: a move whose input's digest no longer matches the one recorded replies
    * `Diverged`, though every step still replays.
    */
  private def replies(history: History): Either[String, Option[String]] = {
    val seen = new Seen
    new InMemoryDurable()
      .replay(history.id, history.steps)(Run.body(new World().env(), jobsOf(probing(1, seen))))
      .map(_ => seen.reply)
  }

  /** The reply `history`'s run recorded. */
  private def recorded(history: History): Option[String] =
    history.steps.collectFirst {
      case InMemoryDurable.Step("reply", InMemoryDurable.Outcome.Output(o)) =>
        ujson.read(o).objOpt.flatMap(_.get("text")).flatMap(_.strOpt)
    }.flatten

  /** Each history `pick` takes that does not replay under `jobs`, and why. */
  private def failing(pick: History => Boolean, jobs: grit.core.job.Jobs): Vector[String] =
    histories.flatMap { case (path, parsed) =>
      val outcome = parsed.flatMap { history =>
        if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
        else if (history.workflow != "run") Left(s"a ${history.workflow} history")
        else if (!pick(history)) Right("not picked")
        else {
          val w = new World
          new InMemoryDurable().replay(history.id, history.steps)(Run.body(w.env(), jobs))
        }
      }
      outcome.left.toOption.map(why => s"$path: $why")
    }

  val tests = Tests {
    test(
      "each run history of the previous epoch, its moves renamed, is its scenario's in this one"
    ) {
      val (before, after) = (namesOf("2026-10-14"), namesOf(Turn.Epoch))
      (before.size, before.keySet == after.keySet) ==> (19, true)
      before.map((file, names) => file -> names.map(renamed)) ==> after
    }

    test("the current epoch has run histories to replay, redeployed ones and moves among them") {
      // Without them the gates below pass vacuously.
      val runs = histories.collect { case (path, Right(h)) if h.workflow == "run" => path }
      val expected = Vector(
        "run-replied-then-redeployed",
        "run-asked",
        "run-asked-json",
        "run-asked-judgment",
        "run-asked-uncleared",
        "run-called",
        "run-called-then-asked",
        "run-call-unserved",
        "run-call-unclaimed",
        "run-call-refused",
        "run-capped",
        "run-redeployed-before-moves",
        "run-moved-then-redeployed",
        "run-kept"
      ).map(_ + ".json")
      expected.filterNot(runs.contains) ==> Vector()
    }

    test(
      "every step of the epoch records within its family's budget, every history within the whole's"
    ) {
      val read = histories.collect { case (path, Right(h)) => path -> h }
      assert(read.size == histories.size)
      RecordedSizes.breaches(read, RecordedSizes.familyOf, Budgets, Whole) ==> Vector.empty
    }

    test("every run history that made no move replays under today's body, at a later version") {
      // An empty world: every recorded step reads its output back, and a later deploy's jobs
      // decide nothing a recorded step already did.
      failing(h => !moved(h), jobs(2)) ==> Vector()
    }

    test("every run history that asked or called replays under today's body, at its own version") {
      failing(h => moved(h) && !kept(h), jobsOf(probing(1))) ==> Vector()
    }

    test("every run history that kept replays under today's body, its keeping job at its version") {
      failing(kept, keepingJobs(new Noting(1))) ==> Vector()
    }

    test(
      "every run history that asked or called, and replied, replies under today's body as it recorded"
    ) {
      val replied = histories.collect {
        case (path, Right(h)) if moved(h) && !kept(h) && recorded(h).nonEmpty => (path, h)
      }
      // Every one that recorded a reply is compared: one whose reply no longer parses would
      // otherwise drop out of the gate unseen.
      replied.map(_._1).sorted ==> Vector(
        "run-asked",
        "run-asked-json",
        "run-asked-judgment",
        "run-asked-uncleared",
        "run-call-refused",
        "run-call-unclaimed",
        "run-call-unserved",
        "run-called",
        "run-called-then-asked",
        "run-capped"
      ).map(_ + ".json").sorted
      val differing = replied.collect {
        case (name, h) if replies(h) != Right(recorded(h)) =>
          s"$name: ${replies(h)} != ${recorded(h)}"
      }
      differing ==> Vector()
    }

    test("a run that recorded no move, resumed at another version, recorded superseded") {
      val replayed = histories.collect {
        case (path, Right(h)) if path == "run-redeployed-before-moves.json" =>
          new InMemoryDurable().replay(h.id, h.steps)(Run.body(new World().env(), jobs(2)))
      }
      replayed ==> Vector(Right("superseded: started at v1, its job at v2"))
    }

    test(
      "a run that made a move, resumed at another version, ends in error at its reply, which meets the move"
    ) {
      val replayed = histories.collect {
        case (path, Right(h)) if path == "run-moved-then-redeployed.json" =>
          new InMemoryDurable().replay(h.id, h.steps)(Run.body(new World().env(), jobs(2)))
      }
      replayed.map(_.left.map(_.replaceAll("workflow \\S+ ", "workflow "))) ==>
        Vector(Left("workflow step 1: ran 'reply', recorded 'ask:a'"))
    }
  }
}
