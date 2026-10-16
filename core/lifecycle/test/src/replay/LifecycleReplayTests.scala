package grit.lifecycle.replay

import grit.core.durable.{History, InMemoryDurable, RecordedSizes}
import grit.core.period.ClosingJson
import grit.core.triage.{ShadowAnswers, Shadowed, ShadowedJson}
import grit.lifecycle.close.CloseFixtures
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.SettleFixtures
import grit.lifecycle.shadow.ShadowFixtures
import grit.lifecycle.triage.TriageFixtures
import grit.turn.Turn

import utest.*

/** The versioning gate for the close, settle, posting, triage, placement and shadow workflows (ADR 0004): every history of
  * either recorded under the engine's current epoch, [[Turn.Epoch]], must replay under
  * today's body. A step renamed, reordered, dropped or given an output the old records
  * cannot satisfy fails here, before it strands a workflow in flight.
  */
object LifecycleReplayTests extends TestSuite {
  import CloseFixtures.*
  import RecordLifecycleHistories.{Posted, postWorld}

  private def histories: Vector[(String, Either[String, History])] = historiesOf(Turn.Epoch)

  /** A close's summary step: its judgment's `made` holds what it wrote. */
  private val Summary = "ask:summary"

  /** The closing a close's recorded `ask:summary` output `text` holds, as recorded. */
  private def closingIn(text: String): Option[ujson.Value] =
    ujson
      .read(text)
      .obj
      .get("made")
      .flatMap(_.strOpt)
      .flatMap(made => ujson.read(made).obj.get("closing"))

  /** The most bytes a step of each family may record in these histories, about twice the
    * most any records today, each with what it records. A step new to these workflows has
    * none until it is given one here.
    */
  private val Budgets: Map[String, Int] = Map(
    "check" -> 512, // what is due, and close's known record of the conversation
    "ask:summary" -> 1900, // the closing close drew up, whole, escaped in its judgment
    "seal" -> 64, // the closing's id
    // triage's, settle's and shadow's answers, with their weights, which of close's
    // sections its gate let through, and why, and a placement with what it was shown; each
    // with its call's cost
    "ask" -> 1100,
    "ask:record" -> 64, // whether an ask's classifier cost was recorded
    "record" -> 64, // whether the answers were kept
    "consider" -> 448, // what triage decided of a heard message, and the gate's readings
    "start" -> 64, // the turn triage started, or why not
    "unstarted" -> 64, // whether triage's unstarted row was kept
    "stitched" -> 64, // what a placement did, in a line
    "record-stitch" -> 64, // whether the placement was kept
    "post" -> 64 // what the post came to
  )

  /** What a judgment's `ask:{name}` step, recorded as `text`, made, as its workflow writes it. */
  private def made(text: String): ujson.Value =
    ujson.read(text).obj.get("made").flatMap(_.strOpt).fold(ujson.Null: ujson.Value)(ujson.read(_))

  /** The most bytes one history's steps may record in all, about twice the most today's do. */
  private val Whole = 2000

  private def historiesOf(epoch: String): Vector[(String, Either[String, History])] =
    History.recorded(epoch)

  val tests = Tests {
    test(
      "each lifecycle history of the previous epoch names its steps as its scenario's in this one"
    ) {
      def namesOf(epoch: String)(named: History => Vector[String]): Map[String, Vector[String]] =
        historiesOf(epoch).collect { case (p, Right(h)) => p -> named(h) }.toMap
      // A settle's, triage's or shadow's `ask` is the move `ask:{workflow}`, a placement's
      // `stitch` is `ask:stitch`, and a close's `gate` and `summarise` are `ask:gate` and
      // `ask:summary`, each one's cost then
      // recorded in its `:record` step when the classifier replied: a reply names its model,
      // and a gate's leaves no note. A close unearned or ran asks no gate, and records no step
      // for it.
      def renamed(h: History): Vector[String] = h.steps.flatMap { s =>
        val out = s.outcome.toString
        (h.workflow, s.name) match {
          case ("close", "gate") if out.contains("\"unearned\"") || out.contains("\"ran\"") =>
            Vector()
          case ("close", "gate") if out.contains("\"note\"") => Vector("ask:gate")
          case ("close", "gate") => Vector("ask:gate", "ask:gate:record")
          // A close's `summarise` is `ask:summary`, its cost recorded when a model replied.
          case ("close", "summarise") if out.contains("\"cost\"") =>
            Vector("ask:summary", "ask:summary:record")
          case ("close", "summarise") => Vector("ask:summary")
          case (w @ ("settle" | "triage" | "shadow"), "ask") if out.contains("\"model\"") =>
            Vector(s"ask:$w", s"ask:$w:record")
          case (w @ ("settle" | "triage" | "shadow"), "ask") => Vector(s"ask:$w")
          case ("stitch", "stitch") if out.contains("\"model\"") =>
            Vector("ask:stitch", "ask:stitch:record")
          case ("stitch", "stitch") => Vector("ask:stitch")
          case (_, name) => Vector(name)
        }
      }
      val before = namesOf("2026-10-14")(renamed)
      val after = namesOf(Turn.Epoch)(_.steps.map(_.name))
      (before.size, before == after) ==> (33, true)
    }

    test(
      "the current epoch has close, settle, posting, triage, placement and shadow histories to replay"
    ) {
      // Without them the gate below passes vacuously.
      val workflows = histories.flatMap(_._2.toOption.map(_.workflow)).toSet
      assert(
        workflows.contains("close"),
        workflows.contains("settle"),
        workflows.contains("post"),
        workflows.contains("triage"),
        workflows.contains("stitch"),
        workflows.contains("shadow")
      )
    }

    test(
      "every step of the epoch records within its family's budget, every history within the whole's"
    ) {
      val read = histories.collect { case (path, Right(h)) => path -> h }
      assert(read.size == histories.size)
      // A closing is not answers: the close's summary is its own family.
      def family(name: String): String =
        if (name == Summary) name else RecordedSizes.familyOf(name)
      RecordedSizes.breaches(read, family, Budgets, Whole) ==> Vector.empty
    }

    test(
      "every close, settle, posting, triage, placement and shadow history of the current epoch replays under today's body"
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
                    PostEnv(
                      w.periods,
                      w.plugins.cursors,
                      w.plugins.posting,
                      new grit.core.document.InMemoryDocuments().keeper,
                      new grit.core.store.InMemoryTombstones,
                      new FakeJot,
                      new CloseFixtures.SetClock(java.time.Instant.EPOCH)
                    )
                  )
                )
              // Speaking off: a history recorded before `consider` runs it past its end, and
              // keeps nothing; one recorded after reads its decision and start back.
              case "triage" =>
                new InMemoryDurable().replay(history.id, history.steps)(
                  new TriageFixtures.World()
                    .body(new TriageFixtures.Scripted(Vector.empty, Vector.empty), 0)
                )
              // An empty world: a history's stitch is read back, and its record replays its
              // output.
              case "stitch" =>
                new InMemoryDurable().replay(history.id, history.steps)(
                  new TriageFixtures.World()
                    .placement(new TriageFixtures.Scripted(Vector.empty, Vector.empty), 0)
                )
              // An empty world: a history's ask is read back, and its record replays its output.
              case "shadow" =>
                new InMemoryDurable().replay(history.id, history.steps)(
                  new ShadowFixtures.World()
                    .body(new TriageFixtures.Scripted(Vector.empty, Vector.empty), 0)
                )
              case other => Left(s"a $other history")
            }
        }
        outcome.left.toOption.map(why => s"$path: $why")
      }
      assert(failures.isEmpty)
    }

    test("every recorded shadow row of the epoch reads, and the epoch holds a question set's") {
      // A shadow in flight reads back its ask step: today's reader must read what this epoch
      // records. A shadow asks a question set; a stored wording's row is ShadowedJsonTests'.
      val rows = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector
          .filter(_.workflow == "shadow")
          .flatMap(_.steps.collect {
            case s if s.name == "ask:shadow" => (path, s.outcome)
          })
      }
      val read = rows.map {
        case (file, InMemoryDurable.Outcome.Output(text)) =>
          file -> made(text).objOpt
            .flatMap(_.get("ok"))
            .flatMap(_.obj.get("row"))
            .map(ShadowedJson.read)
        case (file, other) => file -> Some(Left(s"not an output: $other"))
      }
      read.collect { case (file, Some(Left(why))) => s"$file: $why" } ==> Vector.empty
      val answered = read.collect { case (_, Some(Right(Shadowed.Answered(_, a, _, _, _, _)))) =>
        a match {
          case ShadowAnswers.Worded(_) => "worded"
          case ShadowAnswers.Named(_) => "named"
        }
      }
      answered.distinct.sorted ==> Vector("named")
    }

    test(
      "the epoch holds a triage tagged with named answers, one with a source's, one drafting and one held at the gate"
    ) {
      // A triage in flight across a deploy reads back its ask and its decision: replay above
      // reads every form recorded; these say each is recorded. Stored tags of an earlier set
      // are TagsJsonTests'.
      val triages = histories.flatMap { case (_, parsed) =>
        parsed.toOption.toVector.filter(_.workflow == "triage").flatMap(_.steps)
      }
      def output(name: String, read: String => ujson.Value) = triages.collect {
        case s if s.name == name =>
          s.outcome match {
            case InMemoryDurable.Outcome.Output(text) => read(text).objOpt.flatMap(_.get("ok"))
            case _ => None
          }
      }.flatten
      val named = output("ask:triage", made)
        .flatMap(_.objOpt)
        .flatMap(_.get("tags"))
        .flatMap(_.objOpt)
        .flatMap(_.get("answers"))
        .map(_.arr.flatMap(_.obj.get("name")).flatMap(_.strOpt).toVector)
      val considered = output("consider", ujson.read(_)).flatMap(_.objOpt)
      (
        named.exists(_.contains("gap")),
        named.exists(_.exists(_.startsWith("source:"))),
        considered.exists(_.contains("drafting")),
        considered
          .flatMap(_.get("held"))
          .flatMap(_.objOpt)
          .exists(_.get("kind").exists(_.strOpt.contains("gated")))
      ) ==> (true, true, true, true)
    }

    test("the epoch holds a job's run's close, replied and unreplied") {
      // Without them the gate above never replays a close checked due as a run.
      val ran = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector
          .filter(h =>
            h.steps.exists(s =>
              s.name == "check" && (s.outcome match {
                case InMemoryDurable.Outcome.Output(text) =>
                  ujson.read(text).obj.get("ok").flatMap(_.obj.get("due")).flatMap(_.strOpt) ==
                    Some("ran")
                case _ => false
              })
            )
          )
          .flatMap(_.steps.collect {
            case s if s.name == Summary =>
              s.outcome match {
                case InMemoryDurable.Outcome.Output(text) =>
                  closingIn(text).flatMap(ClosingJson.read(_).toOption)
                case _ => None
              }
          })
          .flatten
          .map(c => path -> c.flows.outcome)
      }
      ran.sortBy(_._1) ==> Vector(
        "close-ran-unreplied.json" -> Some("No reply."),
        "close-ran.json" -> Some("Stand up: 3 open pull requests.")
      )
    }

    test("every recorded closing of the epoch reads, and the epoch holds a version-3 one") {
      // A close in flight reads back its summary step: today's reader must read every
      // version this epoch recorded (ADR 0018). An earlier epoch's closes are never resumed;
      // a stored closing of an earlier version is ClosingTests'.
      val closings = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == Summary => (path, s.outcome)
        })
      }
      val read = closings.map {
        case (file, InMemoryDurable.Outcome.Output(text)) =>
          val closing = closingIn(text)
          val version = closing.flatMap(_.obj.get("v")).flatMap(_.numOpt).map(_.toInt)
          file -> closing.map(ClosingJson.read).map(_.map(c => (version, c)))
        case (file, other) => file -> Some(Left(s"not an output: $other"))
      }
      read.collect { case (file, Some(Left(why))) => s"$file: $why" } ==> Vector.empty
      // The epoch holds a closing written as version 3, grounds and all.
      assert(read.exists { case (_, Some(Right((Some(3), _)))) => true; case _ => false })
    }
  }
}
