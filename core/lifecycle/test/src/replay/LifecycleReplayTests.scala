package grit.lifecycle.replay

import grit.core.durable.{History, InMemoryDurable}
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
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }

    test(
      "every recorded shadow row of the epoch reads, and the epoch holds a wording's and a question set's"
    ) {
      // A shadow in flight reads back its ask step: today's reader must read both forms.
      val rows = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector
          .filter(_.workflow == "shadow")
          .flatMap(_.steps.collect {
            case s if s.name == "ask" => (path.last, s.outcome)
          })
      }
      val read = rows.map {
        case (file, InMemoryDurable.Outcome.Output(text)) =>
          file -> ujson.read(text).obj.get("ok").flatMap(_.obj.get("row")).map(ShadowedJson.read)
        case (file, other) => file -> Some(Left(s"not an output: $other"))
      }
      read.collect { case (file, Some(Left(why))) => s"$file: $why" } ==> Vector.empty
      val answered = read.collect { case (_, Some(Right(Shadowed.Answered(_, a, _, _, _, _)))) =>
        a match {
          case ShadowAnswers.Worded(_) => "worded"
          case ShadowAnswers.Named(_) => "named"
        }
      }
      answered.distinct.sorted ==> Vector("named", "worded")
    }

    test(
      "the epoch holds a triage tagged with v1's four probabilities, one with named answers, and one held unasked"
    ) {
      // A triage in flight across a deploy reads back its ask: replay above reads every form
      // recorded; these say both forms are recorded, and the in-flight one's hold.
      val triages = histories.flatMap { case (_, parsed) =>
        parsed.toOption.toVector.filter(_.workflow == "triage").flatMap(_.steps)
      }
      def output(name: String) = triages.collect {
        case s if s.name == name =>
          s.outcome match {
            case InMemoryDurable.Outcome.Output(text) => ujson.read(text).obj.get("ok")
            case _ => None
          }
      }.flatten
      val tags = output("ask").flatMap(_.objOpt).flatMap(_.get("tags")).flatMap(_.objOpt)
      val held = output("consider").flatMap(_.objOpt).flatMap(_.get("held")).flatMap(_.objOpt)
      (
        tags.exists(_.contains("kind")),
        tags.exists(_.contains("answers")),
        held.exists(_.get("kind").exists(_.strOpt.contains("unasked")))
      ) ==> (true, true, true)
    }

    test("every recorded closing of the epoch reads, and the epoch holds a version-3 one") {
      // A close in flight reads back its summarise step: today's reader must read every
      // version this epoch recorded (ADR 0018). An earlier epoch's closes are never resumed;
      // a stored closing of an earlier version is ClosingTests'.
      val closings = histories.flatMap { case (path, parsed) =>
        parsed.toOption.toVector.flatMap(_.steps.collect {
          case s if s.name == "summarise" => (path.last, s.outcome)
        })
      }
      val read = closings.map {
        case (file, InMemoryDurable.Outcome.Output(text)) =>
          val closing = ujson.read(text).obj.get("closing")
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
