package grit.knowledge.sync

import grit.core.visibility.Label
import grit.knowledge.drive.{DriveCardsTests, DriveListing}
import grit.knowledge.ref.CardKey

import utest.*

/** A walk's steps as [[Walking]] makes them, over a routine of its own: each task a path to list,
  * the listing's `listed` making the steps a test names.
  */
object WalkingTests extends TestSuite {

  private val source = DriveCardsTests.cards(Vector(DriveCardsTests.internal))
  private val other = Label.Public

  private def key(path: String): CardKey = CardKey.of(source.corpus, path)

  private final case class Path(path: String) extends caps.Pure
  private case object NoRecord extends caps.Pure

  private type W = Walking[DriveListing, Path, NoRecord.type]

  /** The walk listing each of `paths` in turn, its listing then given to `listed`. */
  private def walk(paths: List[String])(listed: W -> W): W =
    Walking.start(
      Walking.Routine[DriveListing, Path, NoRecord.type](
        source,
        (task, w) => w.list(task.path, Way.Run)((w, _, _) => listed(w)),
        (_, _) => false,
        _ => Vector.empty
      ),
      5,
      Holding(DriveCardsTests.internal, Vector.empty),
      paths.map(Path(_)),
      NoRecord
    )

  /** Every step `w` makes, each list answered with an empty listing and each keep kept. */
  private def run(w: Walk[DriveListing]): (Vector[Step], Synced) = {
    @annotation.tailrec
    def go(w: Walk[DriveListing], steps: Vector[Step]): (Vector[Step], Synced) = w.next match {
      case Left(synced) => (steps, synced)
      case Right(step) =>
        val outcome: Outcome[DriveListing] = step match {
          case Step.List(_, _) => Outcome.Listed(DriveListing(Vector.empty, true), other)
          case _ => Outcome.Kept
        }
        go(w.after(outcome), steps :+ step)
    }
    go(w, Vector.empty)
  }

  val tests = Tests {
    test("a step a listing's listed makes is the walk's next, before the tasks stacked") {
      val (steps, synced) = run(walk(List("a", "b")) { w =>
        if (w.tally.relabelled == 0) w.relabel(key("x"), other) else w
      })
      assert(
        steps.collect {
          case Step.List(path, _) => s"list $path"
          case Step.Relabel(k, _) => s"relabel ${CardKey.value(k) == CardKey.value(key("x"))}"
        } == Vector("list a", "relabel true", "list b"),
        synced.stopped.isEmpty
      )
    }

    test("a second step made before the first's outcome ends the walk, saying so") {
      val (steps, synced) = run(walk(List("a", "b")) { w =>
        w.relabel(key("x"), other).relabel(key("y"), other)
      })
      assert(
        steps.size == 1,
        synced.stopped == Some("a relabel was made while a relabel's outcome was awaited")
      )
    }
  }
}
