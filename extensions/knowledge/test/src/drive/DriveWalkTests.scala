package grit.knowledge.drive

import java.time.Instant

import grit.core.visibility.Label
import grit.knowledge.card.{CardSummary, UnitVersion}
import grit.knowledge.sync.{Fetched, Held, Outcome, Step, Synced, Way}

import utest.*

/** A Drive source's walk over a scripted Drive: the shared drives `drive-a` and `drive-b`;
  * `drive-a` (read at internal) holds the folder `folder-3` (read at confidential, its files
  * `file-text` and `file-1`), the Doc `file-18` and the PDF `file-4`; `drive-b` (read at
  * internal) holds the Sheet `file-30` and the folder `folder-31` (read at internal, its text
  * file `file-32` and the folder `folder-33`, one level too deep to list).
  */
object DriveWalkTests extends TestSuite {

  import DriveCardsTests.{cards, confidential, corpus, internal}
  import DriveEntry.Kind

  private def day(n: Int): Instant = Instant.parse(f"2026-09-$n%02dT10:00:00Z")

  private def entry(id: String, kind: Kind, changed: Int, within: String): DriveEntry =
    DriveEntry(id, kind, Some(s"name of $id"), Some(day(changed)), Some(within))

  private def drive(id: String): DriveEntry = DriveEntry(id, Kind.Drive, Some(id), None, None)

  /** Each listing's path, to its listing and the label it was read at. */
  private val world: Map[String, (DriveListing, Label)] = Map(
    DriveWalk.Drives -> (DriveListing(Vector(drive("drive-a"), drive("drive-b")), true), internal),
    "drive-a" -> (DriveListing(
      Vector(
        entry("folder-3", Kind.Folder, 1, "drive-a"),
        entry("file-18", Kind.Document, 5, "drive-a"),
        entry("file-4", Kind.Other("pdf"), 2, "drive-a")
      ),
      true
    ), internal),
    "folder-3" -> (DriveListing(
      Vector(
        entry("file-text", Kind.Text, 13, "folder-3"),
        entry("file-1", Kind.Document, 8, "folder-3")
      ),
      true
    ), confidential),
    "drive-b" -> (DriveListing(
      Vector(
        entry("file-30", Kind.Spreadsheet, 6, "drive-b"),
        entry("folder-31", Kind.Folder, 3, "drive-b")
      ),
      true
    ), internal),
    "folder-31" -> (DriveListing(
      Vector(
        entry("file-32", Kind.Text, 7, "folder-31"),
        entry("folder-33", Kind.Folder, 4, "folder-31")
      ),
      true
    ), internal)
  )

  private val summary =
    CardSummary.of("A file.", Vector.empty).fold(why => throw new java.lang.AssertionError(why), identity)

  /** Every step the walk makes, each answered from `drive`: a listing it holds, else withheld; a
    * read as `file {id}` at the listed change time; every keep made. `stop` stops the step it
    * names.
    */
  private def run(
      walk: DriveWalk,
      drive: Map[String, (DriveListing, Label)] = world,
      stop: Option[Step] = None
  ): (Vector[Step], Synced) = {
    val changed = drive.values.flatMap(_._1.entries).map(e => e.id -> e.changed).toMap
    @annotation.tailrec
    def go(w: DriveWalk, steps: Vector[Step]): (Vector[Step], Synced) = w.next match {
      case Left(synced) => (steps, synced)
      case Right(step) =>
        val outcome: Outcome[DriveListing] =
          if (stop.contains(step)) Outcome.Stopped("refused")
          else
            step match {
              case Step.List(path, _) =>
                drive.get(path).fold(Outcome.NotTaken("withheld"))((l, at) => Outcome.Listed(l, at))
              case Step.Read(id, _) =>
                changed.get(id).flatten match {
                  case Some(t) => Outcome.Fetched(Fetched.Text(s"file $id", UnitVersion.Changed(t)), internal)
                  case None => Outcome.NotTaken("no such file")
                }
              case Step.Summarise(_, _, _) => Outcome.Summarised(summary)
              case _ => Outcome.Kept
            }
        go(w.after(outcome), steps :+ step)
    }
    go(walk, Vector.empty)
  }

  private def fetches(steps: Vector[Step]): Vector[Step] = steps.collect {
    case s: Step.Fetching => s
  }

  private def dropped(steps: Vector[Step]): Vector[(String, Label)] =
    steps.collect { case Step.Drop(gone) => gone.map(h => (h.path, h.label)) }.flatten

  private def held(id: String, at: Label, changed: Int): Held =
    Held.of(corpus.name, id, at, UnitVersion.Changed(day(changed)), None)

  private val labels: Vector[Label] = Vector(internal, confidential)

  val tests = Tests {
    test("a first run lists the drives, each drive and the folders directly in it, and reads each Doc, Sheet and text file it cards through the listing naming it") {
      val (steps, synced) = run(cards(labels).walk(5, internal, Vector.empty))
      assert(
        fetches(steps) == Vector(
          Step.List(DriveWalk.Drives, Way.Run),
          Step.List("drive-a", Way.Through(DriveWalk.Drives)),
          Step.Read("file-18", Way.Through("drive-a")),
          Step.List("folder-3", Way.Through("drive-a")),
          Step.List("drive-b", Way.Through(DriveWalk.Drives)),
          Step.Read("file-30", Way.Through("drive-b")),
          Step.List("folder-31", Way.Through("drive-b")),
          Step.Read("file-32", Way.Through("folder-31"))
        )
      )
      assert(synced.written == 3)
    }

    test("a written card is titled with the file's name, at the time it last changed") {
      val (steps, _) = run(cards(labels).walk(5, internal, Vector.empty))
      val written = steps.collect { case Step.Write(card, _) => (card.path, card.title, card.version) }
      assert(written.headOption == Some(("file-18", Some("name of file-18"), UnitVersion.Changed(day(5)))))
    }

    test("only a file whose change time is not its card's at the floor is read") {
      val holding = Vector(held("file-18", internal, 5), held("file-30", internal, 1))
      val (steps, synced) = run(cards(labels).walk(5, internal, holding))
      val reads = fetches(steps).collect { case Step.Read(id, _) => id }
      assert(reads == Vector("file-30", "file-32"))
      assert(synced.unchanged == 1)
    }

    test("a listing carded by another run is not read from, and its files' cards at the floor are withdrawn") {
      val holding = Vector(held("file-text", internal, 13))
      val (steps, _) = run(cards(labels).walk(5, internal, holding))
      val reads = fetches(steps).collect { case Step.Read(id, _) => id }
      assert(!reads.contains("file-text"), !reads.contains("file-1"))
      assert(dropped(steps) == Vector(("file-text", internal)))
    }

    test("the run of a listing's label reads from it, and not from the listings below") {
      val (steps, _) = run(cards(labels).walk(5, confidential, Vector.empty))
      val reads = fetches(steps).collect { case Step.Read(id, w) => (id, w) }
      assert(
        reads == Vector(
          ("file-text", Way.Through("folder-3")),
          ("file-1", Way.Through("folder-3"))
        )
      )
    }

    test("a card no listing shows is withdrawn once every listing was made and complete") {
      val (steps, _) = run(cards(labels).walk(5, internal, Vector(held("file-gone", internal, 1))))
      assert(dropped(steps) == Vector(("file-gone", internal)))
    }

    test("after an incomplete or a withheld listing, no card is withdrawn for not being shown") {
      val gone = Vector(held("file-gone", internal, 1))
      val (listing, at) = world("drive-b")
      val cut = world.updated("drive-b", (listing.copy(complete = false), at))
      val (incomplete, _) = run(cards(labels).walk(5, internal, gone), cut)
      val (withheld, _) = run(cards(labels).walk(5, internal, gone), world.removed("folder-31"))
      assert(dropped(incomplete) == Vector.empty, dropped(withheld) == Vector.empty)
    }

    test("past the folders it may list, no card is withdrawn for not being shown") {
      val (steps, _) =
        run(cards(labels, maxUnits = 1).walk(5, internal, Vector(held("file-gone", internal, 1))))
      assert(!fetches(steps).contains(Step.List("folder-31", Way.Through("drive-b"))))
      assert(dropped(steps) == Vector.empty)
    }

    test("a card at a label no longer declared, whose carding is the floor, is withdrawn; one at a declared label is not") {
      val source = cards(Vector(Label.Public, confidential))
      val holding = Vector(held("file-old", internal, 1), held("file-low", Label.Public, 1))
      val (steps, _) = run(source.walk(5, confidential, holding), world.removed("drive-b"))
      assert(dropped(steps).contains(("file-old", internal)))
      assert(!dropped(steps).exists(_._1 == "file-low"))
    }

    test("a card held only at another label is replaced by the write, not dropped apart") {
      val holding = Vector(held("file-18", Label.Public, 5))
      val source = cards(Vector(Label.Public, internal))
      val (steps, _) = run(source.walk(5, internal, holding))
      val replacing = steps.collect { case Step.Write(card, r) if card.path == "file-18" => r }
      assert(replacing == Vector(Some(Label.Public)))
      assert(!dropped(steps).exists(_._1 == "file-18"))
    }

    test("past perRun summaries a changed file is left, and every listing is still made") {
      val (steps, synced) = run(cards(labels).walk(1, internal, Vector.empty))
      val lists = fetches(steps).collect { case Step.List(path, _) => path }
      assert(lists == Vector(DriveWalk.Drives, "drive-a", "folder-3", "drive-b", "folder-31"))
      assert(synced.written == 1, synced.left == 2)
    }

    test("a stopped step ends the walk at once, withdrawing nothing") {
      val stop = Step.List("drive-b", Way.Through(DriveWalk.Drives))
      val (steps, synced) =
        run(cards(labels).walk(5, internal, Vector(held("file-gone", internal, 1))), stop = Some(stop))
      assert(steps.lastOption == Some(stop), synced.stopped == Some("refused"))
    }
  }
}
