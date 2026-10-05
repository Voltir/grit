package grit.eval.harness.reply

import grit.core.context.{AssemblyNote, Window}
import grit.core.id.{ConversationId, DocumentVersion, EntrySeq, TurnSeq, WorkflowId}
import grit.core.place.Place
import grit.core.store.Nearby

import utest.*

/** A rebuilt window against the recorded one, by ids. */
object DriftTests extends TestSuite {

  private val place = Place.read("task:rebuild").fold(sys.error(_), identity)
  private def seqs(xs: Long*) = xs.toVector.map(EntrySeq(_))
  private def open(c: String, xs: Long*) = Nearby.Open(ConversationId(c), place, seqs(xs*))

  private val recorded =
    Window(seqs(1, 2, 3), Vector(AssemblyNote.Recalled(Vector(TurnSeq(0)))), Vector(open("a", 4)))

  val tests = Tests {
    test("the same ids are the same window, whatever its notes") {
      Drift.of(recorded, recorded.copy(notes = Vector.empty), lost = false) ==> Drift.Same
    }

    test("a section from elsewhere changed, added or reordered is nearby drift") {
      Drift.of(recorded, recorded.copy(nearby = Vector(open("a", 4, 5))), lost = false) ==>
        Drift.Nearby
      val two = recorded.copy(nearby = Vector(open("a", 4), open("b", 1)))
      Drift.of(recorded, two, lost = false) ==> Drift.Nearby
      Drift.of(two, two.copy(nearby = two.nearby.reverse), lost = false) ==> Drift.Nearby
    }

    test("a document changed, added or reordered is nearby drift") {
      def held(vs: Long*) = vs.toVector.flatMap(DocumentVersion.of)
      val documented = recorded.copy(documents = held(7))
      Drift.of(documented, documented.copy(documents = held(8)), lost = false) ==> Drift.Nearby
      Drift.of(documented, documented.copy(documents = held(7, 8)), lost = false) ==> Drift.Nearby
      val two = recorded.copy(documents = held(7, 8))
      Drift.of(two, two.copy(documents = held(8, 7)), lost = false) ==> Drift.Nearby
    }

    test("own entries changed is own drift; both changed is both") {
      Drift.of(recorded, recorded.copy(entries = seqs(2, 3)), lost = false) ==> Drift.Own
      Drift.of(recorded, Window(seqs(1, 3), Vector.empty, Vector.empty), lost = false) ==>
        Drift.Both
    }

    test("a rebuild's report counts each drift and names each turn that drifted or failed") {
      val at = java.time.Instant.parse("2026-10-03T12:00:00Z")
      def turn(w: String, r: Either[String, RebuiltWindow]) = WorkflowId(w) -> r
      // Rebuilt as `as`, by default the window every test here records.
      def rebuilt(kept: Option[Window], lost: Boolean = false, as: Window = recorded) =
        Right(RebuiltWindow(at, kept, as, lost))
      Rebuild.report(
        Vector(
          turn("c:1", rebuilt(Some(recorded))),
          turn("c:2", rebuilt(Some(recorded), as = recorded.copy(nearby = Vector.empty))),
          turn("c:3", rebuilt(Some(recorded), lost = true)),
          turn("c:4", rebuilt(None, as = recorded)),
          turn("c:5", Left("c:5 recorded no assemble step"))
        )
      ) ==> Vector(
        "turns: 5; rebuilt 4, not rebuilt 1",
        "  drift: same 1, nearby changed 1, own changed 0, both 0, gone 1, no window recorded 1",
        "  c:2: nearby",
        "  c:3: gone",
        "  c:5: c:5 recorded no assemble step"
      )
    }

    test("a recorded entry the database lost is gone, before any other drift") {
      Drift.of(recorded, recorded, lost = true) ==> Drift.Gone
      Drift.of(recorded, recorded.copy(entries = seqs(2, 3)), lost = true) ==> Drift.Gone
    }
  }
}
