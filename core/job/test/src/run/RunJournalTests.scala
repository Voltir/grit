package grit.job.run

import java.time.Instant

import grit.core.id.{EdgeName, JobName, ScheduleId}
import grit.core.job.{Destination, Report, Slot}

import utest.*

/** The run's recorded step outputs, pinned: a run in flight reads back what an earlier build
  * wrote.
  */
object RunJournalTests extends TestSuite {

  private val slot = Slot(
    ScheduleId.of("asked:00112233445566ff").fold(sys.error, identity),
    Instant.parse("2026-10-07T09:00:00Z")
  )

  private val remind = JobName.of("remind").fold(sys.error, identity)

  private val reads: Vector[(SlotRead, String)] = Vector(
    SlotRead.Read(
      slot,
      remind,
      2,
      Instant.parse("2026-10-07T09:00:05.5Z"),
      ujson.Obj("text" -> "stretch"),
      Report.Posted(Destination(EdgeName.Slack, "C1/1.0"))
    ) -> """{"kind":"read","slot":"asked:00112233445566ff@2026-10-07T09:00:00Z","job":"remind","version":2,"started":"2026-10-07T09:00:05.500Z","params":{"text":"stretch"},"report":{"kind":"posted","edge":"slack","address":"C1/1.0"}}""",
    SlotRead.Read(slot, remind, 1, slot.nominal, ujson.Num(3), Report.Kept) ->
      """{"kind":"read","slot":"asked:00112233445566ff@2026-10-07T09:00:00Z","job":"remind","version":1,"started":"2026-10-07T09:00:00Z","params":3,"report":{"kind":"kept"}}""",
    SlotRead.Unreadable("its schedule is gone") ->
      """{"kind":"unreadable","why":"its schedule is gone"}"""
  )

  private val ends: Vector[(RunEnd, String)] = Vector(
    RunEnd.Replied("stretch") -> """{"kind":"replied","text":"stretch"}""",
    RunEnd.Superseded(3) -> """{"kind":"superseded","current":3}""",
    RunEnd.Jobless -> """{"kind":"jobless"}""",
    RunEnd.Failed("down") -> """{"kind":"failed","why":"down"}"""
  )

  val tests = Tests {
    test("a read-slot output is recorded in its pinned form, and read back as it was") {
      reads.map((r, _) => RunJournal.slotRead.encode(r)) ==> reads.map(_._2)
      reads.map((_, text) => RunJournal.slotRead.decode(text)) ==> reads.map(r => Right(r._1))
    }

    test("a reply output is recorded in its pinned form, and read back as it was") {
      ends.map((e, _) => RunJournal.runEnd.encode(e)) ==> ends.map(_._2)
      ends.map((_, text) => RunJournal.runEnd.decode(text)) ==> ends.map(e => Right(e._1))
    }

    test("an output of no kind, or naming no slot, reads as why it is none") {
      Vector(
        RunJournal.slotRead.decode("""{"kind":"later"}"""),
        RunJournal.slotRead.decode(
          """{"kind":"read","slot":"main","job":"remind","version":1,"started":"2026-10-07T09:00:00Z","params":3,"report":{"kind":"kept"}}"""
        ),
        RunJournal.runEnd.decode("""{"kind":"superseded","current":1.5}""")
      ) ==> Vector(
        Left("read-slot: no kind later"),
        Left("read-slot: no slot main"),
        Left("expected a whole number 'current'")
      )
    }
  }
}
