package grit.core.job

import java.time.Instant

import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  JobName,
  ScheduleId,
  ScheduleKey,
  SourceId,
  TurnRef,
  TurnSeq
}
import grit.core.message.Message
import grit.core.store.Origin

import utest.*

/** [[Slot]]: one slot of a schedule, its run's conversation and its opening. */
object SlotTests extends TestSuite {

  private def at(text: String): Instant = Instant.parse(text)
  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Remind = right(JobName.of("remind"))
  private val Standup = ScheduleId.declared(Declarer.Deployment, right(ScheduleKey.of("standup")))
  private val Asked = ScheduleId.asked(
    CallSlot
      .of(TurnRef(ConversationId("c1"), TurnSeq(3)), 0, 1)
      .getOrElse(throw new java.lang.AssertionError("a call slot"))
  )

  val tests = Tests {
    // A pin of the stored form: a run's conversation is found by its origin, whose run name
    // this is, so a changed form would start every slot again.
    test(
      "a slot's key is its schedule's id and its instant, ISO-8601, and its run's place holds it"
    ) {
      val slot = Slot(Asked, at("2026-10-07T09:00:00Z"))
      slot.key ==> "asked:705dcb72ad698d36@2026-10-07T09:00:00Z"
      slot.origin(Remind) ==> Origin.Task("remind", "asked:705dcb72ad698d36@2026-10-07T09:00:00Z")
      slot
        .origin(Remind)
        .place
        .written ==> "task:remind/asked:705dcb72ad698d36@2026-10-07T09:00:00Z"
      Slot(Standup, at("2026-10-07T09:00:00.500Z")).key ==>
        "declared:deployment:standup@2026-10-07T09:00:00.500Z"
    }

    test("every slot's key reads back to it") {
      val slots = Vector(
        Slot(Asked, at("2026-10-07T09:00:00Z")),
        Slot(Standup, at("2026-10-07T09:00:00.500Z"))
      )
      slots.map(s => Slot.read(s.key)) ==> slots.map(Some(_))
    }

    test("a run name in no slot's exact form is no slot") {
      val notSlots = Vector(
        "main",
        "declared:deployment:standup",
        "declared:deployment:standup@",
        "x@2026-10-07T09:00:00Z",
        "declared:deployment:standup@not-a-time",
        "declared:deployment:standup@2026-10-07T09:00Z",
        "declared:deployment:standup@2026-10-07T09:00:00.000Z",
        "declared:deployment:standup@1791363600000"
      )
      notSlots.map(Slot.read) ==> notSlots.map(_ => None)
    }

    test("a run's opening names its job and its slot, in UTC to the minute") {
      Slot(Asked, at("2026-10-07T09:00:59Z")).opening(Remind) ==>
        Message.User("Scheduled run of remind, due 2026-10-07 09:00 UTC")
    }

    // A pin of the stored form: a run's opening is kept under this source id, and a run reads
    // the version it was started at back from it.
    test("a run's opening source is v{version}, and reads back; nothing else does") {
      SourceId.value(Slot.source(3)) ==> "v3"
      val versions = Vector(0, 3, 12, -1)
      versions.map(v => Slot.version(Slot.source(v))) ==> versions.map(Some(_))
      val others = Vector("3", "v", "v03", "v-0", "vx", "v+3", "v 3", "V3")
      others.map(s => Slot.version(SourceId(s))) ==> others.map(_ => None)
    }
  }
}
