package grit.core.id

import utest.*

/** [[JobName]], [[ScheduleKey]] and [[ScheduleId]]: a job's and a schedule's names. */
object ScheduleIdsTests extends TestSuite {

  private def slot(index: Int): CallSlot =
    CallSlot.of(TurnRef(ConversationId("c1"), TurnSeq(3)), 0, index) match {
      case Some(s) => s
      case None => throw new java.lang.AssertionError("a call slot with non-negative indices")
    }

  private def name(text: String): PluginName =
    PluginName.of(text).fold(e => throw new java.lang.AssertionError(e), identity)

  private def key(text: String): ScheduleKey =
    ScheduleKey.of(text).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test(
      "a job's name and a schedule's key are lowercase letters, digits and dashes, from a letter"
    ) {
      val good = Vector("remind", "daily-sync", "a2")
      val bad = Vector("", "Remind", "2a", "-a", "a:b", "a/b", "a b")
      good.map(JobName.of(_).map(JobName.value)) ==> good.map(Right(_))
      good.map(ScheduleKey.of(_).map(ScheduleKey.value)) ==> good.map(Right(_))
      bad.map(JobName.of) ==> bad.map(_ =>
        Left("a job's name is lowercase letters, digits and dashes, starting with a letter")
      )
      bad.map(ScheduleKey.of) ==> bad.map(_ =>
        Left("a schedule's key is lowercase letters, digits and dashes, starting with a letter")
      )
    }

    // Pins of the stored form: a schedule row's id and every slot's key hold it. The hashes are
    // `printf 'tool:c1:3:0:1' | sha256sum`'s first 16 hex digits, computed outside the code.
    test("an asked schedule's id is its call's key hashed; one call, one id") {
      ScheduleId.value(ScheduleId.asked(slot(1))) ==> "asked:705dcb72ad698d36"
      ScheduleId.value(ScheduleId.asked(slot(2))) ==> "asked:574ba3e77a498889"
    }

    test("a declared schedule's id names its declarer and key") {
      ScheduleId.value(ScheduleId.declared(Declarer.Deployment, key("standup"))) ==>
        "declared:deployment:standup"
      ScheduleId.value(ScheduleId.declared(Declarer.Plugin(name("digest")), key("weekly"))) ==>
        "declared:plugin:digest:weekly"
    }

    test("every id an id is written as reads back to it") {
      val ids = Vector(
        ScheduleId.asked(slot(1)),
        ScheduleId.declared(Declarer.Deployment, key("standup")),
        ScheduleId.declared(Declarer.Plugin(name("digest")), key("weekly"))
      )
      ids.map(id => ScheduleId.of(ScheduleId.value(id))) ==> ids.map(Right(_))
    }

    test("text in neither form is no schedule's id") {
      val notIds = Vector(
        "",
        "asked:",
        "asked:705dcb72ad698d3", // 15 digits
        "asked:705DCB72AD698D36",
        "asked:705dcb72ad698d36:x",
        "declared:deployment:",
        "declared:deployment:Standup",
        "declared:plugin:digest",
        "declared:plugin:Digest:weekly",
        "declared:plugin:digest:weekly:x",
        "declared:someone:standup",
        "tool:c1:3:0:1"
      )
      notIds.map(ScheduleId.of) ==> notIds.map(t => Left(s"$t is no schedule's id"))
    }
  }
}
