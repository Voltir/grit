package grit.remind

import java.time.Instant

import scala.util.chaining.*

import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.id.{
  CallSlot,
  ConversationId,
  PluginName,
  PrincipalId,
  ScheduleId,
  TestCallSlots,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.job.{
  Destination,
  InMemorySchedules,
  JobRun,
  NotOwn,
  OwnJobs,
  Report,
  ScheduleDesk,
  SlotRule
}
import grit.core.message.AssistantBlock
import grit.core.plugin.{InMemoryPlugins, Needs, PluginReads, PluginTool}
import grit.core.store.{Db, Origin, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, Toolbox}
import grit.dbos.sql.TestTx

import utest.*

object RemindersTests extends TestSuite {

  private val name = PluginName.of("remind").getOrElse(throw new java.lang.AssertionError("name"))

  private val now = Instant.parse("2026-10-06T14:03:12Z")

  private val thread = Origin.Slack("T1", "eng", "1700.1")

  private val asker = PrincipalId("U1")

  /** The turn `n` of the asking thread's conversation. */
  private def turn(n: Int): TurnRef = TurnRef(ConversationId(s"slack:$n"), TurnSeq.First)

  final class FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  /** The reminders' schedules as of [[now]], and their desk, holding `remind`. */
  private final class Desk {
    val schedules = new InMemorySchedules()
    val clock = new SetClock(now)
    val desk: ScheduleDesk^ = schedules.desk(name, Vector(Reminders.Remind.name), clock)

    /** The first call of `turn(n)`, whose reply is posted in the thread at `address`, or
      * nowhere, its message `by`'s.
      */
    def asked(n: Int, address: Option[String] = Some("1700.1"), by: PrincipalId = asker): CallSlot =
      TestCallSlots.at(turn(n)).tap(_ => schedules.asking(turn(n), thread, by, address))

    /** What `tool` comes to, called at `at` with `args`: a call whose arguments do not read
      * comes to what the model is told of it.
      */
    def run[A](tool: PluginTool[A], at: CallSlot, args: (String, ujson.Value)*): Outcome = {
      val db = new FakeDb
      val d = desk
      tool
        .bind(reads, Needs.over(name, Vector.empty), OwnJobs.over(name, Vector(Reminders.Remind)))
        .fold(u => sys.error(u.toString), identity)
        .pipe(r =>
          Toolbox.of[caps.CapSet^{db, d}](tool.described.calling((a, c) => r.run(a, c, db, d)))
        )
        .fold(e => sys.error(e.toString), identity)
        .bind(
          AssistantBlock.ToolCall(ToolCallId("c1"), ToolNameOf(tool), ujson.Obj.from(args)),
          Repairs.All
        ) match {
        case Right(free: Bound.Free) => free(at)
        case Right(other) => sys.error(s"not free: $other")
        case Left(e) => e.outcome
      }
    }
  }

  private def ToolNameOf(tool: PluginTool[?]): String =
    grit.core.tool.ToolName.value(tool.described.name)

  private val reads: PluginReads =
    PluginReads(new InMemoryPlugins().docs(name), new InMemoryDocuments().shelf(name))

  /** `remind_me`'s arguments `args`, read, or what the model is told of why not. */
  private def readMe(args: (String, ujson.Value)*): Either[String, (String, String)] =
    Reminders.RemindMe.described.spec.args
      .read(ujson.Obj.from(args))
      .map(a => (a.reminder.text, a.when.toString))
      .left
      .map(_.message)

  private def id(n: Int): ScheduleId = ScheduleId.asked(TestCallSlots.at(turn(n)))

  private def reminder(text: String): Reminder =
    Reminder.of(text).getOrElse(throw new java.lang.AssertionError(text))

  private val due = Instant.parse("2026-10-07T09:00:00Z")

  /** `remind`'s reply to a run of `text` due at [[due]], started `late` seconds after it. */
  private def replied(text: String, late: Long): String =
    Reminders.Remind.reply(JobRun(reminder(text), due, due.plusSeconds(late)))

  val tests = Tests {
    test("a reminder's text is refused blank or longer than 500 characters, and kept as given") {
      (
        Reminder.of("").isLeft,
        Reminder.of(" \n ").isLeft,
        Reminder.of("x" * 501).isLeft,
        Reminder.of("x" * 500).map(_.text),
        Reminder.of(" buy milk ").map(_.text)
      ) ==> (true, true, true, Right("x" * 500), Right(" buy milk "))
    }

    test("remind reads back the parameters it writes") {
      val r = reminder("call the plumber")
      Reminders.Remind.read(Reminders.Remind.write(r)) ==> Right(r)
    }

    test("remind refuses parameters with no text, or text no reminder holds") {
      (
        Reminders.Remind.read(ujson.Obj()).isLeft,
        Reminders.Remind.read(ujson.Obj("text" -> 3)).isLeft,
        Reminders.Remind.read(ujson.Obj("text" -> "")).isLeft,
        Reminders.Remind.read(ujson.Str("call")).isLeft
      ) ==> (true, true, true, true)
    }

    test("remind replies with the reminder's text when run within a minute of its time") {
      (replied("call the plumber", 0), replied("call the plumber", 60)) ==>
        ("Reminder: call the plumber", "Reminder: call the plumber")
    }

    test("remind run more than a minute late says when it was due, to the minute in UTC") {
      replied("call the plumber", 61) ==>
        "Reminder: call the plumber\n(due 2026-10-07 09:00 UTC; sent late)"
    }

    test("remind_me in_minutes sets a reminder that many minutes from now, posted in its thread") {
      val d = new Desk
      val call = d.asked(1)
      d.run(Reminders.RemindMe, call, "text" -> "stretch", "in_minutes" -> 30) ==>
        Outcome.Done(s"Reminder ${ScheduleId.value(id(1))} is set for 2026-10-06 14:33:12 UTC.")
      d.schedules
        .read(id(1))(using TestTx.fake)
        .map(
          _.map(s => (s.params, s.principal, s.report, s.rule))
        ) ==> Right(
        Some(
          (
            ujson.Obj("text" -> "stretch"),
            asker,
            Report.Posted(Destination(thread.edge, "1700.1")),
            SlotRule.Once(Instant.parse("2026-10-06T14:33:12Z"), Reminders.Grace)
          )
        )
      )
    }

    test("remind_me at reads a time with its offset, and says it in UTC") {
      val d = new Desk
      d.run(
        Reminders.RemindMe,
        d.asked(1),
        "text" -> "standup",
        "at" -> "2026-10-07T09:00+02:00"
      ) ==>
        Outcome.Done(s"Reminder ${ScheduleId.value(id(1))} is set for 2026-10-07 07:00:00 UTC.")
    }

    test("remind_me's arguments are refused without exactly one of in_minutes and at") {
      (
        readMe("text" -> "stretch"),
        readMe("text" -> "stretch", "in_minutes" -> 5, "at" -> "2026-10-07T09:00:00Z")
      ) ==> (
        Left(
          "`in_minutes` is missing: it takes a whole number from 1 to 527040, unless `at` is given."
        ),
        Left(
          "`at` takes nothing when `in_minutes` is given, not \"2026-10-07T09:00:00Z\"."
        )
      )
    }

    test("remind_me's at is refused without its offset, or when it is no time") {
      val accepts = "an ISO-8601 date and time with its offset, such as " +
        "`2026-10-07T09:00:00+02:00` or `2026-10-07T07:00:00Z`"
      (
        readMe("text" -> "stretch", "at" -> "2026-10-07T09:00:00"),
        readMe("text" -> "stretch", "at" -> "tomorrow at 9")
      ) ==> (
        Left(s"`at` takes $accepts, not \"2026-10-07T09:00:00\"."),
        Left(s"`at` takes $accepts, not \"tomorrow at 9\".")
      )
    }

    test("remind_me's text is refused blank or longer than 500 characters") {
      val accepts = "text that is not blank, at most 500 characters"
      (
        readMe("text" -> " ", "in_minutes" -> 5),
        readMe("text" -> "x" * 501, "in_minutes" -> 5).left.map(_.take(60))
      ) ==> (
        Left(s"`text` takes $accepts, not \" \"."),
        Left(s"`text` takes $accepts, not \"xxxx".take(60))
      )
    }

    test("remind_me says why it set nothing: no thread to post in, past, too far, too many") {
      val d = new Desk
      val past = Instant.parse("2026-10-06T14:00:00Z")
      val far = Instant.parse("2027-10-08T00:00:00Z")
      val limit = Instant.parse("2027-10-07T14:03:12Z")
      (1 to 20).foreach(n =>
        d.run(Reminders.RemindMe, d.asked(100 + n), "text" -> s"r$n", "in_minutes" -> n)
      )
      (
        d.run(
          Reminders.RemindMe,
          d.asked(1, address = None, by = PrincipalId("U2")),
          "text" -> "a",
          "in_minutes" -> 5
        ),
        d.run(
          Reminders.RemindMe,
          d.asked(2, by = PrincipalId("U2")),
          "text" -> "a",
          "at" -> "2026-10-06T14:00:00Z"
        ),
        d.run(
          Reminders.RemindMe,
          d.asked(3, by = PrincipalId("U2")),
          "text" -> "a",
          "at" -> "2027-10-08T00:00:00Z"
        ),
        d.run(Reminders.RemindMe, d.asked(4), "text" -> "a", "in_minutes" -> 5),
        (1 to 4).map(n => d.schedules.read(id(n))(using TestTx.fake))
      ) ==> (
        Outcome.Failed(
          "Nothing was scheduled: this conversation's replies are not posted anywhere a " +
            "reminder could be."
        ),
        Outcome.Failed(s"Nothing was scheduled: $past is not after now, $now."),
        Outcome.Failed(
          s"Nothing was scheduled: $far is after $limit, the furthest ahead one can be."
        ),
        Outcome.Failed(
          "Nothing was scheduled: you already have 20 pending, the most one person may have."
        ),
        Vector.fill(4)(Right(None))
      )
    }

    test("remind_me is not bound for a plugin whose jobs do not hold remind") {
      Reminders.RemindMe
        .bind(reads, Needs.over(name, Vector.empty), OwnJobs.over(name, Vector.empty))
        .left
        .map(_.toString) ==> Left(NotOwn(name, Reminders.Remind.name).toString)
    }

    test("reminders says the time now, then the asker's pending reminders, soonest first") {
      val d = new Desk
      d.run(Reminders.RemindMe, d.asked(1), "text" -> "stretch", "in_minutes" -> 30)
      d.run(Reminders.RemindMe, d.asked(2), "text" -> "tea", "in_minutes" -> 10)
      d.run(
        Reminders.RemindMe,
        d.asked(3, by = PrincipalId("U2")),
        "text" -> "not theirs",
        "in_minutes" -> 5
      )
      d.clock.at = now.plusSeconds(60)
      d.run(Reminders.List, d.asked(4)) ==> Outcome.Done(
        "Now: 2026-10-06 14:04:12 UTC\n" +
          "Pending, soonest first:\n" +
          s"${ScheduleId.value(id(2))}, due 2026-10-06 14:13:12 UTC: tea\n" +
          s"${ScheduleId.value(id(1))}, due 2026-10-06 14:33:12 UTC: stretch"
      )
    }

    test("reminders says the time now, and that none are pending") {
      val d = new Desk
      d.run(Reminders.List, d.asked(1)) ==>
        Outcome.Done("Now: 2026-10-06 14:03:12 UTC\nNo reminders are pending.")
    }
  }
}
