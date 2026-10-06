package grit.remind

import java.time.Instant

import grit.core.job.JobRun

import utest.*

object RemindersTests extends TestSuite {

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
  }
}
