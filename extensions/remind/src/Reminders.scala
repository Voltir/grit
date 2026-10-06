package grit.remind

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, OffsetDateTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.id.{CallSlot, JobName, PluginName, ScheduleId}
import grit.core.job.{Grace, Job, JobRun, NotOwn, OwnJobs, ScheduleDesk, When}
import grit.core.plugin.{Needs, Plugin, PluginReads, PluginRun, PluginTool, Unneeded}
import grit.core.store.Db
import grit.core.tool.{Args, ArgsError, Field, Gate, Hosted, Outcome, Retry, ToolName, ToolSpec}

/** Reminders (ADR 0029's first use): its job [[Reminders.Remind]] posts a person's reminder in
  * the thread that asked for it, as late as [[Reminders.Grace]] after its time, and is otherwise
  * missed.
  */
final class Reminders(val name: PluginName) extends Plugin {

  val version: Int = 1

  override val jobs: Vector[Job[?]] = Vector(Reminders.Remind)

  override val tools: Vector[PluginTool[?]] = Vector(Reminders.RemindMe)
}

/** A reminder's text: not blank, and at most [[Reminders.MaxText]] characters. */
final case class Reminder private (text: String) extends caps.Pure

object Reminder {

  /** `text` as a reminder, or why it is none: it is blank, or too long. */
  def of(text: String): Either[String, Reminder] =
    if (text.isBlank) Left("a reminder's text is blank")
    else if (text.length > Reminders.MaxText)
      Left(s"a reminder's text is longer than ${Reminders.MaxText} characters")
    else Right(new Reminder(text))
}

object Reminders {

  /** How late a reminder may run after its time before it is missed: 1 h. */
  val Grace: Grace =
    // A positive literal, which `Grace.of` accepts.
    grit.core.job.Grace.of(1.hour).getOrElse(throw new IllegalStateException("grace"))

  /** The longest a reminder's text may be: 500 characters. */
  val MaxText: Int = 500

  /** The job `remind`, version 1: replies "Reminder: {text}", and, run more than a minute after
    * its time, a second line "(due {time, UTC to the minute}; sent late)". Parameters it cannot
    * read (no text, or text no [[Reminder]] holds) are refused.
    */
  val Remind: Job[Reminder] = new Job[Reminder] {
    val name: JobName =
      // A literal of the job name's grammar: every test of the job throws here if not.
      JobName.of("remind").fold(why => throw new IllegalStateException(why), identity)

    val version: Int = 1

    def write(params: Reminder): ujson.Value = ujson.Obj("text" -> params.text)

    def read(params: ujson.Value): Either[String, Reminder] =
      params.objOpt
        .flatMap(_.get("text"))
        .flatMap(_.strOpt)
        .toRight(s"a reminder's parameters hold its text, not ${params.render().take(100)}")
        .flatMap(Reminder.of)

    def reply(run: JobRun[Reminder]): String = {
      val said = s"Reminder: ${run.params.text}"
      if (Duration.between(run.nominal, run.started).compareTo(Late) > 0)
        s"$said\n(due ${Minute.format(run.nominal)}; sent late)"
      else said
    }
  }

  /** How late a run may start before its reply says so. */
  private val Late: Duration = Duration.ofMinutes(1)

  private val Minute =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'").withZone(ZoneOffset.UTC)

  /** The most minutes ahead `in_minutes` takes: [[ScheduleDesk.Horizon]]'s. */
  private val MaxMinutes: Int = ScheduleDesk.Horizon.toMinutes.toInt

  private val TextAccepts = s"text that is not blank, at most $MaxText characters"

  private val AtAccepts = "an ISO-8601 date and time with its offset, such as " +
    "`2026-10-07T09:00:00+02:00` or `2026-10-07T07:00:00Z`"

  /** An instant as the tools say it to the model: UTC, to the second. */
  private val Second =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss 'UTC'").withZone(ZoneOffset.UTC)

  /** `remind_me`'s arguments: what to remind of, and when. */
  type Setting = (reminder: Reminder, when: When)

  /** `remind_me`: sets a reminder `in_minutes` from now, or `at` a time with its offset, and
    * says its id and its time in UTC; or the sentence its desk refused it with
    * ([[grit.core.job.DeskRefusal.said]]). Free. Not bound for a plugin whose jobs do not hold
    * [[Remind]].
    */
  val RemindMe: PluginTool[Setting] = new PluginTool[Setting] {
    val described: Hosted[Setting] =
      new Hosted(
        ToolSpec(
          ToolName("remind_me"),
          "Set a one-off reminder for the person you are answering, posted in this " +
            "conversation when it is due. Give `text` and exactly one of `in_minutes` or `at`. " +
            "Answers with the reminder's id and its time in UTC. Nothing is set when that time " +
            s"is not in the future or is more than ${ScheduleDesk.Horizon.toDays} days ahead, " +
            s"when they already have ${ScheduleDesk.PendingCap} pending, or when this " +
            "conversation's replies are not posted anywhere; the answer says which.",
          Args
            .of(
              (
                text = Field.text(s"What to remind them of, at most $MaxText characters."),
                in_minutes = Field
                  .count("How many minutes from now; give this or `at`.", 1, MaxMinutes)
                  .optional,
                at = Field
                  .text(s"When, as $AtAccepts; give this or `in_minutes`.")
                  .optional
              )
            )
            .refine(a =>
              for {
                reminder <- Reminder
                  .of(a.text)
                  .left
                  .map(_ => ArgsError.Invalid("text", TextAccepts, quoted(a.text)))
                when <- (a.in_minutes, a.at) match {
                  case (Some(m), None) => Right(When.In(m.minutes))
                  case (None, Some(at)) => instant(at).map(When.At(_))
                  case (None, None) =>
                    Left(
                      ArgsError.Missing(
                        "in_minutes",
                        s"a whole number from 1 to $MaxMinutes, unless `at` is given"
                      )
                    )
                  case (Some(_), Some(at)) =>
                    Left(ArgsError.Invalid("at", "nothing when `in_minutes` is given", quoted(at)))
                }
              } yield (reminder = reminder, when = when)
            ),
          // The same call asking again gets the reminder it first set.
          retry = Retry.Rerun
        ),
        Gate.Free,
        a => s"${shown(a.when)}: ${a.reminder.text}"
      )

    def bind(
        own: PluginReads,
        needs: Needs,
        jobs: OwnJobs
    ): Either[Unneeded | NotOwn, PluginRun[Setting]] =
      jobs.of(Remind).map { booking =>
        new PluginRun[Setting] {
          def run(a: Setting, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome =
            desk.ask(call, booking, a.when, Grace, a.reminder) match {
              case Left(refused) => Outcome.Failed(refused.said)
              case Right(asked) =>
                Outcome.Done(
                  s"Reminder ${ScheduleId.value(asked.id)} is set for ${Second.format(asked.at)}."
                )
            }
        }
      }
  }

  /** The instant `at` writes, with its offset; refused without one. */
  private def instant(at: String): Either[ArgsError, Instant] =
    scala.util
      .Try(OffsetDateTime.parse(at).toInstant)
      .toOption
      .toRight(ArgsError.Invalid("at", AtAccepts, quoted(at)))

  /** `text` as JSON, cut as an argument error quotes what was sent. */
  private def quoted(text: String): String = ujson.Str(text).render().take(ArgsError.Shown)

  private def shown(when: When): String = when match {
    case When.In(delay) => s"in ${delay.toMinutes} min"
    case When.At(at) => s"at ${Second.format(at)}"
  }
}
