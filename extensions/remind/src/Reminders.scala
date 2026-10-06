package grit.remind

import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.id.{JobName, PluginName}
import grit.core.job.{Grace, Job, JobRun}
import grit.core.plugin.Plugin

/** Reminders (ADR 0029's first use): its job [[Reminders.Remind]] posts a person's reminder in
  * the thread that asked for it, as late as [[Reminders.Grace]] after its time, and is otherwise
  * missed.
  */
final class Reminders(val name: PluginName) extends Plugin {

  val version: Int = 1

  override val jobs: Vector[Job[?]] = Vector(Reminders.Remind)
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
}
