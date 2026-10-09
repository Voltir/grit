package grit.core.job

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.Moves
import grit.core.id.JobName

import utest.*

/** [[Jobs]], a deployment's jobs by name, and [[When]], a once slot as a person asks for it. */
object JobTests extends TestSuite {

  /** A job named `name` whose parameters are a count, at `version`. */
  final class Counting(text: String, val version: Int = 1) extends PlainJob[Count] {
    val name: JobName = JobName.of(text).getOrElse(throw new java.lang.AssertionError(text))
    def write(params: Count): ujson.Value = ujson.Num(params.n)
    def read(params: ujson.Value): Either[String, Count] =
      params.numOpt.map(n => Count(n.toInt)).toRight(s"not a count: $params")
    def run(run: JobRun[Count], moves: Moves^): String = s"${run.params.n}"
  }

  final case class Count(n: Int) extends caps.Pure

  private def name(text: String): JobName =
    JobName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  val tests = Tests {
    test("a deployment's jobs are found by name; a name it lacks finds none") {
      val (remind, standup) = (new Counting("remind", 2), new Counting("standup"))
      val jobs = Jobs
        .of(Vector(Owned.Deployments(remind), Owned.Deployments(standup)))
        .getOrElse(throw new java.lang.AssertionError())
      Vector("remind", "standup", "digest").map(n => jobs.named(name(n)).map(_.job.version)) ==>
        Vector(Some(2), Some(1), None)
    }

    test("jobs two of which share a name are refused, naming the first name shared") {
      Jobs
        .of(
          Vector(
            new Counting("a"),
            new Counting("b"),
            new Counting("b", 2),
            new Counting("a", 3)
          ).map(Owned.Deployments(_))
        )
        .fold(shared => Some(JobName.value(shared)), _ => None) ==> Some("b")
    }

    test("a once slot asked at an instant falls then; asked in a delay, that long after now") {
      val now = Instant.parse("2026-10-07T09:00:00Z")
      When.At(Instant.parse("2026-10-08T00:00:00Z")).from(now) ==>
        Instant.parse("2026-10-08T00:00:00Z")
      When.In(90.minutes).from(now) ==> Instant.parse("2026-10-07T10:30:00Z")
      When.In(1500.millis).from(now) ==> Instant.parse("2026-10-07T09:00:01.500Z")
    }
  }
}
