package grit.core.job

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Keeping, Moves}
import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{JobName, PluginName}

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

  /** A keeping job named `tally` whose parameters are a count. */
  final class Tallying extends KeepingJob[Count] {
    val name: JobName = JobName.of("tally").getOrElse(throw new java.lang.AssertionError("tally"))
    val version: Int = 1
    def write(params: Count): ujson.Value = ujson.Num(params.n)
    def read(params: ujson.Value): Either[String, Count] =
      params.numOpt.map(n => Count(n.toInt)).toRight(s"not a count: $params")
    def run(run: JobRun[Count], moves: Keeping^): String = s"${run.params.n}"
  }

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

    test("only a plugin's keeping jobs keep, under its terms: never the deployment's own") {
      val tally = new Tallying
      val p = PluginName.of("p").getOrElse(throw new java.lang.AssertionError("p"))
      val terms = DocLabel
        .of("tallies")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 10))
        .getOrElse(throw new java.lang.AssertionError("terms"))
      // A plugin's plain job, and its keeping job under its terms, compile.
      val _ = (Owned.Plugins(p, new Counting("c")), Owned.Keeps(p, terms, tally))
      val notPlain =
        "Found:    (tally : grit.core.job.JobTests.Tallying)\nRequired: grit.core.job.PlainJob[?]"
      (
        assertCompileError("Owned.Deployments(tally)").msg,
        assertCompileError("Owned.Plugins(p, tally)").msg
      ) ==> (notPlain, notPlain)
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
