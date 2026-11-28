package grit.eval.harness.bench

import scala.util.Try

import grit.core.json.Fields
import grit.dbos.internal.TrialRows

/** A git commit's full name: 40 lower-case hex digits. */
opaque type Sha = String

object Sha {

  /** `name` as a commit's full name, or why not. */
  def of(name: String): Either[String, Sha] =
    if (name.length == 40 && name.forall(c => c.isDigit || ('a' to 'f').contains(c))) Right(name)
    else Left(s"`$name` is not a commit's full name: 40 lower-case hex digits")

  def value(c: Sha): String = c
}

/** What runs a check: `scripts/check` or `scripts/it` (the integration tier), with a check's
  * arguments after it.
  */
enum Runner {
  case Check, It
}

/** One of a task's checks: `runner` with `args`, as `scripts/check grit.models`. */
final case class Check(runner: Runner, args: Vector[String]) extends caps.Pure

/** A past change frozen as a task: the commit it started from, the brief its coder was given, the
  * commits that made it, in order, its checks (the law is every task's, beside them), and what a
  * trial must leave beyond them to be graded Complete, when it names that.
  */
final case class BenchTask(
    name: TrialRows.TaskName,
    base: Sha,
    brief: String,
    commits: Vector[Sha],
    checks: Vector[Check],
    completion: Option[Completion]
) extends caps.Pure

object BenchTask {

  /** The sentences every brief ends with: the rig runs the checks itself, so the coder runs a
    * suite while it works and every module it changed before it stops, never the whole gate.
    */
  val closing: String =
    "When you stop, the rig runs this task's checks itself. While you work, run the suite you " +
      "are changing; before you stop, run every module whose files you changed, each as " +
      "`scripts/check <module>`, and never a bare `scripts/check`."

  /** The task `name` of a `task.json`'s text and its brief, its [[closing]] sentence after it, or
    * why not: not an object of `base`, `commits` and `checks`; a commit not 40 lower-case hex
    * digits; no commits; no checks; a runner not `check` or `it`; a check with no arguments; an
    * empty brief; a `complete` not an object of `commits` (a count from 0), `exists`, `absent`
    * (globs) and `holds` (each a `path` glob and its `text`).
    */
  def of(name: TrialRows.TaskName, json: String, brief: String): Either[String, BenchTask] = {
    val named = TrialRows.TaskName.value(name)
    val what = s"$named's task.json"
    for {
      root <- Try(ujson.read(json)).toOption.toRight(s"$what is not JSON")
      fields = Fields(what, root)
      base <- fields.str("base").flatMap(Sha.of)
      listed <- fields.arr("commits")
      commits <- each(listed)(v => Fields.str(s"$what's commit", v).flatMap(Sha.of))
      _ <- Either.cond(commits.nonEmpty, (), s"$what names no commits")
      written <- fields.arr("checks")
      checks <- each(written)(check(named, _))
      _ <- Either.cond(checks.nonEmpty, (), s"$what names no checks")
      _ <- Either.cond(brief.trim.nonEmpty, (), s"$named's brief is empty")
      complete <- fields.added("complete")
      completion <- Fields.opt(complete)(completion(named, _))
    } yield BenchTask(
      name,
      base,
      s"${brief.stripTrailing}\n\n$closing",
      commits,
      checks,
      completion
    )
  }

  /** The completion `v` of the task `named`, or why not. */
  private def completion(named: String, v: ujson.Value): Either[String, Completion] = {
    val what = s"$named's complete"
    val fields = Fields(what, v)
    for {
      commits <- fields.int("commits")
      _ <- Either.cond(commits >= 0, (), s"$what's commits is below 0")
      existing <- fields.arr("exists")
      exists <- each(existing)(Fields.str(s"$what's exists", _))
      absence <- fields.arr("absent")
      absent <- each(absence)(Fields.str(s"$what's absent", _))
      listed <- fields.arr("holds")
      holds <- each(listed) { h =>
        val held = Fields(s"$what's holds", h)
        for {
          path <- held.str("path")
          text <- held.str("text")
        } yield Holding(path, text)
      }
    } yield Completion(commits, exists, absent, holds)
  }

  /** The check `v` of the task `named`, or why not. */
  private def check(named: String, v: ujson.Value): Either[String, Check] = {
    val fields = Fields(s"$named's check", v)
    for {
      written <- fields.str("runner")
      runner <- written match {
        case "check" => Right(Runner.Check)
        case "it" => Right(Runner.It)
        case other => Left(s"$named's check runner `$other` is neither `check` nor `it`")
      }
      listed <- fields.arr("args")
      args <- each(listed)(Fields.str(s"$named's check argument", _))
      _ <- Either.cond(args.nonEmpty, (), s"$named's check of `$written` has no arguments")
    } yield Check(runner, args)
  }

  /** `read` of each of `vs`, or the first refusal. */
  private def each[A](vs: Vector[ujson.Value])(
      read: ujson.Value => Either[String, A]
  ): Either[String, Vector[A]] = {
    val (refused, accepted) = vs.partitionMap(read)
    refused.headOption.toLeft(accepted)
  }
}
