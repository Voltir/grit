package grit.eval.harness.bench

import scala.util.Try

import grit.core.json.Fields
import grit.dbos.internal.BenchRows

/** A git commit's full name: 40 lower-case hex digits. */
opaque type Commit = String

object Commit {

  /** `name` as a commit's full name, or why not. */
  def of(name: String): Either[String, Commit] =
    if (name.length == 40 && name.forall(c => c.isDigit || ('a' to 'f').contains(c))) Right(name)
    else Left(s"`$name` is not a commit's full name: 40 lower-case hex digits")

  def value(c: Commit): String = c
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
  * commits that made it, in order, and its checks (the law is every task's, beside them).
  */
final case class Task(
    name: BenchRows.TaskName,
    base: Commit,
    brief: String,
    commits: Vector[Commit],
    checks: Vector[Check]
) extends caps.Pure

object Task {

  /** The sentence every brief ends with: the rig runs the checks itself, so the coder runs only
    * the suites it touched.
    */
  val closing: String =
    "When you stop, the rig runs this task's checks itself, so run only the suites you touch, " +
      "never a bare `scripts/check`."

  /** The task `name` of a `task.json`'s text and its brief, its [[closing]] sentence after it, or
    * why not: not an object of `base`, `commits` and `checks`; a commit not 40 lower-case hex
    * digits; no commits; no checks; a runner not `check` or `it`; a check with no arguments; an
    * empty brief.
    */
  def of(name: BenchRows.TaskName, json: String, brief: String): Either[String, Task] = {
    val named = BenchRows.TaskName.value(name)
    val what = s"$named's task.json"
    for {
      root <- Try(ujson.read(json)).toOption.toRight(s"$what is not JSON")
      fields = Fields(what, root)
      base <- fields.str("base").flatMap(Commit.of)
      listed <- fields.arr("commits")
      commits <- each(listed)(v => Fields.str(s"$what's commit", v).flatMap(Commit.of))
      _ <- Either.cond(commits.nonEmpty, (), s"$what names no commits")
      written <- fields.arr("checks")
      checks <- each(written)(check(named, _))
      _ <- Either.cond(checks.nonEmpty, (), s"$what names no checks")
      _ <- Either.cond(brief.trim.nonEmpty, (), s"$named's brief is empty")
    } yield Task(name, base, s"${brief.stripTrailing}\n\n$closing", commits, checks)
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
