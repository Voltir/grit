package grit.eval.harness.bench

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{Argv, Processes, Ran, RelPath, Started}
import grit.core.id.{Harness, SessionId}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.ModelId
import grit.dbos.internal.BenchRows

/** An Anthropic API key. It reaches `claude` alone, and never prints. */
final class ApiKey private (private[bench] val value: String) extends caps.Pure {
  override def toString: String = "ApiKey(…)"
}

object ApiKey {

  /** `key`, trimmed, or why not: it is empty. */
  def of(key: String): Either[String, ApiKey] =
    Either.cond(key.trim.nonEmpty, new ApiKey(key.trim), "no Anthropic API key was given")
}

/** What one run may spend: its model, by the Anthropic API's id for it (`claude-haiku-5-5`); the
  * most it may spend, in USD by Claude Code's own estimate, which a run can pass; and its most
  * turns.
  */
final case class Caps private (model: ModelId, maxBudgetUsd: BigDecimal, maxTurns: Int)
    extends caps.Pure {

  /** The model as `claude --model` takes it, its `anthropic/` left off. */
  def claudeModel: String = ModelId.value(model).stripPrefix("anthropic/")
}

object Caps {

  /** The caps of these, or why not: a model not `claude-` and a model id's letters, a budget not a
    * decimal above 0, or turns not a whole number of at least 1.
    */
  def of(model: String, maxBudgetUsd: String, maxTurns: String): Either[String, Caps] =
    for {
      m <- Option
        .when(model.startsWith("claude-"))(model)
        .flatMap(m => ModelId.of(s"anthropic/$m"))
        .toRight(s"the model $model is not a Claude model's id, as claude-haiku-5-5")
      b <- maxBudgetUsd.toDoubleOption
        .filter(_ > 0)
        .map(_ => BigDecimal(maxBudgetUsd))
        .toRight(s"the budget $maxBudgetUsd is not an amount of USD above 0")
      t <- maxTurns.toIntOption.filter(_ >= 1).toRight(s"the turns $maxTurns are not 1 or more")
    } yield Caps(m, b, t)
}

/** What runs the coder: the `claude` program, scout (the program its MCP server runs, in the
  * checkout), and the prompt of the bench's coder, the agent the run's session is.
  */
final case class Arm(claude: String, scout: String, agent: String) extends caps.Pure

/** A trial, the run's output it was read from, a line a message, what that output gave, and the
  * task's checks as they ran after it.
  */
final case class Tried(
    trial: BenchRows.Trial,
    transcript: Vector[String],
    consumed: Consumed,
    checked: Checked,
    stopped: Option[String]
) extends caps.Pure {

  /** What a person reads of it, a line each: its measures, each call's tokens, both costs in
    * cents (grit's, from [[Consumed]], and Claude Code's), how the run ended, each denial, and each
    * check; never the transcript's text.
    */
  def lines: Vector[String] = {
    def cents(usd: Option[BigDecimal]) = usd.fold("unknown")(Cost.cents)
    def input(u: Usage) = {
      import Tokens.value
      s"input ${value(u.input)} (cache read ${value(u.cacheRead)}, written ${value(u.cacheWrite)})"
    }
    def tokens(u: Usage) = s"${input(u)}, output ${Tokens.value(u.output)}"
    val grit = consumed.gritCost.fold(
      reason => s"grit's rates: $reason",
      c => s"${Cost.cents(c)} by grit's rates"
    )
    val t = trial
    Vector(
      s"trial: ${t.session.local} of ${BenchRows.TaskName.value(t.task)} by " +
        s"${ModelId.value(t.model)}, ${t.wall.toSeconds} s",
      s"calls: ${t.calls}; before the first edit: " +
        t.callsBeforeEdit
          .zip(t.readBeforeEdit)
          .fold("no edit")((c, b) => s"$c calls, $b bytes read"),
      s"tokens: ${tokens(t.usage)}"
    ) ++ consumed.calls.zipWithIndex.map((c, i) => s"call ${i + 1} ${c.id}: ${input(c.usage)}") ++
      Vector(
        s"cost: $grit, " +
          s"${cents(consumed.ended.flatMap(_.claimedUsd))} by Claude Code's",
        consumed.ended.fold(stopped.fold("ended: with no result")(why => s"ended: $why"))(e =>
          s"ended: ${e.reason}${if (e.error) ", in error" else ""}"
        )
      ) ++ consumed.ended.toVector
        .flatMap(_.denials)
        .map(d => s"denied: ${d.tool} ${d.input.take(200)}") ++
      checked.runs.map((run, ok) => s"check: $run ${if (ok) "passed" else "failed"}") ++
      Vector(
        s"passed: ${t.passed}; distance: ${t.distance.fold("no diff")(d => s"$d lines")}"
      )
  }
}

/** A coder run on a task in a checkout of its own, and its trial. */
object Rig {

  /** 2 hours: the longest a coder runs before it is killed. */
  val Longest: FiniteDuration = 2.hours

  /** 10 minutes: the longest a coder may write nothing before it is stopped, its trial kept.
    * Claude Code writes an event at least every 30 seconds while a tool runs.
    */
  val Quiet: FiniteDuration = 10.minutes

  /** The name the bench's coder goes by in `--agents`. */
  val Coder: String = "bench-coder"

  /** The tools the coder's session has, beside scout's. */
  val Tools: String = "Bash,Read,Edit,Write,Glob,Grep"

  /** What the coder may do without asking: under `--permission-prompts none`, anything else that
    * would ask is refused. Commands that only read, as `ls`, run without a rule.
    */
  val Allowed: Vector[String] = Vector(
    "Read",
    "Edit",
    "Glob",
    "Grep",
    "mcp__scout__*",
    "Bash(./mill *)",
    "Bash(scripts/check *)",
    "Bash(git add *)",
    "Bash(git commit *)",
    "Bash(git status)",
    "Bash(git status *)",
    "Bash(git diff *)",
    "Bash(git log *)"
  )

  /** What the coder's prompt says before its task's brief. */
  val Preamble: String =
    """You work alone in this directory, a fresh git repository on the branch `bench` that holds
      |one commit's history. There is no planning store, no main session or manager to report to,
      |and no GitHub here: where the brief below names them, or a worktree, a branch or a pull
      |request, read it as this directory and the branch `bench`. Build the change the brief
      |describes, with its tests, and commit it on `bench` when it is done. Make no model call.
      |
      |# The brief
      |
      |""".stripMargin

  /** A coder, as `arm` runs it under `cap`, on `task` in a new checkout at `dir` of the repository
    * at `repository` (an absolute path); then the task's checks run there, and its diff measured
    * against the known diff. `key` reaches `claude` alone, through a file removed once it exits,
    * and never the environment of anything it runs. A run a cap stopped, or that ended in error,
    * is a trial not passed; a run that writes nothing for `quiet` is stopped and its trial kept
    * the same way; one that wrote no session is a `Left`. The checkout's `out/` is removed
    * whatever happens, and the checkout itself unless the result is a `Left`.
    */
  def trial(work: Processes, clock: Clock^)(
      repository: String,
      task: Task,
      arm: Arm,
      cap: Caps,
      key: ApiKey,
      dir: RelPath,
      quiet: FiniteDuration
  ): Either[String, Tried] =
    for {
      knownDir <- RelPath.of(s"${RelPath.value(dir)}-known").left.map(_.toString)
      known <- tidied(work, knownDir, keep = false)(
        Clone
          .picked(work)(repository, task, knownDir)
          .flatMap(_ => Clone.changed(work)(knownDir, task.base))
      )
      tried <- tidied(work, dir, keep = true)(
        for {
          _ <- Clone.fresh(work)(repository, task.base, dir)
          checkout <- run(work, dir, Clone.Longest)("pwd", "-P").map(_.output.text.trim)
          streamed <- handedOver(key) { (helper, added) =>
            for {
              argv <- Argv.of(
                "timeout",
                Vector("-k", "1m", s"${Longest.toSeconds}s", arm.claude) ++
                  arguments(arm, cap, task, helper)*
              )
              started = clock.millis()
              streamed <- work
                .started(argv, dir, added) { s =>
                  s.input.close()
                  val (lines, stopped) = streamOf(s, quiet)
                  if (stopped.isDefined) s.kill()
                  val ended = s.await(Longest + 2.minutes).left.map(_.message)
                  s.kill()
                  Streamed(lines, ended, (clock.millis() - started).max(0L).millis, stopped)
                }
                .left
                .map(e => s"claude did not run: ${e.message}")
            } yield streamed
          }
          Streamed(lines, ended, wall, stopped) = streamed
          consumed <- Consumed.of(lines, checkout)
          local <- consumed.session.toRight(neverStarted(ended, key))
          _ <- Either.cond(
            consumed.apiKeySource.contains("apiKeyHelper"),
            (),
            consumed.apiKeySource.fold("claude's init line names no apiKeySource") { source =>
              s"claude used $source, not its apiKeyHelper"
            }
          )
          changed <- Clone.changed(work)(dir, task.base)
          checked <- Checks.run(work)(dir, task.checks).left.map(_.message)
          harness <- Harness.of("claude-code")
          session <- SessionId.of(harness, local)
          trial <- BenchRows.Trial.of(
            session,
            task.name,
            cap.model,
            consumed.usage,
            consumed.calls.size,
            wall,
            consumed.readBeforeEdit,
            consumed.callsBeforeEdit,
            passed = checked.passed && consumed.ended.exists(!_.error),
            distance = Option.when(changed.lines.nonEmpty)(Changed.distance(changed, known))
          )
        } yield Tried(trial, lines, consumed, checked, stopped)
      )
    } yield tried

  /** `task`'s known diff at `dir`, its commits applied to its base, and then its checks run;
    * `Left` when a commit does not apply. The checkout is removed after, as [[trial]]'s is.
    */
  def known(
      work: Processes
  )(repository: String, task: Task, dir: RelPath): Either[String, Checked] =
    tidied(work, dir, keep = true)(
      Clone
        .picked(work)(repository, task, dir)
        .flatMap(_ => Checks.run(work)(dir, task.checks).left.map(_.message))
    )

  /** What `claude` wrote to its output, a line each, and how long it ran. */
  private final case class Streamed(
      lines: Vector[String],
      ended: Either[String, Ran],
      wall: FiniteDuration,
      stopped: Option[String]
  )

  /** `use` of how `key` reaches `claude`, and only `claude`: `helper`, the command its
    * `apiKeyHelper` setting runs to print the key, which reads a file only this user can read, in
    * a directory of its own under the system temp directory; and `added`, the variables to add to
    * its environment, which hold no key: `CLAUDE_CODE_DISABLE_AUTO_MEMORY`, so a run writes no
    * memory under its user's home, and `CLAUDE_CONFIG_DIR`, an empty directory of its own beside
    * the key file, so a run writes no transcript under its user's home. The directory, and all in
    * it, is removed once `use` returns or throws. `Left` when the file cannot be written.
    */
  private def handedOver[A](key: ApiKey)(
      use: (String, Map[String, String]) => Either[String, A]
  ): Either[String, A] = {
    val owner = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
    val readable =
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    Try(Files.createTempDirectory("grit-bench-key", owner)).toEither.left
      .map(e => s"no directory for the key: ${e.getClass.getName}")
      .flatMap { dir =>
        try {
          for {
            file <- Try {
              val file = Files.createFile(dir.resolve("key"), readable)
              Files.writeString(file, key.value, StandardCharsets.UTF_8)
            }.toEither.left.map(e => s"the key was not written: ${e.getClass.getName}")
            config <- Try(Files.createDirectory(dir.resolve("config"), owner)).toEither.left
              .map(e => s"no config directory: ${e.getClass.getName}")
            _ <- Either.cond(!file.toString.contains("'"), (), s"$file cannot be quoted")
            used <- use(
              s"cat '$file'",
              Map(
                "CLAUDE_CODE_DISABLE_AUTO_MEMORY" -> "1",
                "CLAUDE_CONFIG_DIR" -> config.toAbsolutePath.toString
              )
            )
          } yield used
        } finally removeTree(dir)
      }
  }

  /** Removes `dir` and all in it, deepest first, each path in its own `Try` so that one that
    * cannot go does not keep the rest. A symbolic link is removed as a link, never followed.
    */
  private def removeTree(dir: Path): Unit =
    Try(Files.walk(dir)).foreach { walk =>
      try
        walk.toList.asScala.reverse.foreach { p =>
          val _ = Try(Files.deleteIfExists(p))
        }
      finally walk.close()
    }

  /** `claude`'s arguments for `task` under `cap`, its program left off; `helper` its
    * `apiKeyHelper`.
    */
  private def arguments(arm: Arm, cap: Caps, task: Task, helper: String): Vector[String] = {
    val settings = ujson.Obj(
      "apiKeyHelper" -> helper,
      "disableAllHooks" -> true,
      "permissions" -> ujson.Obj("allow" -> ujson.Arr.from(Allowed))
    )
    val mcp = ujson.Obj(
      "mcpServers" -> ujson.Obj(
        "scout" -> ujson.Obj("command" -> arm.scout, "args" -> ujson.Arr("mcp"))
      )
    )
    val agents = ujson.Obj(
      Coder -> ujson.Obj(
        "description" -> "Builds one change to this repository from its brief, tests first.",
        "prompt" -> arm.agent
      )
    )
    Vector(
      "-p",
      Preamble + task.brief,
      "--restricted",
      "--permission-prompts",
      "none",
      "--output-format",
      "stream-json",
      "--verbose",
      "--tools",
      Tools,
      "--settings",
      ujson.write(settings),
      "--strict-mcp-config",
      "--mcp-config",
      ujson.write(mcp),
      "--agents",
      ujson.write(agents),
      "--agent",
      Coder,
      "--model",
      cap.claudeModel,
      "--max-budget-usd",
      cap.maxBudgetUsd.toString,
      "--max-turns",
      cap.maxTurns.toString
    )
  }

  /** `result`, made in the checkout at `dir`; then its Mill daemon shut down and its `out/`
    * removed, and the checkout itself unless `keep` and `result` is a `Left`.
    */
  private def tidied[A](work: Processes, dir: RelPath, keep: Boolean)(
      result: => Either[String, A]
  ): Either[String, A] = {
    var outcome = Option.empty[Either[String, A]]
    try {
      val r = result
      outcome = Some(r)
      r
    } finally {
      // A checkout that built nothing has no daemon; shutting one down there would start Mill.
      if (run(work, dir, Clone.Longest)("test", "-d", "out/mill-daemon").isRight) {
        val _ = run(work, dir, Clone.Longest)("./mill", "shutdown")
      }
      val _ = run(work, dir, Clone.Longest)("rm", "-rf", "out")
      if (!(keep && outcome.forall(_.isLeft))) {
        val _ = run(work, RelPath.Root, Clone.Longest)("rm", "-rf", RelPath.value(dir))
      }
    }
  }

  /** How often a run's output is checked for quiet: a quiet run is stopped within `quiet` and this
    * much longer.
    */
  private val Tick: FiniteDuration = 500.millis

  /** The lines `s` writes, a line each, and why they stopped early (`Some`, as the report says it)
    * when it wrote nothing for `quiet`; `None` when its output ended. The lines are read by a
    * thread of their own, so that the wait can be timed; the caller kills `s` once they stop, and
    * the kill ends the output, which ends that thread.
    */
  private def streamOf(s: Started^, quiet: FiniteDuration): (Vector[String], Option[String]) = {
    val arrived = new LinkedBlockingQueue[Option[String]]()
    // Read by the thread below alone, and only until its output ends: no other reader is made.
    @caps.unsafe.untrackedCaptures
    val reader = new BufferedReader(new InputStreamReader(s.output, StandardCharsets.UTF_8))
    val pump = new Thread(() =>
      try reader.lines().forEach(line => arrived.put(Some(line)))
      finally arrived.put(None)
    )
    pump.setDaemon(true)
    pump.start()
    @tailrec def collect(
        acc: Vector[String],
        idle: FiniteDuration
    ): (Vector[String], Option[String]) =
      Option(arrived.poll(Tick.toMillis, TimeUnit.MILLISECONDS)) match {
        case None if idle + Tick >= quiet =>
          (
            acc,
            Some(s"quiet for ${
                if (quiet.toSeconds % 60 == 0) s"${quiet.toMinutes} min"
                else s"${quiet.toSeconds} s"
              }")
          )
        case None => collect(acc, idle + Tick)
        case Some(None) => (acc, None)
        case Some(Some(line)) => collect(acc :+ line, 0.millis)
      }
    collect(Vector.empty, 0.millis)
  }

  /** Why a run wrote no session: how its wait ended, and for an exit, its code and the last line
    * of its errors. `key` is replaced by `[key]` throughout.
    */
  private def neverStarted(ended: Either[String, Ran], key: ApiKey): String =
    ended
      .fold(
        e => s"claude wrote no session: $e",
        ran =>
          ran.output.text.linesIterator
            .filter(_.trim.nonEmpty)
            .toVector
            .lastOption
            .fold(s"claude wrote no session (exit ${ran.exit})") { line =>
              s"claude wrote no session (exit ${ran.exit}): $line"
            }
      )
      .replace(key.value, "[key]")

  /** `program` with `args` run in `dir`; `Left` when it cannot start, times out or exits other
    * than 0.
    */
  private def run(work: Processes, dir: RelPath, timeout: FiniteDuration)(
      program: String,
      args: String*
  ): Either[String, Ran] =
    for {
      argv <- Argv.of(program, args*)
      ran <- work.run(argv, dir, timeout).left.map(_.message)
      _ <- Either.cond(ran.exit == 0, (), s"$program exited ${ran.exit}: ${ran.output.text}")
    } yield ran
}
