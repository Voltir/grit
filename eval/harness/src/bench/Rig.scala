package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{Argv, Handing, Processes, Ran, RelPath, Started, Variables}
import grit.core.id.{Harness, SessionId}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.{Grade, ModelId}
import grit.core.secret.Secret
import grit.dbos.internal.TrialRows
import grit.dbos.sql.DbConfig
import grit.kit.environment.DotEnv
import grit.models.{AnthropicRates, ModelRole}

/** What one run may spend: its model, by the Anthropic API's id for it (`claude-haiku-5-5`); the
  * most it may spend, in USD, which a run can pass (Claude Code's run, by Claude Code's own
  * estimate; grit's, priced by [[grit.models.AnthropicRates]] as each call answers and stopped
  * once its calls reach it, so by at most the call that reached it and one then in flight); and
  * its most turns (grit's tool rounds).
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

/** What runs the coder: Claude Code or grit. */
sealed trait Arm extends caps.Pure

object Arm {

  /** Claude Code: the `claude` program, scout (the program its MCP server runs, in the
    * checkout), and the prompt of the bench's coder, the agent the run's session is.
    */
  final case class ClaudeCode(claude: String, scout: String, agent: String) extends Arm

  /** grit, as `grit work` runs it: the program `launcher` starts grit's app; `scout` is the
    * program grit's scout service runs; `coder` is the instructions the coder's brief begins
    * with; `database` is the JDBC URL of the database grit keeps its turn in, logged in as
    * `docker-compose.yml`'s user; `maxTokens` is the most each reply may write; `passed` are the
    * variables of this machine's that grit's JVM needs, such as `PATH`, `HOME` and `JAVA_HOME`.
    */
  final case class Grit private (
      launcher: String,
      scout: String,
      coder: String,
      database: String,
      maxTokens: Int,
      passed: Variables
  ) extends Arm

  object Grit {

    /** The arm of these, or why not: a database URL that names no database, or names the
      * operator's own ([[DbConfig.DefaultDatabase]]), which a run of the bench must never write
      * to; or `maxTokens` below 1.
      */
    def of(
        launcher: String,
        scout: String,
        coder: String,
        database: String,
        maxTokens: Int,
        passed: Variables
    ): Either[String, Grit] =
      for {
        named <- DbConfig.Endpoint
          .of(database)
          .database
          .toRight("the grit coder's database URL names no database")
        _ <- Either.cond(
          named != DbConfig.DefaultDatabase,
          (),
          s"the grit coder's database $named is the operator's own; name one of the bench's"
        )
        _ <- Either.cond(
          maxTokens >= 1,
          (),
          s"the most tokens a reply writes, $maxTokens, is below 1"
        )
      } yield Grit(launcher, scout, coder, database, maxTokens, passed)
  }
}

/** The grit coder's turn, read back from the database it ran on. */
trait CoderTurns extends caps.SharedCapability {

  /** The newest turn the database records, read once the coder has ended; `Left` when it records
    * none or cannot be read.
    */
  def newest(): Either[String, GritTurn]
}

/** A trial, the run's output it was read from, a line a message, what that output gave, and the
  * task's checks as they ran after it.
  */
final case class Tried(
    trial: TrialRows.Trial,
    transcript: Vector[String],
    consumed: Consumed,
    checked: Checked,
    stopped: Option[String]
) extends caps.Pure {

  /** What a person reads of it, a line each: its measures, each call's tokens, its cost in cents
    * by grit's rates ([[Consumed.gritCost]]) and, for Claude Code's run, by Claude Code's own
    * estimate, how the run ended, each denial, and each check; never the transcript's text.
    */
  def lines: Vector[String] = {
    def tokens(u: Usage) = s"${Tried.input(u)}, output ${Tokens.value(u.output)}"
    val t = trial
    Vector(
      s"trial: ${t.session.local} of ${TrialRows.TaskName.value(t.task)} by " +
        s"${ModelId.value(t.model)}, ${t.wall.toSeconds} s",
      s"calls: ${t.calls}; before the first edit: " +
        t.callsBeforeEdit
          .zip(t.readBeforeEdit)
          .fold("no edit")((c, b) => s"$c calls, $b bytes read"),
      s"tokens: ${tokens(t.usage)}"
    ) ++ Tried.spending(consumed, t.session.harness) ++
      Vector(
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

object Tried {

  /** What a person reads of what `consumed` spent, a line each: each call's tokens, then its cost
    * in cents by grit's rates ([[Consumed.gritCost]]) and, for a run of a harness other than
    * grit, by Claude Code's own estimate. Printed for a trial, and for a run refused one.
    */
  def spending(consumed: Consumed, harness: Harness): Vector[String] = {
    val grit = consumed.gritCost.fold(
      reason => s"grit's rates: $reason",
      c => s"${Cost.cents(c)} by grit's rates"
    )
    val claimed = consumed.ended.flatMap(_.claimedUsd).fold("unknown")(Cost.cents)
    consumed.calls.zipWithIndex.map((c, i) => s"call ${i + 1} ${c.id}: ${input(c.usage)}") :+
      (if (harness == Harness.Grit) s"cost: $grit" else s"cost: $grit, $claimed by Claude Code's")
  }

  private def input(u: Usage): String = {
    import Tokens.value
    s"input ${value(u.input)} (cache read ${value(u.cacheRead)}, written ${value(u.cacheWrite)})"
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
    * against the known diff, `claude` handed `key` for its `apiKeyHelper` to read. A run a cap
    * stopped, or that ended in error,
    * is a trial not passed; a run that writes nothing for `quiet` is stopped and its trial kept
    * the same way; one that wrote no session is a `Left`. The checkout's `out/` is removed
    * whatever happens, and the checkout itself unless the result is a `Left`.
    */
  def trial(work: Processes, handing: Handing, clock: Clock^)(
      repository: String,
      task: Task,
      arm: Arm.ClaudeCode,
      cap: Caps,
      key: Secret,
      dir: RelPath,
      quiet: FiniteDuration
  ): Either[String, Tried] =
    tried(work)(repository, task, cap, dir) { checkout =>
      for {
        streamed <- configured { variables =>
          handing
            .started(dir, key, variables) { file =>
              for {
                _ <- Either.cond(!file.contains("'"), (), s"$file cannot be quoted")
                argv <- Argv.of(
                  "timeout",
                  Vector("-k", "1m", s"${Longest.toSeconds}s", arm.claude) ++
                    arguments(arm, cap, task, s"cat '$file'")*
                )
              } yield argv
            }(s => streamed(s, clock, quiet, _ => None))
            .left
            .map(e => s"claude did not run: ${e.message}")
        }
        Streamed(lines, ended, wall, stopped) = streamed
        consumed <- Consumed.of(lines, checkout)
        local <- consumed.session.toRight(neverStarted(ended))
        _ <- Either.cond(
          consumed.apiKeySource.contains("apiKeyHelper"),
          (),
          consumed.apiKeySource.fold("claude's init line names no apiKeySource") { source =>
            s"claude used $source, not its apiKeyHelper"
          }
        )
        harness <- Harness.of("claude-code")
        session <- SessionId.of(harness, local)
      } yield Coded(session, consumed, lines, wall, stopped)
    }

  /** The grit coder, as `arm` runs `grit work` under `cap`, on `task` in a new checkout at `dir`
    * of the repository at `repository`, as [[trial]] runs Claude Code's: the checks run after it,
    * the diff measured, the checkout tidied, and a quiet run stopped. Its brief is `arm`'s coder
    * instructions, [[Preamble]] and the task's brief, in a file of its own outside the checkout;
    * grit starts from an empty environment, but for `arm`'s passed variables and those that
    * point it at its database, the model `cap` names in every role at the Anthropic API, its
    * allowlisted tools and scout, `cap`'s turns as its rounds, and `key` as
    * `ANTHROPIC_API_KEY`, read from its file. A run whose calls reach `cap`'s budget is
    * stopped, a trial not passed, as is one whose turn did not succeed; its turn is read from
    * `turns` once it has ended. A command it left running dies with it, and its Mill daemon is
    * shut down. What grit wrote to its output, then the tail of its errors, is written to
    * `output` once it has ended, whatever becomes of the trial. `Left` for fewer than 2 turns, or
    * a model [[AnthropicRates]] has no rates for, before anything runs; when `output` cannot be
    * written or its turn cannot be read; or when the trial row refuses the run, its
    * [[Tried.spending]] lines first.
    */
  def gritTrial(work: Processes, handing: Handing, clock: Clock^, turns: CoderTurns)(
      repository: String,
      task: Task,
      arm: Arm.Grit,
      cap: Caps,
      key: Secret,
      dir: RelPath,
      quiet: FiniteDuration,
      output: Path
  ): Either[String, Tried] =
    for {
      _ <- gritCapped(cap)
      none <- Variables.of(Map.empty)
      tried <- tried(work)(repository, task, cap, dir) { _ =>
        for {
          streamed <- briefed(arm, task) { brief =>
            handing
              .started(dir, key, none) { file =>
                Argv.of(
                  "timeout",
                  Vector("-k", "1m", s"${Longest.toSeconds}s") ++
                    gritEnvironment(arm, cap) ++
                    Vector("sh", "-c", Handed, "sh", file, arm.launcher, "work", brief)*
                )
              }(s => streamed(s, clock, quiet, spent(cap)))
              .left
              .map(e => s"grit did not run: ${e.message}")
          }
          Streamed(lines, ended, wall, stopped) = streamed
          _ <- kept(output, lines, ended)
          turn <- turns.newest()
          consumed = Consumed.ofTurn(lines, turn, stopped)
          session <- SessionId.of(Harness.Grit, turn.session)
        } yield Coded(session, consumed, lines ++ turn.rendered.linesIterator, wall, stopped)
      }
    } yield tried

  /** What the grit coder's first model call on `task` would send, as `grit work --dry` prints
    * it, in a new checkout at `dir` of `repository`, removed after: run as [[gritTrial]] runs
    * it, but handed no key, so nothing is sent and nothing spent. `Left` as [[gritTrial]]
    * refuses, or with the last line of grit's errors when it fails.
    */
  def dry(work: Processes)(
      repository: String,
      task: Task,
      arm: Arm.Grit,
      cap: Caps,
      dir: RelPath
  ): Either[String, String] =
    for {
      _ <- gritCapped(cap)
      shown <- tidied(work, dir, keep = false)(
        for {
          _ <- Clone.fresh(work)(repository, task.base, dir)
          shown <- briefed(arm, task) { brief =>
            for {
              argv <- Argv.of(
                "env",
                gritEnvironment(arm, cap).drop(1) ++ Vector(arm.launcher, "work", "--dry", brief)*
              )
              shown <- work
                .started(argv, dir) { s =>
                  s.input.close()
                  val lines = s.lines.toVector
                  s.await(Longest).left.map(_.message).flatMap { ran =>
                    Either.cond(
                      ran.exit == 0,
                      lines.mkString("\n"),
                      ran.output.text.linesIterator
                        .filter(_.trim.nonEmpty)
                        .toVector
                        .lastOption
                        .fold(s"grit work --dry failed (exit ${ran.exit})")(line =>
                          s"grit work --dry failed (exit ${ran.exit}): $line"
                        )
                    )
                  }
                }
                .left
                .map(e => s"grit did not run: ${e.message}")
                .flatten
            } yield shown
          }
        } yield shown
      )
    } yield shown

  /** Writes `lines`, what grit wrote to its output, to `output`, then the tail of its errors as
    * `ended` gives them, or why its wait ended; `Left` naming `output` when it cannot be written.
    */
  private def kept(
      output: Path,
      lines: Vector[String],
      ended: Either[String, Ran]
  ): Either[String, Unit] =
    Try(
      Files.writeString(
        output,
        (lines :+ "[rig] the tail of grit's errors:" :+ ended
          .fold(identity, _.output.text)
          .stripTrailing)
          .mkString("", "\n", "\n")
      )
    ).toEither.left
      .map(e => s"grit's output was not written to $output: ${e.getClass.getName}")
      .map(_ => ())

  /** `Right` when grit can run under `cap`: at least 2 turns, and a model whose calls
    * [[AnthropicRates]] prices, since the cap is kept by that price.
    */
  private def gritCapped(cap: Caps): Either[String, Unit] =
    for {
      _ <- Either.cond(
        cap.maxTurns >= 2,
        (),
        s"grit takes at least 2 turns, its first call and one after a tool's; not ${cap.maxTurns}"
      )
      _ <- Either.cond(
        AnthropicRates.cost(cap.claudeModel, Usage.Zero).isDefined,
        (),
        s"grit's rates price no model ${cap.claudeModel}, so its cap could not stop it"
      )
    } yield ()

  /** Why a grit run whose output so far is `lines` is stopped under `cap`: its calls, priced by
    * [[AnthropicRates]], cost its budget or more; `None` while they cost less.
    */
  private def spent(cap: Caps)(lines: Vector[String]): Option[String] = {
    val cost = Consumed.gritCalls(lines).flatMap(_.usage.costUsd).sum
    Option.when(cost >= cap.maxBudgetUsd)(
      s"its calls cost ${Cost.cents(cost)}, reaching its cap of ${Cost.cents(cap.maxBudgetUsd)}"
    )
  }

  /** `env -i` and the variables grit runs with, as `name=value` arguments, as [[gritTrial]]
    * says; no key among them.
    */
  private def gritEnvironment(arm: Arm.Grit, cap: Caps): Vector[String] = {
    val model = ModelId.value(cap.model)
    val upstream = "api.anthropic.com"
    val roles = Vector(Grade.Standard, Grade.Light, Grade.Strong).flatMap { g =>
      Vector(ModelRole.Turn.modelVar(g) -> model, ModelRole.Turn.upstreamVar(g) -> upstream)
    } ++ Vector(ModelRole.Dream.modelVar -> model, ModelRole.Dream.upstreamVar -> upstream)
    Vector("env", "-i") ++ (arm.passed.values.toVector.sorted ++ Vector(
      // With no locale, the JVM writes its output in ASCII, each `·` and `¢` of a call's line a
      // `?` that no longer reads back as a call.
      "LANG" -> "C.UTF-8",
      DotEnv.FileVar -> "/dev/null",
      DbConfig.UrlVar -> arm.database,
      "GRIT_TOOLS" -> "allowlist",
      "GRIT_SCOUT" -> arm.scout,
      ModelRole.Turn.maxTokensVar -> arm.maxTokens.toString,
      "GRIT_TOOL_ROUNDS" -> cap.maxTurns.toString,
      "GRIT_ASSEMBLER" -> "linear",
      "GRIT_SESSION" -> "bench",
      "GRIT_SWEEP" -> "24h"
    ) ++ roles).map((k, v) => s"$k=$v")
  }

  /** The script `sh -c` runs to start grit with its key: the key read from the file its first
    * argument names into `ANTHROPIC_API_KEY`, then the rest of its arguments run in its place,
    * so the key is in no program's arguments.
    */
  private val Handed: String =
    """ANTHROPIC_API_KEY="$(cat "$1")"; export ANTHROPIC_API_KEY; shift; exec "$@""""

  /** `use` of the absolute path of a file holding the grit coder's brief for `task`: `arm`'s
    * coder instructions, [[Preamble]] and the task's brief. The file is in a directory only this
    * user can read under the system temp directory, removed once `use` returns or throws.
    */
  private def briefed[A](arm: Arm.Grit, task: Task)(
      use: String => Either[String, A]
  ): Either[String, A] =
    privately("the brief") { dir =>
      Try(
        Files.writeString(
          dir.resolve("brief.md"),
          s"${arm.coder.stripTrailing}\n\n$Preamble${task.brief}"
        )
      ).toEither.left
        .map(e => s"the brief was not written: ${e.getClass.getName}")
        .flatMap(file => use(file.toAbsolutePath.toString))
    }

  /** A coder's run, as `coded` makes it in the checkout whose absolute path it is given, on
    * `task` under `cap` in a new checkout at `dir` of `repository`; then the task's checks run
    * there and its diff measured against the known diff. Tidied as [[trial]] says.
    */
  private def tried(work: Processes)(repository: String, task: Task, cap: Caps, dir: RelPath)(
      coded: String => Either[String, Coded]
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
          checkout <- Clone.ran(work, dir, "pwd", "-P").map(_.trim)
          c <- coded(checkout)
          changed <- Clone.changed(work)(dir, task.base)
          checked <- Checks.run(work)(dir, task.checks).left.map(_.message)
          trial <- TrialRows.Trial
            .of(
              c.session,
              task.name,
              cap.model,
              c.consumed.usage,
              c.consumed.calls.size,
              c.wall,
              c.consumed.readBeforeEdit,
              c.consumed.callsBeforeEdit,
              passed = checked.passed && c.consumed.ended.exists(!_.error),
              distance = Option.when(changed.lines.nonEmpty)(DiffLines.distance(changed, known))
            )
            .left
            .map(refused =>
              (Tried.spending(c.consumed, c.session.harness) :+ refused).mkString("\n")
            )
        } yield Tried(trial, c.transcript, c.consumed, checked, c.stopped)
      )
    } yield tried

  /** What a coder's run gave: its session, what it consumed, its transcript, how long it ran,
    * and why it was stopped early, if it was.
    */
  private final case class Coded(
      session: SessionId,
      consumed: Consumed,
      transcript: Vector[String],
      wall: FiniteDuration,
      stopped: Option[String]
  )

  /** What the started coder `s` writes, read until it ends, writes nothing for `quiet`, or
    * `stop` gives why it is stopped; then it is killed, with every process it started still its
    * descendant, and its wall time read from `clock`.
    */
  private def streamed(
      s: Started^,
      clock: Clock^,
      quiet: FiniteDuration,
      stop: Vector[String] -> Option[String]
  ): Streamed = {
    val started = clock.millis()
    s.input.close()
    val (lines, stopped) = streamOf(s, quiet, stop)
    if (stopped.isDefined) s.kill()
    val ended = s.await(Longest + 2.minutes).left.map(_.message)
    s.kill()
    Streamed(lines, ended, (clock.millis() - started).max(0L).millis, stopped)
  }

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

  /** What the coder wrote to its output, a line each, how its wait ended, how long it ran, and
    * why it was stopped early, if it was.
    */
  private final case class Streamed(
      lines: Vector[String],
      ended: Either[String, Ran],
      wall: FiniteDuration,
      stopped: Option[String]
  )

  /** `use` of the variables `claude` runs with, none of them a key:
    * `CLAUDE_CODE_DISABLE_AUTO_MEMORY`, so a run writes no memory under its user's home, and
    * `CLAUDE_CONFIG_DIR`, an empty directory only this user can read, in a directory of its own
    * under the system temp directory, so a run writes no transcript under its user's home. That
    * directory, and all in it, is removed once `use` returns or throws. `Left` when it cannot be
    * made. It makes and removes the directory itself, as host's tree removal does, since the
    * bench names no `grit.host` (the law's `harness-names-host-from-main-alone`).
    */
  private def configured[A](use: Variables => Either[String, A]): Either[String, A] =
    privately("claude's config") { dir =>
      for {
        config <- Try(Files.createDirectory(dir.resolve("config"), Owner)).toEither.left
          .map(e => s"no config directory: ${e.getClass.getName}")
        variables <- Variables.of(
          Map(
            "CLAUDE_CODE_DISABLE_AUTO_MEMORY" -> "1",
            "CLAUDE_CONFIG_DIR" -> config.toAbsolutePath.toString
          )
        )
        used <- use(variables)
      } yield used
    }

  /** `use` of a new directory only this user can read, under the system temp directory, for
    * `what`; the directory, and all in it, removed once `use` returns or throws. `Left` naming
    * `what` when it cannot be made.
    */
  private def privately[A](what: String)(use: Path => Either[String, A]): Either[String, A] =
    Try(Files.createTempDirectory("grit-bench", Owner)).toEither.left
      .map(e => s"no directory for $what: ${e.getClass.getName}")
      .flatMap { dir =>
        try use(dir)
        finally removeTree(dir)
      }

  /** Read, written and searched by this user alone. */
  private val Owner =
    PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))

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
  private def arguments(
      arm: Arm.ClaudeCode,
      cap: Caps,
      task: Task,
      helper: String
  ): Vector[String] = {
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
      if (Clone.ran(work, dir, "test", "-d", "out/mill-daemon").isRight) {
        val _ = Clone.ran(work, dir, "./mill", "shutdown")
      }
      val _ = Clone.ran(work, dir, "rm", "-rf", "out")
      if (!(keep && outcome.forall(_.isLeft))) {
        val _ = Clone.ran(work, RelPath.Root, "rm", "-rf", RelPath.value(dir))
      }
    }
  }

  /** How often a run's output is checked for quiet: a quiet run is stopped within `quiet` and this
    * much longer.
    */
  private val Tick: FiniteDuration = 500.millis

  /** The lines `s` writes, a line each, and why they stopped early (`Some`, as the report says it):
    * it wrote nothing for `quiet`, or `stop` of the lines so far gave a reason; `None` when its
    * output ended. The caller kills `s` once they stop.
    */
  private def streamOf(
      s: Started^,
      quiet: FiniteDuration,
      stop: Vector[String] -> Option[String]
  ): (Vector[String], Option[String]) = {
    @tailrec def collect(
        acc: Vector[String],
        idle: FiniteDuration
    ): (Vector[String], Option[String]) =
      s.next(Tick) match {
        case Started.Next.Quiet if idle + Tick >= quiet =>
          (
            acc,
            Some(s"quiet for ${
                if (quiet.toSeconds % 60 == 0) s"${quiet.toMinutes} min"
                else s"${quiet.toSeconds} s"
              }")
          )
        case Started.Next.Quiet => collect(acc, idle + Tick)
        case Started.Next.Ended => (acc, None)
        case Started.Next.Line(line) =>
          val read = acc :+ line
          stop(read) match {
            case Some(why) => (read, Some(why))
            case None => collect(read, 0.millis)
          }
      }
    collect(Vector.empty, 0.millis)
  }

  /** Why a run wrote no session: how its wait ended, and for an exit, its code and the last line
    * of its errors.
    */
  private def neverStarted(ended: Either[String, Ran]): String =
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

}
