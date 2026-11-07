package grit.eval.harness.bench

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{Argv, Handing, Memory, Processes, Ran, RelPath, Variables}
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
  def claudeModel: String = Caps.anthropicId(model)
}

object Caps {

  /** `model` by the Anthropic API's own id, as [[AnthropicRates]] and `claude --model` take it:
    * its `anthropic/` left off.
    */
  def anthropicId(model: ModelId): String = ModelId.value(model).stripPrefix("anthropic/")

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

/** The most memory a coder's processes may hold together while it runs, in bytes, and the least
  * the machine must keep available meanwhile: past either, the run is stopped.
  */
final case class MemoryCap private (most: Long, floor: Long) extends caps.Pure

object MemoryCap {

  /** 1 GB: the least memory the machine must keep available while a coder runs. */
  val Floor: Long = 1L << 30

  /** 8 GB, its floor [[Floor]]. */
  val Default: MemoryCap = MemoryCap(8L << 30, Floor)

  /** `gb` gigabytes, its floor [[Floor]], or why not: not a whole number of at least 1. */
  def of(gb: String): Either[String, MemoryCap] =
    gb.toIntOption
      .filter(_ >= 1)
      .map(n => MemoryCap(n.toLong << 30, Floor))
      .toRight(s"the memory budget $gb is not a whole number of GB of at least 1")

  /** Why a run whose processes hold what `held` says is stopped under `cap`, as the report says
    * it: they hold more than its budget, or the machine has less available than its floor;
    * `None` while neither.
    */
  def stops(cap: MemoryCap)(held: Memory.Held): Option[String] =
    if (held.resident > cap.most)
      Some(
        s"its processes held ${gb(held.resident)} GB, past its memory budget of ${gb(cap.most)} GB"
      )
    else if (held.available < cap.floor)
      Some(
        s"the machine had ${gb(held.available)} GB available, under its floor of ${gb(cap.floor)} GB"
      )
    else None

  /** `bytes` in GB to a tenth, its trailing zeros left off. */
  private def gb(bytes: Long): String =
    (BigDecimal(bytes) / BigDecimal(1L << 30))
      .setScale(1, BigDecimal.RoundingMode.HALF_UP)
      .bigDecimal
      .stripTrailingZeros
      .toPlainString
}

/** What runs the coder: Claude Code or grit. */
sealed trait Arm extends caps.Pure

object Arm {

  /** Claude Code: the `claude` program, scout (the program its MCP server runs, in the
    * checkout), and the prompt of the bench's coder, the agent the run's session is.
    */
  final case class ClaudeCode(claude: String, scout: String, agent: String) extends Arm

  /** grit, as `grit work` runs it, its trials kept under [[Harness.Grit]]: the program `launcher`
    * starts grit's app; `scout` is the program grit's scout service runs; `standing` is what the
    * coder's brief begins with; `database` is the JDBC URL of the database grit keeps its turn
    * in, logged in as `docker-compose.yml`'s user; `maxTokens` is the most each reply may write;
    * `passed` are the variables of this machine's that grit's JVM needs, such as `PATH`, `HOME`
    * and `JAVA_HOME`; `settings` is what the trial sets of grit's run.
    */
  final case class Grit private (
      launcher: String,
      scout: String,
      standing: Standing,
      database: String,
      maxTokens: Int,
      passed: Variables,
      settings: Settings
  ) extends Arm

  /** What a grit trial sets of grit's run besides the rig's own variables: its window's limit
    * (`window`, `GRIT_WINDOW_TOKENS`) and the most turns `grit work` goes on for (`workTurns`,
    * `GRIT_WORK_TURNS`); each unset leaves grit's own default.
    */
  final case class Settings(
      window: Option[Tokens] = None,
      workTurns: Option[Int] = None
  )

  object Grit {

    /** The arm of these, or why not: a database URL that names no database, or names the
      * operator's own ([[DbConfig.DefaultDatabase]]), which a run of the bench must never write
      * to; `maxTokens` below 1.
      */
    def of(
        launcher: String,
        scout: String,
        standing: Standing,
        database: String,
        maxTokens: Int,
        passed: Variables,
        settings: Settings = Settings()
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
      } yield Grit(launcher, scout, standing, database, maxTokens, passed, settings)
  }
}

/** The grit coder's task, read back from the database it ran on. */
trait CoderTurns extends caps.SharedCapability {

  /** The task of the newest turn the database records, every turn of that turn's conversation,
    * read once the coder has ended; failed for why when its turns cannot be read back
    * ([[GritTurn.unread]]); `Left` when the database records no turn, or cannot be read.
    */
  def task(): Either[String, GritTurn]
}

/** A trial, the run's output it was read from, a line a message, the coder's diff from the task's
  * base ([[Clone.changes]]), what that output gave, the task's checks as they ran after it, and
  * the arm that ran it.
  */
final case class Tried(
    trial: TrialRows.Trial,
    transcript: Vector[String],
    diff: String,
    consumed: Consumed,
    checked: Checked,
    stopped: Option[String],
    arm: Arm
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
    ) ++ Tried.spending(consumed, arm) ++
      Vector(
        consumed.ended.fold(stopped.fold("ended: with no result")(why => s"ended: $why"))(e =>
          s"ended: ${e.reason}${if (e.error) ", in error" else ""}"
        )
      ) ++ consumed.ended.toVector
        .flatMap(_.denials)
        .map(d => s"denied: ${d.tool} ${d.input.take(200)}") ++
      checked.lines ++
      Vector(
        s"passed: ${t.passed}; distance: ${t.distance.fold("no diff")(d => s"$d lines")}"
      )
  }
}

object Tried {

  /** What a person reads of what `consumed` spent, a line each: each call's tokens, then its cost
    * in cents by grit's rates ([[Consumed.gritCost]]) and, for a run `arm` is Claude Code's, by
    * Claude Code's own estimate, then its thinking ([[thinking]]). Printed for a trial, and for a run refused one.
    */
  def spending(consumed: Consumed, arm: Arm): Vector[String] = {
    val grit = consumed.gritCost.fold(
      reason => s"grit's rates: $reason",
      c => s"${Cost.cents(c)} by grit's rates"
    )
    val claimed = consumed.ended.flatMap(_.claimedUsd).fold("unknown")(Cost.cents)
    consumed.calls.zipWithIndex.map((c, i) => s"call ${i + 1} ${c.id}: ${input(c.usage)}") :+
      (arm match {
        case _: Arm.Grit => s"cost: $grit"
        case _: Arm.ClaudeCode => s"cost: $grit, $claimed by Claude Code's"
      }) :+ thinking(consumed.calls)
  }

  /** The thinking of `calls` together, its cost at [[AnthropicRates.thinkingCost]] (`≥` first
    * when a call's model has no rates); `not reported` where a call's provider said nothing of it.
    */
  private def thinking(calls: Vector[Call]): String = {
    import Tokens.value
    val total = Usage.total(calls.map(_.usage))
    val reported = calls.flatMap(_.usage.thinking)
    total.thinking match {
      case Some(t) =>
        val cost = AnthropicRates.thinkingCost(calls.map(c => c.model -> c.usage))
        s"thinking: ${value(t)} of ${value(total.output)} output tokens, ${cost.inCents} by grit's rates"
      case None if reported.nonEmpty =>
        s"thinking: not reported by every call (${reported.map(value).sum} over the " +
          s"${reported.size} of ${calls.size} that did)"
      case None => "thinking: not reported"
    }
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

  /** The work root at `root` cleared before a run, a run being what one trial left under it,
    * named `<task>-<seconds>` by when it began: its checkout and known checkout (`-known`), which
    * a run removes as it ends unless it was killed, and its log (`.log`). Every run's checkouts
    * go, and every log but the newest `runs`; any other entry is left. How many entries went, or
    * why the root could not be read; an entry that cannot be removed is left for the next.
    */
  def cleared(root: Path)(runs: Int): Either[String, Int] =
    Try(Files.list(root)).toEither.left
      .map(e => s"the work root $root could not be read: ${e.getClass.getName}")
      .map { listing =>
        val entries =
          try listing.iterator.asScala.toVector
          finally listing.close()
        val runOf = entries.flatMap { p =>
          p.getFileName.toString match {
            case RunEntry(stem, seconds, suffix) =>
              seconds.toLongOption.map(s => (p, (s, stem), suffix == ".log"))
            case _ => None
          }
        }
        val newest = runOf.map(_._2).distinct.sorted.reverse
        val logsKept = newest.take(runs).toSet
        runOf.count { (p, run, log) =>
          val goes = !log || !logsKept(run)
          goes && {
            if (log) Try(Files.deleteIfExists(p)).isSuccess
            else { removeTree(p); !Files.exists(p) }
          }
        }
      }

  /** A run's entry under the work root: its stem and when it began, then `-known`, `.log`
    * or nothing.
    */
  private val RunEntry = raw"(.+)-(\d+)(-known|\.log)?".r

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

  /** What both arms' coders are told of where they work: this directory and its branch. */
  private val Situation: String =
    """You work alone in this directory, a fresh git repository on the branch `bench` that holds
      |one commit's history. There is no planning store, no main session or manager to report to,
      |and no GitHub here: where the brief below names them, or a worktree, a branch or a pull
      |request, read it as this directory and the branch `bench`. Build the change the brief
      |describes, with its tests, and commit it on `bench` when it is done. Make no model call.
      |""".stripMargin

  /** What heads the task's brief, after the situation. */
  private val Heading: String = "\n# The brief\n\n"

  /** What Claude Code's prompt says before its task's brief: the situation alone, its agent
    * prompt saying the rest.
    */
  val Preamble: String = Situation + Heading

  /** What the grit coder's brief says between its standing text and its task's brief: the
    * situation, and what holds of a grit run alone, its compiled output and its memory budget.
    */
  val GritPreamble: String =
    Situation +
      """
        |Only the modules the task's checks name were compiled here before you started: compile
        |any other module before you ask scout about it. The run is stopped once the processes
        |working in this directory together hold more than its memory budget.
        |""".stripMargin + Heading

  /** A coder, as `arm` runs it under `cap`, on `task` in a new checkout at `dir` of the repository
    * at `repository` (an absolute path); then the task's checks run there, and its diff measured
    * against the known diff, `claude` handed `key` for its `apiKeyHelper` to read. A run a cap
    * stopped, or that ended in error,
    * is a trial not passed; a run that writes nothing for `quiet` is stopped and its trial kept
    * the same way; one that wrote no session is a `Left`. `output`, the run's log, is begun
    * afresh; whatever happens, what the coder left in the checkout ([[Clone.left]]) is added to
    * it, and then the checkout is removed.
    */
  def trial(work: Processes, handing: Handing, clock: Clock^)(
      repository: String,
      task: BenchTask,
      arm: Arm.ClaudeCode,
      cap: Caps,
      key: Secret,
      dir: RelPath,
      quiet: FiniteDuration,
      output: Path
  ): Either[String, Tried] =
    tried(work)(repository, task, arm, cap, dir, output, () => None) { checkout =>
      for {
        streamed <- configured { variables =>
          handing
            .started(dir, key, variables) { file =>
              for {
                _ <- Either.cond(!file.contains("'"), (), s"$file cannot be quoted")
                argv <- Watched.limited(
                  Longest,
                  arm.claude +: arguments(arm, cap, task, s"cat '$file'")*
                )
              } yield argv
            }(s => timed(clock)(Watched.ran(s, quiet, Longest + 2.minutes, _ => None, () => None)))
            .left
            .map(e => s"claude did not run: ${e.message}")
        }
        (Watched.Read(lines, ended, stopped), wall) = streamed
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
    * the diff measured, the checkout tidied, and a quiet run stopped. Before grit starts, the
    * checkout compiles every module the task's checks name, unbilled, for at most [[Warming]],
    * its Mill daemon left warm for the coder; a checkout that does not compile is a `Left` saying
    * so, grit never started. Its brief is `arm`'s standing text, [[GritPreamble]] and the task's
    * brief, in a file of its own outside the checkout;
    * grit starts from an empty environment, but for `arm`'s passed variables and those that
    * point it at its database, the model `cap` names in every role at the Anthropic API, its
    * allowlisted tools and scout, `cap`'s turns as its rounds, the linear assembler, `arm`'s
    * settings ([[Arm.Settings]]), and `key` as `ANTHROPIC_API_KEY`, read from its file. A run whose calls reach `cap`'s budget is
    * stopped, a trial not passed, as is one whose processes, all those working in the checkout
    * ([[Memory.held]]), hold more than `memoryCap` allows or leave the machine under its floor
    * ([[MemoryCap.stops]]), or one whose turn did not succeed; that budget holds for the compile
    * before grit, a `Left` naming it, and for the checks as [[Checks.run]] says; its task, every
    * turn of it, is read from `turns` once it has ended. A command it left running dies with it, and its Mill daemon is
    * shut down. What grit wrote to its output, then the tail of its errors, is written to
    * `output` once it has ended, whatever becomes of the trial. `Left` for fewer than 2 turns, or
    * a model [[AnthropicRates]] has no rates for, before anything runs; when `memory` cannot be
    * read in the checkout, before its compile starts; when `output` cannot be
    * written or its turn cannot be read; or when the trial row refuses the run, its
    * [[Tried.spending]] lines first.
    */
  def gritTrial(
      work: Processes,
      handing: Handing,
      clock: Clock^,
      turns: CoderTurns,
      memory: Memory
  )(
      repository: String,
      task: BenchTask,
      arm: Arm.Grit,
      cap: Caps,
      memoryCap: MemoryCap,
      key: Secret,
      dir: RelPath,
      quiet: FiniteDuration,
      output: Path
  ): Either[String, Tried] =
    for {
      _ <- gritCapped(cap)
      none <- Variables.of(Map.empty)
      tried <- tried(work)(
        repository,
        task,
        arm,
        cap,
        dir,
        output,
        holding(memory, dir, memoryCap)
      ) { _ =>
        for {
          _ <- memory
            .held(dir)
            .left
            .map(e =>
              s"the memory grit's processes hold cannot be read, so its budget could not hold: ${e.message}"
            )
          _ <- warmed(work)(task, arm, cap, dir, output, holding(memory, dir, memoryCap))
          streamed <- briefed(arm, task) { brief =>
            handing
              .started(dir, key, none) { file =>
                Watched.limited(
                  Longest,
                  gritEnvironment(arm, cap) ++
                    Vector("sh", "-c", Handed, "sh", file, arm.launcher, "work", brief)*
                )
              }(s =>
                timed(clock)(
                  Watched.ran(
                    s,
                    quiet,
                    Longest + 2.minutes,
                    spent(cap),
                    holding(memory, dir, memoryCap)
                  )
                )
              )
              .left
              .map(e => s"grit did not run: ${e.message}")
          }
          (Watched.Read(lines, ended, stopped), wall) = streamed
          _ <- kept(output, lines, ended)
          turn <- turns.task()
          reported = ended.fold(_ => turn, r => Consumed.reported(turn, r.exit, r.output.text))
          consumed = Consumed.ofTurn(lines, reported, stopped)
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
      task: BenchTask,
      arm: Arm.Grit,
      cap: Caps,
      dir: RelPath
  ): Either[String, String] =
    for {
      _ <- gritCapped(cap)
      shown <- tidied(work, dir, None)(
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

  /** Every JVM started under it held to 15% of the machine's memory: read by every `java` grit's
    * tree and the task's checks start, grit, a Mill daemon and each forked test JVM alike, each
    * of which takes a quarter by default, and a few of them at once took it all.
    */
  val CappedJvm: (String, String) = "JDK_JAVA_OPTIONS" -> "-XX:MaxRAMPercentage=15"

  /** 60 minutes: the longest the checkout's compile before the coder may take. */
  val Warming: FiniteDuration = 60.minutes

  /** The checkout at `dir` compiled, every module `task`'s checks name and those under it
    * (`./mill <module>.__.compile`), in grit's environment under `cap` but with no key, so that
    * the coder's first build or test is not a cold one and nothing it spends goes on it. Its
    * Mill daemon is left for the coder, and [[tidied]] shuts it down. `output` gets the command,
    * the last 200 lines it printed and the tail of its errors. `watch` is asked while it runs, as
    * [[Watched.ran]] asks it. `Left` in words when it fails, takes longer than [[Warming]], is
    * stopped by `watch` (naming the compile and the reason), or `output` cannot be written;
    * nothing when the checks name no module.
    */
  private def warmed(work: Processes)(
      task: BenchTask,
      arm: Arm.Grit,
      cap: Caps,
      dir: RelPath,
      output: Path,
      watch: () => Option[String]
  ): Either[String, Unit] = {
    val targets = task.checks.flatMap(_.args).distinct.map(m => s"$m.__.compile")
    val mill = Vector("./mill", "--ticker", "false") ++
      targets.flatMap(t => Vector("+", t)).drop(1)
    val failed = "the checkout did not compile before the coder"
    if (targets.isEmpty) Right(())
    else
      for {
        argv <- Watched.limited(
          Warming,
          (Vector("env") ++ gritEnvironment(arm, cap).drop(1) ++ mill)*
        )
        read <- work
          .started(argv, dir)(
            Watched.ran(_, Warming + 1.minute, Warming + 2.minutes, _ => None, watch)
          )
          .left
          .map(e => s"$failed: ${e.message}")
        _ <- written(output, append = true)(
          s"[rig] compiled before the coder: ${mill.mkString(" ")}" +: (read.lines.takeRight(200) ++
            read.ended.fold(Vector(_), r => r.output.text.linesIterator.toVector))
        )
        _ <- read.stopped
          .map(why => s"the checkout's compile before the coder was stopped: $why")
          .toLeft(())
        ran <- read.ended.left.map(e => s"$failed: $e")
        _ <- Either.cond(
          ran.exit == 0,
          (),
          (read.lines ++ ran.output.text.linesIterator)
            .filter(_.trim.nonEmpty)
            .lastOption
            .fold(s"$failed (exit ${ran.exit})")(line => s"$failed (exit ${ran.exit}): $line")
        )
      } yield ()
  }

  /** Writes `lines`, what grit wrote to its output, to `output` after what is there, then the
    * tail of its errors as `ended` gives them, or why its wait ended; `Left` naming `output` when
    * it cannot be written.
    */
  private def kept(
      output: Path,
      lines: Vector[String],
      ended: Either[String, Ran]
  ): Either[String, Unit] =
    written(output, append = true)(
      lines :+ "[rig] the tail of grit's errors:" :+ ended
        .fold(identity, _.output.text)
        .stripTrailing
    )

  /** `lines` written to `output`, a line each, after what it holds when `append`; `Left` naming
    * `output` when they cannot be.
    */
  private def written(output: Path, append: Boolean)(lines: Vector[String]): Either[String, Unit] =
    Try {
      val text = if (lines.isEmpty) "" else lines.mkString("", "\n", "\n")
      if (append)
        Files.writeString(
          output,
          text,
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
      else Files.writeString(output, text, StandardCharsets.UTF_8)
    }.toEither.left
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

  /** `run`'s result, and how long it took by `clock`. */
  private def timed[A](clock: Clock^)(run: => A): (A, FiniteDuration) = {
    val start = clock.millis()
    val a = run
    (a, (clock.millis() - start).max(0L).millis)
  }

  /** Why the processes working in `dir` are past `cap`, as [[MemoryCap.stops]] says, asked of
    * `memory`; nothing when `memory` cannot tell.
    */
  private def holding(memory: Memory^, dir: RelPath, cap: MemoryCap): () => Option[String] =
    () => memory.held(dir).toOption.flatMap(MemoryCap.stops(cap))

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
      CappedJvm,
      DotEnv.FileVar -> "/dev/null",
      DbConfig.UrlVar -> arm.database,
      "GRIT_TOOLS" -> "allowlist",
      "GRIT_SCOUT" -> arm.scout,
      ModelRole.Turn.maxTokensVar -> arm.maxTokens.toString,
      "GRIT_TOOL_ROUNDS" -> cap.maxTurns.toString,
      "GRIT_ASSEMBLER" -> "linear",
      "GRIT_SESSION" -> "bench",
      "GRIT_SWEEP" -> "24h"
    ) ++ roles ++ settings(arm.settings)).map((k, v) => s"$k=$v")
  }

  /** The variables `s` sets, each it leaves unset left out. */
  private def settings(s: Arm.Settings): Vector[(String, String)] =
    s.window.map(w => "GRIT_WINDOW_TOKENS" -> Tokens.value(w).toString).toVector ++
      s.workTurns.map(n => "GRIT_WORK_TURNS" -> n.toString)

  /** The script `sh -c` runs to start grit with its key: the key read from the file its first
    * argument names into `ANTHROPIC_API_KEY`, then the rest of its arguments run in its place,
    * so the key is in no program's arguments.
    */
  private val Handed: String =
    """ANTHROPIC_API_KEY="$(cat "$1")"; export ANTHROPIC_API_KEY; shift; exec "$@""""

  /** `use` of the absolute path of a file holding the grit coder's brief for `task`: `arm`'s
    * standing text, [[GritPreamble]] and the task's brief. The file is in a directory only this
    * user can read under the system temp directory, removed once `use` returns or throws.
    */
  private def briefed[A](arm: Arm.Grit, task: BenchTask)(
      use: String => Either[String, A]
  ): Either[String, A] =
    privately("the brief") { dir =>
      Try(
        Files.writeString(
          dir.resolve("brief.md"),
          s"${arm.standing.text.stripTrailing}\n\n$GritPreamble${task.brief}"
        )
      ).toEither.left
        .map(e => s"the brief was not written: ${e.getClass.getName}")
        .flatMap(file => use(file.toAbsolutePath.toString))
    }

  /** A coder's run, as `coded` makes it in the checkout whose absolute path it is given, on
    * `task` under `cap` by `arm` in a new checkout at `dir` of `repository`; then the task's
    * checks run there, `watch` asked as [[Checks.run]] says, and its diff measured against the
    * known diff. Tidied as [[trial]] says.
    */
  private def tried(work: Processes)(
      repository: String,
      task: BenchTask,
      arm: Arm,
      cap: Caps,
      dir: RelPath,
      output: Path,
      watch: () => Option[String]
  )(
      coded: String => Either[String, Coded]
  ): Either[String, Tried] =
    for {
      _ <- written(output, append = false)(Vector.empty)
      knownDir <- RelPath.of(s"${RelPath.value(dir)}-known").left.map(_.toString)
      known <- tidied(work, knownDir, None)(
        Clone
          .picked(work)(repository, task, knownDir)
          .flatMap(_ => Clone.changed(work)(knownDir, task.base))
      )
      tried <- tidied(work, dir, Some((base = task.base, log = output)))(
        for {
          _ <- Clone.fresh(work)(repository, task.base, dir)
          checkout <- Clone.ran(work, dir, "pwd", "-P").map(_.trim)
          c <- coded(checkout)
          diff <- Clone.changes(work)(dir, task.base)
          changed = DiffLines.of(diff)
          checked <- Checks
            .run(work)(dir, task.checks, Vector(CappedJvm), watch)
            .left
            .map(_.message)
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
              distance = Option.when(changed.lines.nonEmpty)(DiffLines.distance(changed, known)),
              standing = Standing.ofArm(arm).map(_.sha256)
            )
            .left
            .map(refused => (Tried.spending(c.consumed, arm) :+ refused).mkString("\n"))
        } yield Tried(trial, c.transcript, diff, c.consumed, checked, c.stopped, arm)
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

  /** `task`'s known diff at `dir`, its commits applied to its base, and then its checks run;
    * `Left` when a commit does not apply. The checkout is removed after, as [[trial]]'s is.
    */
  def known(
      work: Processes
  )(repository: String, task: BenchTask, dir: RelPath): Either[String, Checked] =
    tidied(work, dir, None)(
      Clone
        .picked(work)(repository, task, dir)
        .flatMap(_ =>
          Checks.run(work)(dir, task.checks, Vector(CappedJvm), () => None).left.map(_.message)
        )
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
      task: BenchTask,
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

  /** `result`, made in the checkout at `dir`; then, whatever it is, its Mill daemon shut down and
    * its `out/` removed, what the coder left ([[Clone.left]] from `saved`'s base) added to
    * `saved`'s log, or why it could not be read, and the checkout removed.
    */
  private def tidied[A](work: Processes, dir: RelPath, saved: Option[(base: Sha, log: Path)])(
      result: => Either[String, A]
  ): Either[String, A] =
    try result
    finally {
      // A checkout that built nothing has no daemon; shutting one down there would start Mill.
      if (Clone.ran(work, dir, "test", "-d", "out/mill-daemon").isRight) {
        val _ = Clone.ran(work, dir, "./mill", "shutdown")
      }
      val _ = Clone.ran(work, dir, "rm", "-rf", "out")
      saved.foreach { (base, log) =>
        val _ = written(log, append = true)(
          Clone
            .left(work)(dir, base)
            .fold(why => Vector(s"[rig] what the coder left: $why"), identity)
        )
      }
      val _ = Clone.ran(work, RelPath.Root, "rm", "-rf", RelPath.value(dir))
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
