package grit.eval.harness.bench

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.matching.Regex

import grit.assembly.linear.LinearAssembler
import grit.core.clock.Clock
import grit.core.host.{Argv, Handing, Memory, Processes, Ran, RelPath, Variables}
import grit.core.id.{Harness, SessionId}
import grit.core.message.{Cost, SpendCap, Tokens, Usage}
import grit.core.model.{CatalogJson, Effort, Grade, ModelId, ModelRef, ReasoningReplay, Upstream}
import grit.core.secret.{Secret, Variable}
import grit.dbos.internal.TrialRows
import grit.dbos.sql.DbConfig
import grit.kit.environment.DotEnv
import grit.models.{AnthropicProvider, AnthropicRates, ModelRole, OpenRouterConfig}
import grit.turn.TurnRecord

/** What one run may spend: its pair, a Claude model at the Anthropic API or an OpenRouter model
  * at one upstream; the most it may spend, in USD, which a run can pass (Claude Code's run, by
  * Claude Code's own estimate; grit's, each call priced by [[grit.models.AnthropicRates]] where
  * it has the model, else by the cost its provider reported, as each call answers, and stopped
  * once its calls reach it, so by at most the call that reached it and one then in flight); and
  * its most rounds of a turn (grit's tool rounds, and Claude Code's `--max-turns`).
  */
final case class Caps private (
    model: ModelId,
    upstream: Upstream,
    budget: SpendCap,
    maxRounds: Int
) extends caps.Pure {

  def pair: ModelRef = ModelRef(model, Some(upstream))

  /** The model as `claude --model` takes it, its `anthropic/` left off; `None` for a pair not at
    * the Anthropic API, which Claude Code cannot run.
    */
  def claudeModel: Option[String] =
    Option.when(upstream == Upstream.AnthropicApi)(Caps.anthropicId(model))

  /** The variable holding the key its pair is called with: `ANTHROPIC_API_KEY` at the Anthropic
    * API, else `OPENROUTER_API_KEY`.
    */
  def key: Variable =
    if (upstream == Upstream.AnthropicApi) AnthropicProvider.KeyVar else OpenRouterConfig.KeyVar
}

object Caps {

  /** `model` by the Anthropic API's own id, as [[AnthropicRates]] and `claude --model` take it:
    * its `anthropic/` left off.
    */
  def anthropicId(model: ModelId): String = ModelId.value(model).stripPrefix("anthropic/")

  /** The caps of these, or why not: with no upstream, a model not a Claude model's id, which runs
    * at the Anthropic API (`claude-haiku-5-5`); with one, a model not an OpenRouter model's id
    * (`deepseek/deepseek-v4.1-flash-20260910`) or an upstream not an OpenRouter slug
    * (`fireworks`); a budget not a decimal above 0; or rounds not a whole number of at least 1.
    */
  def of(
      model: String,
      upstream: Option[String],
      maxBudgetUsd: String,
      maxRounds: String
  ): Either[String, Caps] = {
    val OpenRouterExample = "deepseek/deepseek-v4.1-flash-20260910"
    for {
      m <- upstream match {
        case None =>
          Option
            .when(model.startsWith("claude-"))(model)
            .flatMap(m => ModelId.of(s"anthropic/$m"))
            .map(id => (id, Upstream.AnthropicApi))
            .toRight(
              s"the model $model is not a Claude model's id, as claude-haiku-5-5, and names no " +
                s"upstream, as an OpenRouter model's does ($OpenRouterExample at fireworks)"
            )
        case Some(slug) =>
          for {
            id <- ModelId
              .of(model)
              .toRight(s"the model $model is not an OpenRouter model's id, as $OpenRouterExample")
            u <- Upstream
              .of(slug)
              .toRight(s"the upstream $slug is not an OpenRouter upstream's slug, as fireworks")
            _ <- Either.cond(
              u != Upstream.AnthropicApi,
              (),
              s"the upstream $slug is the Anthropic API, named by giving a Claude model's id, as " +
                "claude-haiku-5-5, and no upstream"
            )
          } yield (id, u)
      }
      b <- Try(BigDecimal(maxBudgetUsd)).toOption
        .flatMap(SpendCap.of(_).toOption)
        .toRight(s"the budget $maxBudgetUsd is not an amount of USD above 0")
      r <- maxRounds.toIntOption.filter(_ >= 1).toRight(s"the rounds $maxRounds are not 1 or more")
      (id, u) = m
    } yield Caps(id, u, b, r)
  }
}

/** The most memory a coder's processes may hold together while it runs, in bytes, and the least
  * the machine must keep available meanwhile: past either, the run is stopped.
  */
final case class MemoryCap private (most: Long, floor: Long) extends caps.Pure

object MemoryCap {

  /** 1 GB: the least memory the machine must keep available while a coder runs. */
  val Floor: Long = 1L << 30

  /** 24 GB, its floor [[Floor]]: the frozen tasks' checks were measured at up to 16.5 GB, all of
    * one task's run in turn, sampled every 2 s, so a peak between samples went unseen.
    */
  val Default: MemoryCap = MemoryCap(24L << 30, Floor)

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
    * (`window`, `GRIT_WINDOW_TOKENS`), the most turns `grit work` goes on for (`workTurns`,
    * `GRIT_WORK_TURNS`), and the reasoning effort its turn's standard rung and its dreaming ask
    * (`effort`, `GRIT_EFFORT` and `GRIT_DREAM_EFFORT`; the turn's summary, query and heard roles,
    * the heard role the task classifier, follow the turn's); each unset leaves grit's own
    * default. A call routed to another rung, or allotted another effort, asks that one
    * ([[Tried.under]] reports which). `replay` is how the turn's calls send earlier reasoning
    * back (`GRIT_REPLAY`, [[grit.models.OpenRouterConfig.ReplayVar]]), unset its pair's profile's.
    */
  final case class Settings(
      window: Option[Tokens] = None,
      workTurns: Option[Int] = None,
      effort: Option[Effort] = None,
      replay: Option[ReasoningReplay] = None
  )

  object Settings {

    /** How `--replay` names `replay`: `all`, `none`, `latest`. */
    def replayWord(replay: ReasoningReplay): String = replay match {
      case ReasoningReplay.Details => "all"
      case ReasoningReplay.Dropped => "none"
      case ReasoningReplay.Latest => "latest"
    }

    /** The replay [[replayWord]] spells `word`, or why there is none. */
    def readReplay(word: String): Either[String, ReasoningReplay] = {
      val replays = ReasoningReplay.values.toVector
      replays
        .find(replayWord(_) == word)
        .toRight(s"is not one of ${replays.map(replayWord).mkString(", ")}")
    }
  }

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
  * base ([[Clone.changes]]), what that output gave, the task's checks as they ran after it, the
  * arm that ran it, the pair it ran on, whose upstream the trial does not record, and what a grit
  * coder's turn recorded it ran under ([[RanUnder.Unrecorded]] for Claude Code's).
  */
final case class Tried(
    trial: TrialRows.Trial,
    transcript: Vector[String],
    diff: String,
    consumed: Consumed,
    checked: Checked,
    stopped: Option[String],
    arm: Arm,
    pair: ModelRef,
    ranUnder: RanUnder
) extends caps.Pure {

  /** What a person reads of it, a line each: its measures and its pair, for a grit coder what it
    * ran under ([[Tried.under]]), its work ([[Tried.work]]), each call's tokens, its cost as [[Tried.spending]] gives it, how
    * the run ended, each denial, and each check; never the transcript's text.
    */
  def lines: Vector[String] = {
    def tokens(u: Usage) = s"${Tried.input(u)}, output ${Tokens.value(u.output)}"
    val t = trial
    Vector(
      s"trial: ${t.session.local} of ${TrialRows.TaskName.value(t.task)} by $pair, " +
        s"${t.wall.toSeconds} s"
    ) ++ (arm match {
      case g: Arm.Grit => Vector(Tried.under(g.settings, ranUnder))
      case _: Arm.ClaudeCode => Vector.empty
    }) ++ Vector(
      s"calls: ${t.calls}; before the first edit: " +
        t.callsBeforeEdit
          .zip(t.readBeforeEdit)
          .fold("no edit")((c, b) => s"$c calls, $b bytes read"),
      Tried.work(consumed.work, consumed.calls.size),
      s"tokens: ${tokens(t.usage)}"
    ) ++ Tried.spending(consumed, arm) ++
      Vector(
        consumed.ended.fold(stopped.fold("ended: with no result")(why => s"ended: $why"))(e =>
          s"ended: ${e.reason}${if (e.error) ", in error" else ""}"
        )
      ) ++ consumed.ended.toVector
        .flatMap(_.denials)
        .map(d => s"denied: ${d.tool} ${d.input.take(200)}") ++
      checked.lines :+
      Tried.verdict(t)
  }
}

object Tried {

  /** `trial`'s grade as a person reads it, the last of [[Tried.lines]]: whether it passed, and
    * its distance from its task's known diff in lines, or `no diff`.
    */
  def verdict(trial: TrialRows.Trial): String =
    s"passed: ${trial.passed}; distance: ${trial.distance.fold("no diff")(d => s"$d lines")}"

  /** What every [[verdict]] holds, found anywhere in a line holding one. */
  val Verdicts: Regex = "passed: (true|false); distance: ".r

  /** `work` in `calls` model calls, as a person reads it: its tool calls a model call to one
    * decimal, `-` when it made no model call.
    */
  def work(work: Consumed.Work, calls: Int): String = {
    val each =
      if (calls == 0) "-"
      else (BigDecimal(work.tools) / calls).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString
    s"work: ${work.tools} tool calls in $calls model calls ($each a call); edits: " +
      s"${work.edits}, ${work.failedEdits} failed; suite runs: ${work.suiteRuns}; " +
      s"commits: ${work.commits}"
  }

  /** What a grit coder set by `settings` ran under, as `ran` recorded it, as a person reads it:
    * the grade its turn's calls were routed to and the effort they asked, followed by
    * `(set <effort>)` when that is not what `settings` asked (`none` when unset); a pin asking
    * none at the Anthropic API as [[AnthropicProvider.DefaultEffort]], `(grit's default)`, the
    * effort that provider sends then; the reasoning replay its pin sent under, in
    * [[Arm.Settings.replayWord]]'s words, followed by `(set <replay>)` when `settings` set
    * another; the window a boundary draws (grit's own default when
    * unset); the input limit; and what ended its last turn ([[TurnRecord.EndedBy]]). What `ran`
    * holds no record of is `not recorded`, and what did not read `not read`.
    */
  def under(settings: Arm.Settings, ran: RanUnder): String = {
    val set = settings.effort.fold("none")(CatalogJson.effort)
    val asked = ran.pin match {
      case Right(Some(p)) =>
        val effort = p.assignment.effort match {
          case Some(e) => CatalogJson.effort(e)
          case None if p.assignment.ref.upstream.contains(Upstream.AnthropicApi) =>
            s"${CatalogJson.effort(AnthropicProvider.DefaultEffort)} (grit's default)"
          case None => "none"
        }
        val differs = if (p.assignment.effort == settings.effort) "" else s" (set $set)"
        s"grade ${p.grade.word}, effort $effort$differs"
      case Right(None) => s"effort not recorded (set $set)"
      case Left(_) => s"effort not read (set $set)"
    }
    val setReplay = settings.replay.map(Arm.Settings.replayWord)
    def setBeside(sent: Option[String]) =
      setReplay.filterNot(sent.contains).fold("")(s => s" (set $s)")
    val replay = ran.pin match {
      case Right(Some(p)) =>
        val sent = Arm.Settings.replayWord(p.settings.replay)
        s"replay $sent${setBeside(Some(sent))}"
      case Right(None) => s"replay not recorded${setBeside(None)}"
      case Left(_) => s"replay not read${setBeside(None)}"
    }
    val window = Tokens.value(settings.window.getOrElse(LinearAssembler.DefaultBudget))
    val limit = ran.inputLimit match {
      case Right(Some(l)) => s"${Tokens.value(l)} tokens"
      case Right(None) => "not recorded"
      case Left(_) => "not read"
    }
    val ended = ran.endedBy match {
      case Right(Some(TurnRecord.EndedBy.Budget)) => "last turn ended by its call budget"
      case Right(Some(TurnRecord.EndedBy.InputLimit)) => "last turn ended by its input limit"
      case Right(Some(TurnRecord.EndedBy.Reply)) => "last turn ended by its reply"
      case Right(None) => "last turn's end not recorded"
      case Left(_) => "last turn's end not read"
    }
    s"under: $asked; $replay; window $window tokens; input limit $limit; $ended"
  }

  /** What a person reads of what `consumed` spent, a line each: each call's tokens, then its cost
    * in cents by grit's rates ([[Consumed.gritCost]]) and, for a run `arm` is Claude Code's, by
    * Claude Code's own estimate; for grit's, how many of its calls were priced by the figure
    * their provider reported ([[Call]]'s `reported`), to two significant digits, or by neither;
    * then its thinking ([[thinking]]). Printed for a trial, and for a run refused one.
    */
  def spending(consumed: Consumed, arm: Arm): Vector[String] = {
    val grit = consumed.gritCost.fold(
      reason => s"grit's rates: $reason",
      c => s"${Cost.cents(c)} by grit's rates"
    )
    val claimed = consumed.ended.flatMap(_.claimedUsd).fold("unknown")(Cost.cents)
    consumed.calls.zipWithIndex.map((c, i) => s"call ${i + 1} ${c.id}: ${input(c.usage)}") :+
      (arm match {
        case _: Arm.Grit =>
          val calls = consumed.calls
          val unpriced = calls.count(_.usage.costUsd.isEmpty)
          val reported = calls.count(_.reported)
          val cents = Cost.cents(calls.flatMap(_.usage.costUsd).sum)
          val printed = "printed to two significant digits"
          if (unpriced > 0)
            s"cost: unknown, $unpriced of ${calls.size} calls priced neither by grit's rates " +
              "nor by their provider"
          else if (reported == 0) s"cost: $grit"
          else if (reported == calls.size)
            s"cost: $cents as the provider reported each call, $printed"
          else
            s"cost: $cents, $reported of ${calls.size} calls as their provider reported them, " +
              s"$printed, the rest by grit's rates"
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
    listed(root, "work root").map { entries =>
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
    * the same way; one that wrote no session is a `Left`, as is a `cap` whose pair is not at the
    * Anthropic API, before anything runs. A run that changed no source line of the task's base
    * is a trial not passed, whatever its checks say. `output`, the run's log, is begun
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
    cap.claudeModel
      .toRight(s"Claude Code runs a Claude model at the Anthropic API, not ${cap.pair}")
      .flatMap { model =>
        tried(work)(repository, task, arm, cap, dir, output, () => None) { checkout =>
          for {
            streamed <- configured { variables =>
              handing
                .started(dir, key, variables) { file =>
                  for {
                    _ <- Either.cond(!file.contains("'"), (), s"$file cannot be quoted")
                    argv <- Watched.limited(
                      Longest,
                      arm.claude +: arguments(arm, model, cap, task, s"cat '$file'")*
                    )
                  } yield argv
                }(s =>
                  timed(clock)(Watched.ran(s, quiet, Longest + 2.minutes, _ => None, () => None))
                )
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
          } yield Coded(session, consumed, lines, wall, stopped, RanUnder.Unrecorded)
        }
      }

  /** The grit coder, as `arm` runs `grit work` under `cap`, on `task` in a new checkout at `dir`
    * of the repository at `repository`, as [[trial]] runs Claude Code's: the checks run after it,
    * the diff measured, the checkout tidied, and a quiet run stopped. Before grit starts, the
    * checkout compiles every module the task's checks name, unbilled, for at most [[Warming]],
    * its Mill daemon left warm for the coder; a checkout that does not compile is a `Left` saying
    * so, grit never started. Its brief is `arm`'s standing text, [[GritPreamble]] and the task's
    * brief, in a file of its own outside the checkout;
    * grit starts from an empty environment, but for `arm`'s passed variables and those that
    * point it at its database, `cap`'s pair in every role it sets ([[Caps.pair]]), its
    * allowlisted tools and scout, `cap`'s turns as its rounds, the linear assembler, `arm`'s
    * settings ([[Arm.Settings]]), and `key` as [[Caps.key]] names it, read from its file. A run whose calls reach `cap`'s budget is
    * stopped, a trial not passed, as is one whose processes, all those working in the checkout
    * ([[Memory.held]]), hold more than `memoryCap` allows or leave the machine under its floor
    * ([[MemoryCap.stops]]), or one whose turn did not succeed; that budget holds for the compile
    * before grit, a `Left` naming it, and for the checks as [[Checks.run]] says; its task, every
    * turn of it, is read from `turns` once it has ended. A command it left running dies with it, and its Mill daemon is
    * shut down. What grit wrote to its output, then the tail of its errors, is written to
    * `output` once it has ended, whatever becomes of the trial, and once its turn is read, what
    * it ran under ([[Tried.under]]). `Left` for fewer than 2 turns, or
    * a model at the Anthropic API [[AnthropicRates]] has no rates for, before anything runs; when `memory` cannot be
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
                    Vector("sh", "-c", handed(cap.key), "sh", file, arm.launcher, "work", brief)*
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
          _ <- written(output, append = true)(
            Vector(s"[rig] ${Tried.under(arm.settings, turn.ranUnder)}")
          )
          reported = ended.fold(_ => turn, r => Consumed.reported(turn, r.exit, r.output.text))
          consumed = Consumed.ofTurn(lines, reported, stopped)
          session <- SessionId.of(Harness.Grit, turn.session)
        } yield Coded(
          session,
          consumed,
          lines ++ turn.rendered.linesIterator,
          wall,
          stopped,
          turn.ranUnder
        )
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

  /** `Right` when grit can run under `cap`: at least 2 rounds, and at the Anthropic API a model
    * whose calls [[AnthropicRates]] prices, since the cap is kept by that price; elsewhere each
    * call is priced as it answers, by the cost its provider reported ([[spent]]).
    */
  private def gritCapped(cap: Caps): Either[String, Unit] =
    for {
      _ <- Either.cond(
        cap.maxRounds >= 2,
        (),
        s"grit takes at least 2 rounds, its first call and one after a tool's; not ${cap.maxRounds}"
      )
      _ <- cap.claudeModel
        .filter(AnthropicRates.pricing(_).isEmpty)
        .map(m => s"grit's rates price no model $m, so its cap could not stop it")
        .toLeft(())
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

  /** Why a grit run whose output so far is `lines` is stopped under `cap`: a call priced neither
    * by [[AnthropicRates]] nor by the cost its provider reported ([[Consumed.gritCalls]]), since
    * the cap cannot hold past it; or its calls cost its budget or more. `None` while they cost
    * less.
    */
  private def spent(cap: Caps)(lines: Vector[String]): Option[String] = {
    val calls = Consumed.gritCalls(lines)
    val cost = calls.flatMap(_.usage.costUsd).sum
    calls
      .find(_.usage.costUsd.isEmpty)
      .map(c => s"a call of ${c.model} reported no cost, so its cap could not hold")
      .orElse(
        Option.unless(cap.budget.admits(Cost.Exact(cost)))(
          s"its calls cost ${Cost.cents(cost)}, reaching its cap of ${Cost.cents(cap.budget.usd)}"
        )
      )
  }

  /** `env -i` and the variables grit runs with, as `name=value` arguments, as [[gritTrial]]
    * says; no key among them.
    */
  private[bench] def gritEnvironment(arm: Arm.Grit, cap: Caps): Vector[String] = {
    val model = ModelId.value(cap.model)
    val upstream = Upstream.value(cap.upstream)
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
      "GRIT_TOOL_ROUNDS" -> cap.maxRounds.toString,
      "GRIT_ASSEMBLER" -> "linear",
      "GRIT_SESSION" -> "bench",
      "GRIT_SWEEP" -> "24h"
    ) ++ roles ++ settings(arm.settings)).map((k, v) => s"$k=$v")
  }

  /** The variables `s` sets, each it leaves unset left out. */
  private def settings(s: Arm.Settings): Vector[(String, String)] =
    s.window.map(w => "GRIT_WINDOW_TOKENS" -> Tokens.value(w).toString).toVector ++
      s.workTurns.map(n => "GRIT_WORK_TURNS" -> n.toString) ++
      s.effort.toVector.flatMap(e =>
        Vector(ModelRole.Turn, ModelRole.Dream).map(_.effortVar -> CatalogJson.effort(e))
      ) ++
      s.replay.map(r => OpenRouterConfig.ReplayVar -> CatalogJson.replay(r))

  /** The script `sh -c` runs to start grit with its key: the key read from the file its first
    * argument names into `key`, then the rest of its arguments run in its place, so the key is in
    * no program's arguments.
    */
  private def handed(key: Variable): String = {
    val k = Variable.value(key)
    s"""$k="$$(cat "$$1")"; export $k; shift; exec "$$@""""
  }

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
    * known diff. It passes only when its checks pass, its run ended in no error and it changed
    * a source line ([[DiffLines]]). Tidied as [[trial]] says.
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
              // A tree left at its base passes no task, though the task's checks are green there.
              passed = checked.passed && c.consumed.ended.exists(!_.error) &&
                changed.lines.nonEmpty,
              distance = Option.when(changed.lines.nonEmpty)(DiffLines.distance(changed, known)),
              standing = Standing.ofArm(arm).map(_.sha256)
            )
            .left
            .map(refused => (Tried.spending(c.consumed, arm) :+ refused).mkString("\n"))
        } yield Tried(
          trial,
          c.transcript,
          diff,
          c.consumed,
          checked,
          c.stopped,
          arm,
          cap.pair,
          c.ranUnder
        )
      )
    } yield tried

  /** What a coder's run gave: its session, what it consumed, its transcript, how long it ran,
    * why it was stopped early, if it was, and what its turn recorded it ran under.
    */
  private final case class Coded(
      session: SessionId,
      consumed: Consumed,
      transcript: Vector[String],
      wall: FiniteDuration,
      stopped: Option[String],
      ranUnder: RanUnder
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

  /** The entries of the directory `root`, the `what` a prune clears; or why it could not be
    * read: `the <what> <root> could not be read`, naming the exception's class.
    */
  private[harness] def listed(root: Path, what: String): Either[String, Vector[Path]] =
    Try(Files.list(root)).toEither.left
      .map(e => s"the $what $root could not be read: ${e.getClass.getName}")
      .map { listing =>
        try listing.iterator.asScala.toVector
        finally listing.close()
      }

  /** Removes `dir` and all in it, deepest first, each path in its own `Try` so that one that
    * cannot go does not keep the rest. A symbolic link is removed as a link, never followed.
    */
  private[harness] def removeTree(dir: Path): Unit =
    Try(Files.walk(dir)).foreach { walk =>
      try
        walk.toList.asScala.reverse.foreach { p =>
          val _ = Try(Files.deleteIfExists(p))
        }
      finally walk.close()
    }

  /** `claude`'s arguments for `task` under `cap`, its program left off; `model` its model, as
    * `claude --model` takes it; `helper` its `apiKeyHelper`.
    */
  private def arguments(
      arm: Arm.ClaudeCode,
      model: String,
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
      model,
      "--max-budget-usd",
      cap.budget.usd.toString,
      // Claude Code's "agentic turn" is a round: its --max-turns and the bench's --max-rounds
      // count the same model calls.
      "--max-turns",
      cap.maxRounds.toString
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
