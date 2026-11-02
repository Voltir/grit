package grit.tools

import scala.concurrent.duration.*

import grit.core.host.{
  Allowlist,
  Allowlisted,
  Argv,
  Clipped,
  Edits,
  HostError,
  Lines,
  Ran,
  RelPath,
  Replace,
  RunId,
  RunStatus,
  Runs,
  Shell,
  Workspace
}
import grit.core.place.Place
import grit.core.session.Event
import grit.core.session.Event.Unrecorded
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  SetRefusal,
  Tool,
  ToolName,
  ToolSpec,
  Toolbox
}
import grit.core.visibility.Integrity

/** The coding tool set: reading, searching, changing and running things in a checkout. Each
  * tool's description is all the model is told of it, so it states the tool's limits and
  * every failure the model can see. `write`, `edit` and `run` ask a person first in [[all]], and
  * nobody in [[allowlisted]] (ADR 0055), where `stop` joins them; either way they are bound only
  * to a call stamped at [[Integrity.Member]] or above. Every tool's results are believed at [[Integrity.Member]]:
  * the checkout is its operator's own.
  */
object Coding {

  type ReadArgs = (path: RelPath, offset: Option[Int], limit: Option[Int])
  type ListArgs = (path: RelPath, depth: Int)
  type SearchArgs = (pattern: String, path: RelPath)
  type WriteArgs = (path: RelPath, content: String)
  type EditArgs = (path: RelPath, edits: List[Replace])
  type RunArgs = (command: String, timeout: FiniteDuration)
  type ArgvArgs = (argv: Argv, timeout: FiniteDuration, wait: FiniteDuration)
  type TailArgs = (id: RunId, wait: FiniteDuration)
  type StopArgs = (id: RunId)

  /** The seconds a command the gated `run` starts may run when the call gives no `timeout`. */
  val DefaultTimeout: FiniteDuration = 60.seconds

  /** The most arguments an allowlisted `run` passes a program. */
  val MaxArgs = 256

  /** The deepest `list` goes. */
  val MaxDepth = 5

  /** The most lines of each side of an edit, or of a written file, a person is shown when
    * asked to approve it.
    */
  val Preview = 12

  /** Every tool as the engine offers it, in the order [[all]] offers them: described, never
    * run. An edge runs them.
    */
  lazy val hosted: Vector[Tool.Offered] =
    Vector(readHosted, listHosted, searchHosted, writeHosted, editHosted, runHosted)

  /** The first three of [[hosted]]: those that only read. */
  lazy val readOnlyHosted: Vector[Tool.Offered] = Vector(readHosted, listHosted, searchHosted)

  /** `read`, `list` and `search`; `Left` names a tool that cannot be offered with the others. */
  def readOnly(ws: Workspace^): Either[SetRefusal, Toolbox[caps.CapSet^{ws}]] =
    Toolbox.of(read(ws), list(ws), search(ws))

  /** Every tool: `read`, `list`, `search`, `write`, `edit` and `run`; `Left` names a tool
    * that cannot be offered with the others.
    */
  def all(
      ws: Workspace^,
      edits: Edits^,
      shell: Shell^
  ): Either[SetRefusal, Toolbox[caps.CapSet^{ws, edits, shell}]] =
    Toolbox.of(read(ws), list(ws), search(ws), write(edits), edit(edits), run(shell))

  /** `read`, `list`, `search`, `write`, `edit`, `run`, `tail` and `stop` as an allowlist offers
    * them, in that order: described, never run; an edge runs them ([[allowlisted]]). None asks
    * first; `write`, `edit`, `run` and `stop` are still bound only to a call stamped at
    * [[Integrity.Member]] or above. `run` takes a program and its arguments, no shell line, its
    * description lists what `allowlist` permits, and its `timeout` is at most the allowlist's
    * longest; a command still going after its `wait` answers with an id `tail` and `stop` take.
    */
  def allowlistedHosted(allowlist: Allowlist): Vector[Tool.Offered] =
    Vector(
      readHosted,
      listHosted,
      searchHosted,
      freeWrite,
      freeEdit,
      argvRunHosted(allowlist),
      tailHosted,
      stopHosted
    )

  /** The tools [[allowlistedHosted]] describes for `runs.allowlist`, each `run` refused, running
    * nothing, when `runs` does not permit it, and `tail` and `stop` reaching only what `runs`
    * started; `Left` names a tool that cannot be offered with
    * the others.
    */
  def allowlisted(
      ws: Workspace^,
      edits: Edits^,
      runs: Allowlisted^
  ): Either[SetRefusal, Toolbox[caps.CapSet^{ws, edits, runs}]] =
    Toolbox.of(
      read(ws),
      list(ws),
      search(ws),
      writing(freeWrite, edits),
      editing(freeEdit, edits),
      argvRun(runs),
      tail(runs),
      stop(runs)
    )

  /** `read` as the engine offers it: an edge runs it ([[read]]). */
  lazy val readHosted: Hosted[ReadArgs] =
    new Hosted(
      ToolSpec(
        ToolName("read"),
        "Read a UTF-8 text file in the checkout. Shows at most " + limits + ", from " +
          "`offset`; a note after the text says which lines were shown and the offset to " +
          "continue from. Each line is shown as its number, right-aligned, a tab, then its " +
          "text, numbered from 1 as `search` numbers them; the number and tab are not part " +
          "of the file. Fails when the file does not exist, is a " +
          s"directory, is over ${mb(Workspace.MaxFileBytes)} or is not UTF-8, or when " +
          "`offset` is past its end." + pathRule,
        Args
          .of(
            (
              path = Field.text("The file, relative to the checkout's root."),
              offset =
                Field.count("The line to start at, counting from 1.", 1, Int.MaxValue).optional,
              limit = Field.count("The most lines to read.", 1, Clipped.MaxLines).optional
            )
          )
          .refine(a => located(a.path).map(p => (path = p, offset = a.offset, limit = a.limit))),
        // Running it twice leaves what running it once does.
        retry = Retry.Rerun
      ),
      Gate.Free,
      a => RelPath.value(a.path) + a.offset.fold("")(o => s":$o"),
      Believed.At(Integrity.Member)
    ).logging((a, _, _) => Some(Unrecorded.read(Event.Shape.Path, RelPath.value(a.path))))

  def read(ws: Workspace^): Tool[ReadArgs]^{ws} =
    readHosted.over(a =>
      outcome(ws.read(a.path, Lines.of(a.offset.getOrElse(1), a.limit)))(c => c.show)
    )

  /** `list` as the engine offers it: an edge runs it ([[list]]). */
  lazy val listHosted: Hosted[ListArgs] =
    new Hosted(
      ToolSpec(
        ToolName("list"),
        "List the files and directories under a directory of the checkout, as paths " +
          "relative to it, sorted, directories ending in `/`. `depth` 1 lists its own " +
          "entries; each level more goes one directory deeper. Hidden directories (such as " +
          ".git) and node_modules, out and target are listed but not entered. Shows at most " +
          s"$limits. Fails when the directory does not exist or is a file." + pathRule,
        Args
          .of(
            (
              path = Field
                .text("The directory, relative to the checkout's root; `.` for the root.")
                .optional,
              depth =
                Field.count("How many levels down to list; 1 when not given.", 1, MaxDepth).optional
            )
          )
          .refine(a =>
            located(a.path.getOrElse(".")).map(p => (path = p, depth = a.depth.getOrElse(1)))
          ),
        // Running it twice leaves what running it once does.
        retry = Retry.Rerun
      ),
      Gate.Free,
      a => RelPath.value(a.path),
      Believed.At(Integrity.Member)
    ).logging((a, _, _) => Some(Unrecorded.read(Event.Shape.Path, RelPath.value(a.path))))

  def list(ws: Workspace^): Tool[ListArgs]^{ws} =
    listHosted.over(a => outcome(ws.list(a.path, a.depth))(_.show))

  /** `search` as the engine offers it: an edge runs it ([[search]]). */
  lazy val searchHosted: Hosted[SearchArgs] =
    new Hosted(
      ToolSpec(
        ToolName("search"),
        "Find the lines matching a Java regular expression in the files under a path of " +
          "the checkout (a directory, or one file). Each match is shown as " +
          "`path:line: text`, in path order; a match anywhere in the line counts. Skips .env " +
          "files, links, hidden directories (such as .git), node_modules, out and target, " +
          s"files over ${mb(Workspace.MaxFileBytes)} and files that are not UTF-8. A line is " +
          s"cut at ${Workspace.MaxLineChars} characters; at most $limits of matches are " +
          "shown. Says \"No matches.\" when none match. Fails when the pattern is not a " +
          "valid regular expression or the path does not exist." + pathRule,
        Args
          .of(
            (
              pattern = Field.text("The regular expression, in Java's syntax."),
              path = Field
                .text("Where to search, relative to the checkout's root; `.` for all of it.")
                .optional
            )
          )
          .refine(a => located(a.path.getOrElse(".")).map(p => (pattern = a.pattern, path = p))),
        // Running it twice leaves what running it once does.
        retry = Retry.Rerun
      ),
      Gate.Free,
      a => s"\"${a.pattern}\" ${RelPath.value(a.path)}",
      Believed.At(Integrity.Member)
    ).logging((a, _, _) => Some(Unrecorded.read(Event.Shape.Named, a.pattern)))

  def search(ws: Workspace^): Tool[SearchArgs]^{ws} =
    searchHosted.over(a => outcome(ws.search(a.pattern, a.path))(_.show))

  /** `write` as the engine offers it: an edge runs it ([[write]]). */
  lazy val writeHosted: Hosted[WriteArgs] =
    writeAs(Gate.Ask(a => {
      val lines = Clipped.lines(a.content)
      s"Write ${RelPath.value(a.path)} (${count(lines.size, "line")}):\n" +
        preview(lines, "+ ")
    }))

  def write(edits: Edits^): Tool[WriteArgs]^{edits} = writing(writeHosted, edits)

  /** `write` as an allowlisted set offers it: asking no one. */
  private lazy val freeWrite: Hosted[WriteArgs] = writeAs(Gate.Free)

  private def writeAs(gate: Gate[WriteArgs]): Hosted[WriteArgs] =
    new Hosted(
      ToolSpec(
        ToolName("write"),
        "Make `content` the whole of a file in the checkout, creating it and any missing " +
          "directory above it, or replacing what it held. `content` is the file's text " +
          "only: never the line numbers and tabs `read` shows. To change part of a file, " +
          "use `edit`. Fails when the path is a directory or a " +
          "directory on the way is a file." + pathRule,
        Args
          .of(
            (
              path = Field.text("The file, relative to the checkout's root."),
              content = Field.text("The file's whole new text.")
            )
          )
          .refine(a => located(a.path).map(p => (path = p, content = a.content))),
        // Running it twice leaves what running it once does.
        retry = Retry.Rerun,
        action = Some("write files")
      ),
      gate,
      a => RelPath.value(a.path),
      Believed.At(Integrity.Member)
    ).requiring(Integrity.Member)
      .logging((a, _, _) => Some(Unrecorded.edited(RelPath.value(a.path))))

  private def writing(hosted: Hosted[WriteArgs], edits: Edits^): Tool[WriteArgs]^{edits} =
    hosted.over(a =>
      outcome(edits.write(a.path, a.content))(_ =>
        s"Wrote ${RelPath.value(a.path)}: ${count(Clipped.lines(a.content).size, "line")}."
      )
    )

  /** `edit` as the engine offers it: an edge runs it ([[edit]]). */
  lazy val editHosted: Hosted[EditArgs] =
    editAs(Gate.Ask(a => {
      val shown = a.edits.zipWithIndex.map { (e, i) =>
        s"@@ edits[$i]\n" + preview(Clipped.lines(e.oldText), "- ") + "\n" +
          preview(Clipped.lines(e.newText), "+ ")
      }
      s"Edit ${RelPath.value(a.path)} (${count(a.edits.size, "replacement")}):\n" +
        shown.mkString("\n")
    }))

  def edit(edits: Edits^): Tool[EditArgs]^{edits} = editing(editHosted, edits)

  /** `edit` as an allowlisted set offers it: asking no one. */
  private lazy val freeEdit: Hosted[EditArgs] = editAs(Gate.Free)

  private def editAs(gate: Gate[EditArgs]): Hosted[EditArgs] =
    new Hosted(
      ToolSpec(
        ToolName("edit"),
        "Change a file in the checkout by replacing passages of it. Every `oldText` is " +
          "matched against the file as it was before this call, exactly, whitespace and " +
          "line breaks included; each must occur exactly once, and no two may overlap. " +
          "`oldText` and `newText` are the file's text only: leave out the line number and " +
          "tab `read` shows before each line. All " +
          "the edits apply, or none does. Line endings (\\r\\n or \\n) and a byte order " +
          "mark are kept. Fails, naming the edit by its " +
          "index from 0, when an `oldText` is empty, not found (saying so when it carries " +
          "`read`'s line numbers), or found more than once, " +
          "when two edits overlap, or when the edits change nothing; also when the file " +
          s"does not exist, is a directory, is over ${mb(Workspace.MaxFileBytes)} or is not " +
          "UTF-8." + pathRule,
        Args
          .of(
            (
              path = Field.text("The file, relative to the checkout's root."),
              edits = Field.each(
                "The passages to replace, each found exactly once in the file.",
                replacement
              )
            )
          )
          .refine(a =>
            located(a.path)
              .map(p => (path = p, edits = a.edits.map(e => Replace(e.oldText, e.newText))))
          )
      ),
      gate,
      a => RelPath.value(a.path),
      Believed.At(Integrity.Member)
    ).requiring(Integrity.Member)
      .logging((a, _, _) => Some(Unrecorded.edited(RelPath.value(a.path))))

  private def editing(hosted: Hosted[EditArgs], edits: Edits^): Tool[EditArgs]^{edits} =
    hosted.over(a =>
      edits.edit(a.path, a.edits) match {
        case Left(error) => Outcome.Failed(error.message)
        case Right(done) =>
          val on = done.at.mkString(", ")
          Outcome.Done(
            s"Edited ${RelPath.value(a.path)}: ${count(done.at.size, "replacement")}, " +
              s"starting on line${if (done.at.size == 1) "" else "s"} $on."
          )
      }
    )

  /** `run` as the engine offers it: an edge runs it ([[run]]). */
  lazy val runHosted: Hosted[RunArgs] =
    new Hosted(
      ToolSpec(
        ToolName("run"),
        "Run a shell command (sh -c) in the checkout's root, with nothing on its input, " +
          "and return its exit code and its output and errors, interleaved. A non-zero exit " +
          "is returned, not an error. Shows at most the last " + limits + " of output; " +
          "narrow a long output with grep, head or tail. Fails when " +
          s"the command runs past `timeout` (${DefaultTimeout.toSeconds} seconds when not " +
          "given), killing it and everything it started and returning its output so far, " +
          "or when it cannot start.",
        Args
          .of(
            (
              command = Field.text("The command, as sh reads it."),
              timeout = Field
                .count(
                  s"The most seconds it may run; ${DefaultTimeout.toSeconds} when not given.",
                  1,
                  Shell.Longest.toSeconds.toInt
                )
                .optional
            )
          )
          .map(a => (command = a.command, timeout = a.timeout.fold(DefaultTimeout)(_.seconds)))
      ),
      Gate.Ask(a => s"Run in the checkout (timeout ${a.timeout.toSeconds} s):\n${a.command}"),
      a => a.command,
      Believed.At(Integrity.Member)
    ).requiring(Integrity.Member)
      // A command line is content, never a shape; a commit's sha and subject are its own.
      .logging((a, at, shown) =>
        Some(
          committed(at, Commits.findFirstIn(a.command).nonEmpty, shown)
            .getOrElse(Unrecorded.Ran(Event.Shape.Empty))
        )
      )

  def run(shell: Shell^): Tool[RunArgs]^{shell} =
    runHosted.over(a => outcome(shell.run(a.command, a.timeout))(answered))

  /** `run` as an allowlisted set offers it: asking no one, taking an argv, its description
    * listing what `allowlist` permits.
    */
  private def argvRunHosted(allowlist: Allowlist): Hosted[ArgvArgs] = {
    val longest = allowlist.longest
    new Hosted(
      ToolSpec(
        ToolName("run"),
        "Run a program from the list below in the checkout's root, with nothing on its " +
          "input, and return its exit code and its output and errors, interleaved. " +
          "`program` and `args` are passed as they are, with no shell: no pipes, `&&`, " +
          "redirects, globs or variables; use `search` and `read` for what those were for. " +
          "A non-zero exit is returned, not an error. Shows at most the last " + limits +
          " of output. Give `git commit` its message with `-m`: no editor can open. Waits up " +
          s"to `wait` seconds (${Runs.Wait.toSeconds} when not given): a command still running " +
          "then keeps running, and the answer gives its id and its output so far; follow it " +
          s"with `tail`, or end it with `stop`. At most ${Runs.Most} commands run at once in a " +
          "checkout. Fails, running nothing, when the command is not on the list or " +
          s"${Runs.Most} are already running, and when it cannot start. A command is killed " +
          "with everything it started when it runs past `timeout` (at most " +
          s"${longest.toSeconds} seconds, and that when not given) or its output passes " +
          s"${mb(Workspace.MaxFileBytes)}. The commands it runs, where `…` stands for any " +
          "further arguments:\n" +
          allowlist.rules.map(r => s"- `${r.written}`").mkString("\n"),
        Args
          .of(
            (
              program = Field.text("The program, written as the list writes it."),
              args = Field
                .texts("Its arguments, in order, each passed as one word.", 0, MaxArgs)
                .optional,
              timeout = Field
                .count(
                  s"The most seconds it may run, waited on or not; ${longest.toSeconds} when " +
                    "not given.",
                  1,
                  longest.toSeconds.toInt
                )
                .optional,
              wait = Field
                .count(
                  "The most seconds to wait for it to end before answering with its id; " +
                    s"${Runs.Wait.toSeconds} when not given.",
                  0,
                  Shell.Longest.toSeconds.toInt
                )
                .optional
            )
          )
          .refine(a =>
            Argv
              .of(a.program, a.args.getOrElse(Nil)*)
              .left
              .map(why => ArgsError.Refused("program", s"The command cannot run: $why."))
              .map(argv =>
                (
                  argv = argv,
                  timeout = a.timeout.fold(longest)(_.seconds),
                  wait = a.wait.fold(Runs.Wait)(_.seconds)
                )
              )
          )
      ),
      Gate.Free,
      a => a.argv.written,
      Believed.At(Integrity.Member)
    ).requiring(Integrity.Member)
      // An argv is content, never a shape; a commit's sha and subject are its own.
      .logging((a, at, shown) =>
        Some(
          committed(at, commits(a.argv), shown).getOrElse(Unrecorded.Ran(Event.Shape.Empty))
        )
      )
  }

  private def argvRun(runs: Allowlisted^): Tool[ArgvArgs]^{runs} =
    argvRunHosted(runs.allowlist).over(a =>
      runs
        .start(a.argv, a.timeout, a.wait)
        .fold(e => Outcome.Failed(e.message), s => standing(s.id, s.status))
    )

  /** `tail` as an allowlisted set offers it: asking no one, bound at any integrity, as `read`. */
  private lazy val tailHosted: Hosted[TailArgs] =
    new Hosted(
      ToolSpec(
        ToolName("tail"),
        "See how a command `run` left running stands: still running, with its output so far, " +
          "or ended, with its exit code and output. Give `wait` to wait up to that many seconds " +
          "for it to end; the answer comes as soon as it does. Shows at most the last " + limits +
          " of output; when that cuts it, the answer names the file holding all of it, for " +
          "`read`. Fails when no command with that id is kept in this checkout: grit restarted " +
          s"since it started, or it ended and was forgotten after ${Runs.Kept} newer commands " +
          s"ended; and when its output passed ${mb(Workspace.MaxFileBytes)}, which killed it.",
        Args
          .of(
            (
              id = idField,
              wait = Field
                .count(
                  "The most seconds to wait for it to end; 0 when not given.",
                  0,
                  Shell.Longest.toSeconds.toInt
                )
                .optional
            )
          )
          .refine(a =>
            runId(a.id).map(id => (id = id, wait = a.wait.fold(Duration.Zero)(_.seconds)))
          )
      ),
      Gate.Free,
      a => s"tail ${a.id.written}",
      Believed.At(Integrity.Member)
    ).logging((_, at, shown) => endedCommit(at, shown))

  /** `stop` as an allowlisted set offers it: asking no one, bound only at member, as `run`. */
  private lazy val stopHosted: Hosted[StopArgs] =
    new Hosted(
      ToolSpec(
        ToolName("stop"),
        "End a command `run` left running, killing it and everything it started, and return " +
          "its output until then. A command that already ended is left as it was, and its exit " +
          "code and output returned. Fails as `tail` does when no command with that id is kept.",
        Args.of((id = idField)).refine(a => runId(a.id).map(id => (id = id)))
      ),
      Gate.Free,
      a => s"stop ${a.id.written}",
      Believed.At(Integrity.Member)
    ).requiring(Integrity.Member)
      .logging((_, at, shown) => endedCommit(at, shown))

  private def tail(runs: Allowlisted^): Tool[TailArgs]^{runs} =
    tailHosted.over(a =>
      runs
        .tail(a.id, a.wait)
        .fold(e => Outcome.Failed(e.message), s => named(a.id, s.argv, s.status))
    )

  private def stop(runs: Allowlisted^): Tool[StopArgs]^{runs} =
    stopHosted.over(a =>
      runs.stop(a.id).fold(e => Outcome.Failed(e.message), s => named(a.id, s.argv, s.status))
    )

  /** The `id` field `tail` and `stop` take. */
  private val idField = Field.text("The id `run` answered with, such as `3f9a2c1e`.")

  /** `raw` as a run's id, refused as the `id` argument. */
  private def runId(raw: String): Either[ArgsError, RunId] =
    RunId.parse(raw).left.map(why => ArgsError.Refused("id", why))

  /** How the run `id` stands, as the model reads it, after `lead`. A run past its timeout
    * fails, as one that timed out within the call always has.
    */
  private def standing(id: RunId, status: RunStatus, lead: String = ""): Outcome = status match {
    case RunStatus.Going(elapsed, timeout, output) =>
      val so =
        if (output.show.isEmpty) " No output yet."
        else s" Its output so far:\n${output.show.stripSuffix("\n")}"
      Outcome.Done(
        s"${lead}Still running after ${elapsed.toSeconds} seconds, as `${id.written}`; it is " +
          s"killed at ${timeout.toSeconds} seconds.$so\n\nCall `tail` with id `${id.written}` to " +
          "see how it stands, giving `wait` to wait for it to end, or `stop` to end it."
      )
    case RunStatus.Ended(ran) => Outcome.Done(lead + answered(ran))
    case RunStatus.TimedOut(after, output) =>
      Outcome.Failed(lead + HostError.TimedOut(after, output).message)
    case RunStatus.Stopped(after, output) =>
      Outcome.Done(
        if (output.show.isEmpty) s"${lead}Stopped after ${after.toSeconds} seconds; no output."
        else
          s"${lead}Stopped after ${after.toSeconds} seconds. Its output until then:\n${output.show}"
      )
  }

  /** [[standing]] after a first line naming the command `argv`, as `tail` and `stop` answer. */
  private def named(id: RunId, argv: Argv, status: RunStatus): Outcome =
    standing(id, status, s"`${argv.written}`\n")

  /** The commit an answer of `tail` or `stop` at `at` shows its command made: `shown`'s first
    * line names a `git commit`, and the rest is a run's answer that [[committed]] reads one from.
    * Any other answer logs nothing: `run` logged the command.
    */
  private def endedCommit(at: Place, shown: String): Option[Unrecorded] =
    shown.split("\n", 2).toList match {
      case head :: rest :: Nil =>
        committed(at, Commits.findFirstIn(head.stripPrefix("`").stripSuffix("`")).nonEmpty, rest)
      case _ => None
    }

  /** A finished command as the model reads it: its exit code, then its output. */
  private def answered(ran: Ran): String = {
    val out = ran.output.show
    if (out.isEmpty) s"$ExitLead${ran.exit}; no output."
    else s"$ExitLead${ran.exit}. Output:\n$out"
  }

  /** `argv` runs `git commit`, with any `-C <dir>` before it. */
  private def commits(argv: Argv): Boolean =
    argv.program == "git" && (argv.args match {
      case "commit" :: _ => true
      case "-C" :: _ :: "commit" :: _ => true
      case _ => false
    })

  /** `Exit code `: what a run's answer begins with, before its exit code. */
  private val ExitLead: String = "Exit code "

  /** `command` runs `git commit` (first, or after `;`, `&&` or `|`, with any `-C <dir>`). */
  private val Commits = """(?:^|[;&|])\s*git\s+(?:-C\s+\S+\s+)?commit\b""".r

  /** git's line for a commit it made: `[<branch>[ (root-commit)] <sha>] <subject>`. */
  private val Made = """(?m)^\[\S+(?: \(root-commit\))? ([0-9a-f]{7,40})\] (.+)$""".r

  /** The commit a run at the worktree `at` made, read from its answer `shown`: when the command
    * `commits` (runs `git commit`), it exited 0, and git's line for the commit is in its output.
    */
  private def committed(at: Place, commits: Boolean, shown: String): Option[Unrecorded] =
    if (!commits || !Seq(".", ";").exists(e => shown.startsWith(s"${ExitLead}0$e"))) None
    else
      Made
        .findFirstMatchIn(shown)
        .flatMap(m => Event.Shape.committed(at, m.group(1), Some(m.group(2))).toOption)
        .map(Unrecorded.Commit(_))

  /** One of `edit`'s `edits`. A value of its own: built inline inside the outer `Args.of`,
    * its tuple gets a capture-set variable and `Args.Values` does not reduce.
    */
  private val replacement = Args.of(
    (
      oldText = Field.text(
        "The exact text to replace, as it is in the file, without `read`'s line numbers."
      ),
      newText = Field.text("What replaces it.")
    )
  )

  private val limits =
    s"${Clipped.MaxLines} lines and ${Clipped.MaxBytes / 1024} KB"

  private val pathRule =
    " A path must be relative to the checkout's root and stay inside it, and may not " +
      "name a .env file (.env or .env.*); a link that leads outside the checkout or to " +
      "such a file is refused."

  private def mb(bytes: Long): String = s"${bytes / (1024 * 1024)} MB"

  /** `raw` as a path in the checkout, refused as the `path` argument. */
  private def located(raw: String): Either[ArgsError, RelPath] =
    RelPath.of(raw).left.map(e => ArgsError.Invalid("path", PathAccepts, s"`$raw` (${e.message})"))

  private val PathAccepts =
    "a path relative to the checkout's root that stays inside it and names no .env file"

  private def outcome[A](result: Either[HostError, A])(done: A -> String): Outcome =
    result.fold(e => Outcome.Failed(e.message), a => Outcome.Done(done(a)))

  private def count(n: Int, noun: String): String = s"$n $noun${if (n == 1) "" else "s"}"

  /** At most [[Preview]] of `lines`, each after `mark`, then how many were left out. */
  private def preview(lines: Vector[String], mark: String): String = {
    val kept = lines.take(Preview).map(mark + _)
    val left = lines.size - kept.size
    (if (left > 0) kept :+ s"  … ${count(left, "more line")}" else kept).mkString("\n")
  }

}
