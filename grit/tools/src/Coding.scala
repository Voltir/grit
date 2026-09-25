package grit.tools

import scala.concurrent.duration.*

import grit.core.host.{
  Clipped,
  Edits,
  HostError,
  Lines,
  RelPath,
  Replace,
  Shell,
  Workspace
}
import grit.core.tool.{
  Args,
  ArgsError,
  DuplicateName,
  Field,
  Gate,
  Outcome,
  Tool,
  ToolName,
  ToolSpec,
  Toolbox
}

/** The coding tool set: reading, searching, changing and running things in a checkout. Each
  * tool's description is all the model is told of it, so it states the tool's limits and
  * every failure the model can see. `write`, `edit` and `run` ask a person first.
  */
object Coding {

  type ReadArgs = (path: RelPath, offset: Option[Int], limit: Option[Int])
  type ListArgs = (path: RelPath, depth: Int)
  type SearchArgs = (pattern: String, path: RelPath)
  type WriteArgs = (path: RelPath, content: String)
  type EditArgs = (path: RelPath, edits: List[Replace])
  type RunArgs = (command: String, timeout: FiniteDuration)

  /** The seconds a command may run when the call gives no `timeout`. */
  val DefaultTimeout: FiniteDuration = 60.seconds

  /** The most seconds a call may give a command. */
  val MaxTimeout: FiniteDuration = 600.seconds

  /** The deepest `list` goes. */
  val MaxDepth = 5

  /** The most lines of each side of an edit, or of a written file, a person is shown when
    * asked to approve it.
    */
  val Preview = 12

  /** `read`, `list` and `search`; `Left` names a tool offered twice. */
  def readOnly(ws: Workspace^): Either[DuplicateName, Toolbox[{ws}]] =
    Toolbox.of(read(ws), list(ws), search(ws))

  /** Every tool: `read`, `list`, `search`, `write`, `edit` and `run`; `Left` names a tool
    * offered twice.
    */
  def all(
      ws: Workspace^,
      edits: Edits^,
      shell: Shell^
  ): Either[DuplicateName, Toolbox[{ws, edits, shell}]] =
    Toolbox.of(read(ws), list(ws), search(ws), write(edits), edit(edits), run(shell))

  def read(ws: Workspace^): Tool[ReadArgs]^{ws} =
    new Tool(
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
              offset = Field.count("The line to start at, counting from 1.", 1, Int.MaxValue).optional,
              limit = Field.count("The most lines to read.", 1, Clipped.MaxLines).optional
            )
          )
          .refine(a => located(a.path).map(p => (path = p, offset = a.offset, limit = a.limit)))
      ),
      Gate.Free,
      a => RelPath.value(a.path) + a.offset.fold("")(o => s":$o"),
      a =>
        outcome(ws.read(a.path, Lines.of(a.offset.getOrElse(1), a.limit)))(c => c.show)
    )

  def list(ws: Workspace^): Tool[ListArgs]^{ws} =
    new Tool(
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
              path = Field.text("The directory, relative to the checkout's root; `.` for the root.").optional,
              depth = Field.count("How many levels down to list; 1 when not given.", 1, MaxDepth).optional
            )
          )
          .refine(a =>
            located(a.path.getOrElse(".")).map(p => (path = p, depth = a.depth.getOrElse(1)))
          )
      ),
      Gate.Free,
      a => RelPath.value(a.path),
      a => outcome(ws.list(a.path, a.depth))(_.show)
    )

  def search(ws: Workspace^): Tool[SearchArgs]^{ws} =
    new Tool(
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
              path = Field.text("Where to search, relative to the checkout's root; `.` for all of it.").optional
            )
          )
          .refine(a => located(a.path.getOrElse(".")).map(p => (pattern = a.pattern, path = p)))
      ),
      Gate.Free,
      a => s"\"${a.pattern}\" ${RelPath.value(a.path)}",
      a => outcome(ws.search(a.pattern, a.path))(_.show)
    )

  def write(edits: Edits^): Tool[WriteArgs]^{edits} =
    new Tool(
      ToolSpec(
        ToolName("write"),
        "Make `content` the whole of a file in the checkout, creating it and any missing " +
          "directory above it, or replacing what it held. `content` is the file's text " +
          "only: never the line numbers and tabs `read` shows. To change part of a file, " +
          "use `edit`. A person approves each call first; if they decline, nothing is written " +
          "and their reason, if any, is returned. Fails when the path is a directory or a " +
          "directory on the way is a file." + pathRule,
        Args
          .of(
            (
              path = Field.text("The file, relative to the checkout's root."),
              content = Field.text("The file's whole new text.")
            )
          )
          .refine(a => located(a.path).map(p => (path = p, content = a.content)))
      ),
      Gate.Ask(a => {
        val lines = Clipped.lines(a.content)
        s"Write ${RelPath.value(a.path)} (${count(lines.size, "line")}):\n" +
          preview(lines, "+ ")
      }),
      a => RelPath.value(a.path),
      a =>
        outcome(edits.write(a.path, a.content))(_ =>
          s"Wrote ${RelPath.value(a.path)}: ${count(Clipped.lines(a.content).size, "line")}."
        )
    )

  def edit(edits: Edits^): Tool[EditArgs]^{edits} =
    new Tool(
      ToolSpec(
        ToolName("edit"),
        "Change a file in the checkout by replacing passages of it. Every `oldText` is " +
          "matched against the file as it was before this call, exactly, whitespace and " +
          "line breaks included; each must occur exactly once, and no two may overlap. " +
          "`oldText` and `newText` are the file's text only: leave out the line number and " +
          "tab `read` shows before each line. All " +
          "the edits apply, or none does. Line endings (\\r\\n or \\n) and a byte order " +
          "mark are kept. A person approves each call first; if they decline, nothing " +
          "changes and their reason, if any, is returned. Fails, naming the edit by its " +
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
            located(a.path).map(p =>
              (path = p, edits = a.edits.map(e => Replace(e.oldText, e.newText)))
            )
          )
      ),
      Gate.Ask(a => {
        val shown = a.edits.zipWithIndex.map { (e, i) =>
          s"@@ edits[$i]\n" + preview(Clipped.lines(e.oldText), "- ") + "\n" +
            preview(Clipped.lines(e.newText), "+ ")
        }
        s"Edit ${RelPath.value(a.path)} (${count(a.edits.size, "replacement")}):\n" +
          shown.mkString("\n")
      }),
      a => RelPath.value(a.path),
      a =>
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

  def run(shell: Shell^): Tool[RunArgs]^{shell} =
    new Tool(
      ToolSpec(
        ToolName("run"),
        "Run a shell command (sh -c) in the checkout's root, with nothing on its input, " +
          "and return its exit code and its output and errors, interleaved. A non-zero exit " +
          "is returned, not an error. Shows at most the last " + limits + " of output; " +
          "narrow a long output with grep, head or tail. A person approves each call first; " +
          "if they decline, nothing runs and their reason, if any, is returned. Fails when " +
          s"the command runs past `timeout` (${DefaultTimeout.toSeconds} seconds when not " +
          "given), killing it and everything it started and returning its output so far, " +
          "or when it cannot start.",
        Args
          .of(
            (
              command = Field.text("The command, as sh reads it."),
              timeout = Field.count(
                s"The most seconds it may run; ${DefaultTimeout.toSeconds} when not given.",
                1,
                MaxTimeout.toSeconds.toInt
              ).optional
            )
          )
          .map(a =>
            (command = a.command, timeout = a.timeout.fold(DefaultTimeout)(_.seconds))
          )
      ),
      Gate.Ask(a => s"Run in the checkout (timeout ${a.timeout.toSeconds} s):\n${a.command}"),
      a => a.command,
      a =>
        outcome(shell.run(a.command, a.timeout)) { ran =>
          val out = ran.output.show
          if (out.isEmpty) s"Exit code ${ran.exit}; no output."
          else s"Exit code ${ran.exit}. Output:\n$out"
        }
    )

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
    s"${Clipped.MaxLines} lines or ${Clipped.MaxBytes / 1024} KB, whichever comes first"

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
