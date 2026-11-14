package grit.claudecode

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

/** A temporary machine for the join: a `HOME` of its own, a `bin` holding a fake `claude` that
  * notes each call's arguments in `claude.log` and reads all of its input, and a git checkout.
  */
final class JoinFixtures(val root: Path) {

  val home: Path = Files.createDirectories(root.resolve("home"))
  val bin: Path = Files.createDirectories(root.resolve("bin"))

  /** The address every join here is given. */
  val Edge: String = "http://127.0.0.1:7317"

  /** The MCP name every join here registers: no deployment's. */
  val Name: String = "work-edge"

  locally {
    write(
      bin.resolve("claude"),
      "#!/bin/sh\nprintf '%s\\n' \"$*\" >> \"$HOME/claude.log\"\ncat > /dev/null\n" +
        "[ \"$1 $2\" = 'mcp add' ] && [ -n \"${FAKE_CLAUDE_ADD_FAILS:-}\" ] && exit 1\nexit 0\n",
      executable = true
    )
  }

  /** `path` holding `text`, executable when asked. */
  def write(path: Path, text: String, executable: Boolean = false): Path = {
    Files.createDirectories(path.getParent)
    Files.writeString(path, text, UTF_8)
    if (executable)
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
    path
  }

  /** `path`'s text, or `None` when nothing is there. */
  def read(path: Path): Option[String] =
    Option.when(Files.exists(path, java.nio.file.LinkOption.NOFOLLOW_LINKS))(
      Files.readString(path, UTF_8)
    )

  /** bash running `script` in `dir`, failing the test unless it exits 0. */
  def sh(dir: Path, script: String): Vector[String] = {
    val done = Bash.run(dir, home, bin, Seq("-c", script))
    if (done.exit != 0)
      throw new java.lang.AssertionError(s"`$script` exited ${done.exit}: ${done.err}")
    done.out
  }

  /** A new git checkout at `name` under the root, with one commit. */
  def checkout(name: String): Path = {
    val dir = Files.createDirectories(root.resolve(name))
    val _ = sh(
      dir,
      "git init -q . && git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m first"
    )
    dir
  }

  /** The join script piped into bash in `dir` with `args`, as `curl … | bash -s -- args` runs it. */
  def join(dir: Path, args: Seq[String], env: Map[String, String] = Map.empty): Bash.Done =
    Bash.run(dir, home, bin, "-s" +: "--" +: args, ClaudeCode.adapter.join, env)

  /** The join of `dir` to [[Edge]] as [[Name]]. */
  def joined(dir: Path, more: String*): Bash.Done = join(dir, Seq(Edge, "--mcp-name", Name) ++ more)

  /** Its undo. */
  def undone(dir: Path): Bash.Done = join(dir, Seq("--undo", "--mcp-name", Name))

  /** `dir`'s git common directory, absolute. */
  def common(dir: Path): Path =
    Path.of(sh(dir, "git rev-parse --path-format=absolute --git-common-dir").mkString)

  /** The hook a join of `dir` installs. */
  def hook(dir: Path): Path = common(dir).resolve("grit/claude-code/hook")

  /** `dir`'s local Claude Code settings. */
  def settings(dir: Path): Path = dir.resolve(".claude/settings.local.json")

  /** The git hook `name` of `dir`'s repository. */
  def gitHook(dir: Path, name: String): Path = common(dir).resolve("hooks").resolve(name)

  /** Each call the fake `claude` was given, its arguments joined by spaces. */
  def claudeCalls: Vector[String] =
    read(home.resolve("claude.log")).fold(Vector.empty[String])(_.linesIterator.toVector)

  /** Every file under `dir`, relative, with its text: what a join may have changed. */
  def tree(dir: Path): Map[String, String] =
    Files
      .walk(dir)
      .iterator()
      .asScala
      .filter(p => Files.isRegularFile(p, java.nio.file.LinkOption.NOFOLLOW_LINKS))
      .map(p =>
        dir.relativize(p).toString -> Files
          .readString(p, java.nio.charset.StandardCharsets.ISO_8859_1)
      )
      .toMap
}

object JoinFixtures {

  /** `body` over fixtures on a fresh temporary root. */
  def apply[A](body: JoinFixtures => A): A = Bash.inTemp(root => body(new JoinFixtures(root)))
}
