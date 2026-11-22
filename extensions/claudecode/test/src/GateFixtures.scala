package grit.claudecode

import java.nio.file.{Files, Path}

/** A checkout holding the real scripts/check and scripts/gate-stamp, every program they run
  * (Mill, the law, the tier scripts, scripts/it) a stub that notes its arguments in its own log
  * under the machine's `HOME`, and a fake `gh` in the machine's `bin`. Its `main` holds one
  * commit; `work`, checked out, one more. origin is a GitHub address never reached: its
  * remote-tracking branches are set by hand.
  *
  * The stubs answer as these environment variables say: `FAKE_MILL_FAILS`, a Mill task whose call
  * exits 1 (`show` fails the module graph); `FAKE_LAW_COMMITS`, the law commits; `FAKE_LAW_DIRTIES`,
  * the law leaves an untracked file; `FAKE_GH_FAILS`, gh's every call prints it and exits 1.
  */
final class GateFixtures(val machine: JoinFixtures) {

  private def env(name: String): Path =
    Path.of(sys.env.getOrElse(name, throw new java.lang.AssertionError(s"no $name")))

  /** The module graph the stub Mill shows: an `it` module over grit.dbos, another over grit.app,
    * which depends on grit.dbos, and grit.claudecode, which reaches neither.
    */
  private val Graph: Vector[String] = Vector(
    "grit.dbos core/dbos",
    "grit.dbos.it core/dbos/it grit.dbos",
    "grit.app deployments/app grit.dbos",
    "grit.app.it deployments/app/it grit.app grit.dbos.it",
    "grit.claudecode extensions/claudecode",
    "grit.claudecode.test extensions/claudecode/test grit.claudecode"
  )

  val dir: Path = Files.createDirectories(machine.root.resolve("repo"))

  /** The address origin is given. */
  val Origin: String = "git@github.com:someone/somerepo.git"

  private def stub(name: String, more: String = ""): Path =
    machine.write(
      dir.resolve(name),
      s"#!/bin/sh\nprintf '%s\\n' \"$$*\" >> \"$$HOME/${name.replace('/', '-')}.log\"\n$more\nexit 0\n",
      executable = true
    )

  locally {
    val _ = machine.write(dir.resolve("scripts/check"), Files.readString(env("GRIT_CHECK")), true)
    val _ = machine.write(
      dir.resolve("scripts/gate-stamp"),
      Files.readString(env("GRIT_GATE_STAMP")),
      true
    )
    val graph = Graph.map(g => s"\"$g\"").mkString("[", ",", "]")
    val _ = stub(
      "mill",
      s"""case " $$* " in *" $${FAKE_MILL_FAILS:-none} "*) exit 1 ;; esac
         |case "$$*" in *"show grit.moduleGraph"*) echo '$graph' ;; esac""".stripMargin
    )
    val _ = stub(
      "scripts/enola-law.sh",
      """[ -z "${FAKE_LAW_COMMITS:-}" ] || git commit -q --allow-empty -m moved
        |[ -z "${FAKE_LAW_DIRTIES:-}" ] || touch left-behind""".stripMargin
    )
    Vector("format-check", "clock-check", "unsafe-check", "doc-length", "tui-gate", "it")
      .foreach(s => stub(s"scripts/$s"))
    // A test module, so that a target names one; the groups scripts/check walks.
    val _ = machine.write(dir.resolve("extensions/claudecode/test/src/Keep.scala"), "\n")
    Vector("core", "kit", "deployments", "eval").foreach(g =>
      Files.createDirectories(dir.resolve(g))
    )
    val _ = machine.write(dir.resolve(".gitignore"), "out/\n")
    val _ = machine.write(
      machine.bin.resolve("gh"),
      s"""#!/bin/sh
         |printf '%s|%s\\n' "$${GITHUB_TOKEN:-}$${GH_TOKEN:-}" "$$*" >> "$$HOME/gh.log"
         |[ -z "$${FAKE_GH_FAILS:-}" ] || { echo "$$FAKE_GH_FAILS" >&2; exit 1; }
         |case "$$*" in
         |  *' -X POST '*) echo '{}' ;;
         |  *) cat "$$HOME/gh-status.json" 2>/dev/null || echo '{"statuses":[]}' ;;
         |esac
         |""".stripMargin,
      executable = true
    )
    val _ = machine.sh(
      dir,
      s"""git init -q -b main . && git config user.name t && git config user.email t@example.invalid &&
         |git add -A && git commit -q -m first && git checkout -q -b work &&
         |git remote add origin '$Origin'""".stripMargin
    )
  }

  /** `work` given one commit holding `files`, each a path and its text. */
  def change(files: (String, String)*): String = {
    files.foreach((p, text) => machine.write(dir.resolve(p), text))
    sh("git add -A && git commit -q -m change && git rev-parse HEAD").mkString
  }

  /** origin's `branch` set to `sha`, as a fetch would leave it. */
  def remote(branch: String, sha: String): Unit = {
    val _ = sh(s"git update-ref refs/remotes/origin/$branch $sha")
  }

  /** bash running `script` in the checkout, failing the test unless it exits 0. */
  def sh(script: String): Vector[String] = machine.sh(dir, script)

  /** A commit's SHA, in full. */
  def sha(rev: String): String = sh(s"git rev-parse $rev").mkString

  /** A commit's short SHA, as git abbreviates it here. */
  def short(rev: String): String = sh(s"git rev-parse --short $rev").mkString

  /** scripts/check run with `args`, given `more` in its environment. */
  def check(args: Seq[String], more: Map[String, String] = Map.empty): Bash.Done =
    Bash.run(dir, machine.home, machine.bin, "scripts/check" +: args, env = more)

  /** scripts/gate-stamp run with `args`, given `more` in its environment. */
  def stamp(args: Seq[String], more: Map[String, String] = Map.empty): Bash.Done =
    Bash.run(dir, machine.home, machine.bin, "scripts/gate-stamp" +: args, env = more)

  /** The gate's record, or `None` when there is none. */
  def record: Option[String] = machine.read(dir.resolve("out/check/passed"))

  /** Each call a stub made, its arguments joined by spaces: `name` as written, a path's slashes
    * dashes.
    */
  def calls(name: String): Vector[String] =
    machine.read(machine.home.resolve(s"$name.log")).fold(Vector.empty)(_.linesIterator.toVector)
}

object GateFixtures {

  /** `body` over fixtures on a fresh temporary root. */
  def apply[A](body: GateFixtures => A): A = JoinFixtures(m => body(new GateFixtures(m)))
}
