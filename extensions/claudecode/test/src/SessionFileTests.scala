package grit.claudecode

import java.nio.file.{Files, Path}

import utest.*

/** The hook's session files, against the commands edge: each session's start keeps one beside the
  * hook naming the harness process that runs it, removed when its end is recorded.
  */
object SessionFileTests extends TestSuite {

  import HookFixtures.First

  /** The file the hook keeps for the session `local`, as JSON; none when there is none. */
  private def kept(f: HookFixtures, local: String): Option[ujson.Value] = {
    val file = f.hook.resolveSibling("sessions").resolve(local)
    Option.when(Files.exists(file))(ujson.read(Files.readString(file)))
  }

  /** The text of `/proc/<path>`, its trailing newline dropped. */
  private def proc(path: String): String = Files.readString(Path.of("/proc", path)).trim

  /** Field 22 of the process `pid`'s stat, read after the last `)`: its start, in clock ticks
    * since boot.
    */
  private def began(pid: Long): String =
    proc(s"$pid/stat").split("\\) ").lastOption.flatMap(_.split(' ').lift(19)).getOrElse("")

  /** What the hook keeps for a session in `f`'s checkout run by this JVM, the process that runs
    * the hook here as Claude Code's does there.
    */
  private def thisMachine(f: HookFixtures): ujson.Value = {
    val jvm = ProcessHandle.current().pid()
    ujson.Obj(
      "host" -> proc("sys/kernel/hostname"),
      "boot" -> proc("sys/kernel/random/boot_id"),
      "ns" -> Files.readSymbolicLink(Path.of("/proc/self/ns/pid")).toString,
      "pid" -> jvm.toString,
      "start" -> began(jvm),
      "dir" -> f.proj.toString
    )
  }

  val tests = Tests {
    test(
      "a session's start keeps a file naming its harness process as this boot knows it, and its recorded end removes it"
    ) {
      HookFixtures { f =>
        val started = f.run("opening", f.payload("session-start-startup.json"))
        val atStart = kept(f, First)
        val ended = f.run("event", f.payload("session-end-other.json"))
        ((started.exit, started.err), atStart, (ended.exit, ended.err), kept(f, First)) ==> (
          (0, ""),
          Some(thisMachine(f)),
          (0, ""),
          None
        )
      }
    }
    test("a session's end the edge did not keep leaves its file, for a later start to end it") {
      HookFixtures { f =>
        val _ = f.run("opening", f.payload("session-start-startup.json"))
        f.script.failing(Some("the database is down"))
        val _ = f.run("event", f.payload("session-end-other.json"))
        kept(f, First) ==> Some(thisMachine(f))
      }
    }
    test("an undo removes the session files with the hook") {
      HookFixtures { f =>
        val _ = f.run("opening", f.payload("session-start-startup.json"))
        val undone = f.machine.join(f.proj, Seq("--undo", "--mcp-name", f.machine.Name))
        (undone.exit, Files.exists(f.hook.getParent)) ==> (0, false)
      }
    }
  }
}
