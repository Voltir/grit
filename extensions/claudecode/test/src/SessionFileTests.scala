package grit.claudecode

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.session.Event

import utest.*

/** The hook's session files, against the commands edge: each session's start keeps one beside the
  * hook naming the harness process that runs it, removed when its end is recorded, and ends the
  * sessions whose files say that process died.
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

  /** The directory of the hook's session files. */
  private def sessions(f: HookFixtures): Path = f.hook.resolveSibling("sessions")

  /** The names of the session files the hook keeps, in order. */
  private def files(f: HookFixtures): Vector[String] =
    if (!Files.isDirectory(sessions(f))) Vector.empty
    else {
      val listed = Files.list(sessions(f))
      try listed.iterator().asScala.map(_.getFileName.toString).toVector.sorted
      finally listed.close()
    }

  /** A directory standing in for /proc: the machine `desk`, booted as `this-boot`, its pid
    * namespace `pid:[1]`, running this JVM, started at tick 100, as the hook's harness.
    */
  private def faked(f: HookFixtures): Path = {
    val proc = f.machine.root.resolve("proc")
    val _ = f.machine.write(proc.resolve("sys/kernel/hostname"), "desk\n")
    val _ = f.machine.write(proc.resolve("sys/kernel/random/boot_id"), "this-boot\n")
    val ns = Files.createDirectories(proc.resolve("self/ns")).resolve("pid")
    val _ = Files.createSymbolicLink(ns, Path.of("pid:[1]"))
    process(proc, ProcessHandle.current().pid(), "java", "100")
    proc
  }

  /** The process `pid` in `proc`, named `comm` (its name holding a `) ` as a name may), started
    * at tick `start`.
    */
  private def process(proc: Path, pid: Long, comm: String, start: String): Unit = {
    val fields = Vector("S", "1") ++ Vector.fill(17)("0") ++ Vector(start, "0", "0")
    val _ = Files.writeString(
      Files.createDirectories(proc.resolve(pid.toString)).resolve("stat"),
      s"$pid ($comm) x) ${fields.mkString(" ")}\n"
    )
  }

  /** The hook's environment in `f`, reading `proc` for /proc. */
  private def withProc(f: HookFixtures, proc: Path): Map[String, String] =
    f.claude + ("GRIT_HOOK_PROC" -> proc.toString)

  /** A file for the session `local` as the hook writes one, in `f`'s checkout. */
  private def planted(
      f: HookFixtures,
      local: String,
      host: String = "desk",
      boot: String = "this-boot",
      ns: String = "pid:[1]",
      pid: Long,
      start: String = "500"
  ): Unit = {
    val text = ujson.write(
      ujson.Obj(
        "host" -> host,
        "boot" -> boot,
        "ns" -> ns,
        "pid" -> pid.toString,
        "start" -> start,
        "dir" -> f.proj.toString
      )
    )
    val _ = f.machine.write(sessions(f).resolve(local), text)
  }

  /** The session `local` recorded as started in `f`'s checkout a minute ago, by this machine. */
  private def started(f: HookFixtures, local: String): Unit = {
    val line =
      s"""{"session":"$local","dir":"${f.proj}","n":"1","at":"${Instant
          .now()
          .minusSeconds(60)}","event":"started"}"""
    val _ = f.post("/events?harness=claude-code", line) ==> (
      "HTTP/1.1 200 OK",
      """{"recorded":1,"refused":[]}"""
    )
  }

  /** An event's shape, as JSON. */
  private def shape(e: Event): ujson.Value = ujson.read(Event.Shape.value(e.shape))

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
    test(
      "a start ends each session of this machine whose harness is gone, rebooted or replaced, in a post of its own, and leaves the rest"
    ) {
      HookFixtures { f =>
        val proc = faked(f)
        val dead = Vector("rebooted", "gone", "reused")
        (dead ++ Vector("live", "elsewhere", "contained")).foreach(started(f, _))
        planted(f, "rebooted", boot = "an-earlier-boot", ns = "pid:[7]", pid = 4001)
        planted(f, "gone", pid = 4002)
        planted(f, "reused", pid = 4003, start = "800")
        planted(f, "live", pid = 4004, start = "700")
        planted(f, "elsewhere", host = "another-desk", pid = 4005)
        planted(f, "contained", ns = "pid:[7]", pid = 4006)
        process(proc, 4003, "claude", "900")
        process(proc, 4004, "claude", "700")
        val before = f.recorded.size
        val run = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        val ends =
          f.recorded.drop(before).map(b => (b.id, b.events.map(e => (e.kind.name, shape(e)))))
        (run.exit, run.err, ends, files(f)) ==> (
          0,
          "",
          Vector("gone", "rebooted", "reused").map(s =>
            f.session(s) -> Vector(("ended", ujson.Obj("reason" -> "stopped", "found" -> s)))
          ),
          Vector(First, "contained", "elsewhere", "live")
        )
      }
    }
    test("a session file it cannot read is removed, saying so, and decides nothing") {
      HookFixtures { f =>
        val proc = faked(f)
        f.machine.write(sessions(f).resolve("garbled"), "{\"host\": \"desk\", \"pid\": [1]}")
        f.machine.write(sessions(f).resolve("torn"), "{\"host\": \"de")
        val before = f.recorded.size
        val run = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        (run.exit, run.err.linesIterator.toVector, f.recorded.size - before, files(f)) ==> (
          0,
          Vector("garbled", "torn").map(n =>
            s"grit claude-code hook: removed the session file ${sessions(f).resolve(n)}, which it could not read"
          ),
          0,
          Vector(First)
        )
      }
    }
    test("a dead session's end the edge does not keep leaves its file for the next start") {
      HookFixtures { f =>
        val proc = faked(f)
        started(f, "gone")
        planted(f, "gone", pid = 4002)
        f.script.failing(Some("the database is down"))
        val run = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        f.script.failing(None)
        (run.exit, files(f).contains("gone")) ==> (0, true)
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
