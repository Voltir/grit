package grit.claudecode

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.commands.machine.ClaimHolders
import grit.commands.routes.RouteFixtures.Machine
import grit.core.claim.ClaimRole
import grit.core.edge.{ClosedPort, ScriptedSessions}
import grit.core.either.Got.got
import grit.core.id.DocKey
import grit.core.place.Directory
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

  /** A file for the session `local` as the hook writes one, of a session in `dir` (by default
    * `f`'s checkout).
    */
  private def planted(
      f: HookFixtures,
      local: String,
      host: String = "desk",
      boot: String = "this-boot",
      ns: String = "pid:[1]",
      pid: Long,
      start: String = "500",
      dir: Option[Path] = None
  ): Unit = {
    val text = ujson.write(
      ujson.Obj(
        "host" -> host,
        "boot" -> boot,
        "ns" -> ns,
        "pid" -> pid.toString,
        "start" -> start,
        "dir" -> dir.getOrElse(f.proj).toString
      )
    )
    val _ = f.machine.write(sessions(f).resolve(local), text)
  }

  /** The session `local` recorded as started in `dir` (by default `f`'s checkout) a minute ago,
    * by this machine.
    */
  private def started(f: HookFixtures, local: String, dir: Option[Path] = None): Unit = {
    val line =
      s"""{"session":"$local","dir":"${dir.getOrElse(f.proj)}","n":"1","at":"${Instant
          .now()
          .minusSeconds(60)}","event":"started"}"""
    val _ = f.post("/events?harness=claude-code", line) ==> (
      "HTTP/1.1 200 OK",
      """{"recorded":1,"refused":[]}"""
    )
  }

  /** `text` kept as the brief of the holder of a session in `dir`, for the claim on `subject`,
    * worked by the session `local`.
    */
  private def claimed(f: HookFixtures, dir: Path, subject: String, text: String, local: String) =
    f.script.claimedBrief(
      ClaimHolders.of(Machine, got(Directory.of(dir.toString)), None),
      ScriptedSessions.document(text, Instant.now().minusSeconds(3600)),
      got(DocKey.of(subject)),
      None,
      got(ClaimRole.of("builder")),
      Some(f.session(local))
    )

  /** What the edge answers a session starting afresh in `dir` when it asks for its brief. */
  private def briefOf(f: HookFixtures, dir: Path): String =
    f.get(s"/brief?harness=claude-code&session=onlooker&dir=${dir}&start=fresh")._2

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
    test(
      "a session's end the edge did not take is sent by a later start in the checkout, and its file removed"
    ) {
      HookFixtures { f =>
        val _ = f.run("opening", f.payload("session-start-startup.json"))
        f.script.failing(Some("the database is down"))
        val _ = f.run("event", f.payload("session-end-other.json"))
        f.script.failing(None)
        val atEnd = (kept(f, First), f.queue(First).size)
        val before = f.recorded.size
        val _ = f.run("opening", f.payload("session-start-fork.json"))
        (
          atEnd,
          f.recorded.drop(before).map(b => (b.id, b.events.map(_.kind.name))),
          f.queue(First)
        ) ==> ((None, 1), Vector(f.session(First) -> Vector("ended")), Vector.empty)
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
    test("a start stops ending dead sessions at the first end the edge leaves unanswered") {
      HookFixtures { f =>
        val proc = faked(f)
        planted(f, "gone", pid = 4002)
        planted(f, "lost", pid = 4003)
        val asked = HookFixtures.relaying(f.port, HookFixtures.Relay.First) { (proxy, accepted) =>
          val _ = f.run(
            "opening",
            f.payload("session-start-startup.json"),
            withProc(f, proc) + ("GRIT_EDGE" -> s"http://127.0.0.1:$proxy")
          )
          accepted()
        }
        // The holder's, then the first end's.
        (asked, files(f), f.queue("gone").size, f.queue("lost")) ==> (
          2,
          Vector(".kept", First, "lost"),
          1,
          Vector.empty
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
    test(
      "a dead session's end the edge does not take is sent by the next start, and its file removed"
    ) {
      HookFixtures { f =>
        val proc = faked(f)
        started(f, "gone")
        planted(f, "gone", pid = 4002)
        f.script.failing(Some("the database is down"))
        val run = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        f.script.failing(None)
        val atFirst = (files(f).contains("gone"), f.queue("gone").size)
        val before = f.recorded.size
        val _ = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        (
          run.exit,
          atFirst,
          f.recorded.drop(before).map(b => (b.id, b.events.map(e => (e.kind.name, shape(e))))),
          f.queue("gone")
        ) ==> (
          0,
          (false, 1),
          Vector(
            f.session("gone") ->
              Vector(("ended", ujson.Obj("reason" -> "stopped", "found" -> "gone")))
          ),
          Vector.empty
        )
      }
    }
    test("a dead session's kept lines go to the edge ahead of its end, in one post") {
      HookFixtures { f =>
        val proc = faked(f)
        val down =
          withProc(f, proc) + ("GRIT_EDGE" -> s"http://127.0.0.1:${ClosedPort.port}")
        val _ = f.run("event", f.payload("post-tool-use-read.json").replace(First, "gone"), down)
        planted(f, "gone", pid = 4002)
        val before = f.recorded.size
        val _ = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
        (
          f.recorded.drop(before).map(b => (b.id, b.events.map(_.kind.name))),
          files(f).contains("gone"),
          f.queue("gone")
        ) ==> (Vector(f.session("gone") -> Vector("read", "ended")), false, Vector.empty)
      }
    }
    test(
      "a dead session's claim is free once a start has ended it, and taken by the starting session when it is its holder's"
    ) {
      HookFixtures { f =>
        val proc = faked(f)
        val other = Files.createDirectories(f.proj.resolve("other"))
        started(f, "here")
        started(f, "there", Some(other))
        planted(f, "here", pid = 4002)
        planted(f, "there", pid = 4003, dir = Some(other))
        claimed(f, f.proj, "p/1", "Work the first thing.", "here")
        claimed(f, other, "p/2", "Work the second thing.", "there")
        val before = (briefOf(f, f.proj), briefOf(f, other))
        val env = withProc(f, proc)
        val _ = f.run("opening", f.payload("session-start-startup.json"), env)
        val _ = f.run("event", f.payload("session-start-startup.json"), env)
        def worker(brief: String) =
          "session claude-code:([^ ]+) is working on it".r.findFirstMatchIn(brief).map(_.group(1))
        val freed = briefOf(f, other)
        (
          worker(before._1),
          worker(before._2),
          worker(briefOf(f, f.proj)),
          worker(freed),
          freed.contains("Work the second thing.")
        ) ==> (Some("here"), Some("there"), Some(First), None, true)
      }
    }
    test("a start's opening says how many sessions it ended, and nothing of none") {
      HookFixtures { f =>
        val proc = faked(f)
        def said() = {
          val run = f.run("opening", f.payload("session-start-startup.json"), withProc(f, proc))
          HookFixtures
            .printed(run)("hookSpecificOutput")("additionalContext")
            .str
            .linesIterator
            .filter(_.startsWith("grit: ended"))
            .toVector
        }
        val none = said()
        planted(f, "gone", pid = 4002)
        val one = said()
        planted(f, "gone", pid = 4002)
        planted(f, "rebooted", boot = "an-earlier-boot", pid = 4003)
        (none, one, said()) ==> (
          Vector.empty,
          Vector("grit: ended 1 session of this checkout that had stopped without saying so."),
          Vector("grit: ended 2 sessions of this checkout that had stopped without saying so.")
        )
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
