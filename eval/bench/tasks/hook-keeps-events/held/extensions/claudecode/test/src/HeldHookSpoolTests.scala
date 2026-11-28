package grit.claudecode

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.session.Session

import utest.*

/** The hook when the edge did not take an event: the event is kept and reaches the edge with the
  * session's next post, ahead of that post's own, unless it has grown older than
  * [[Session.Unended]]. What the edge recorded is read, never what the hook posted.
  */
object HeldHookSpoolTests extends TestSuite {

  import HookFixtures.First

  /** The environment Claude Code gives a hook in `f`'s checkout, aimed at a port nothing listens
    * on.
    */
  private def down(f: HookFixtures): Map[String, String] =
    f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:${HookFixtures.closedPort}")

  /** A copy of `f`'s hook beside it, `hook-<name>`, whose clock reads, at every reading, the moment
    * `ago` before now: bash's `EPOCHREALTIME` set as a plain variable after the shebang.
    */
  private def before(f: HookFixtures, name: String, ago: FiniteDuration): Path = {
    val text = Files.readString(f.hook)
    val cut = text.indexOf('\n') + 1
    val us = (System.currentTimeMillis() - ago.toMillis) * 1000L
    val clock = f"unset EPOCHREALTIME; EPOCHREALTIME=${us / 1000000}%d.${us % 1000000}%06d\n"
    val copy = Files.writeString(
      f.hook.resolveSibling(s"hook-$name"),
      text.take(cut) + clock + text.drop(cut)
    )
    Files.setPosixFilePermissions(copy, PosixFilePermissions.fromString("rwx------"))
    copy
  }

  val tests = Tests {
    test(
      "an event the edge did not answer reaches it with the session's next post, ahead of that post's own"
    ) {
      HookFixtures { f =>
        val _ = f.run("event", f.payload("post-tool-use-read.json"), down(f))
        val _ = f.run("event", f.payload("post-tool-use-edit.json"))
        val events = f.recorded.flatMap(_.events)
        val ns = events.map(_.n)
        (
          events.map(_.kind.name),
          ns.sorted.distinct == ns,
          f.script.session(f.session(First)).map(_.counters.toolCalls)
        ) ==> (Vector("read", "edited"), true, Some(2L))
      }
    }
    test(
      "a kept event older than a session's unended span is not sent with the next post, and a younger one is"
    ) {
      HookFixtures { f =>
        val old = before(f, "old", Session.Unended + 1.hour)
        val young = before(f, "young", Session.Unended - 1.hour)
        val _ = f.run("event", f.payload("post-tool-use-read.json"), down(f), program = old)
        val _ = f.run("event", f.payload("post-tool-use-bash.json"), down(f), program = young)
        val _ = f.run("event", f.payload("post-tool-use-edit.json"))
        f.recorded.flatMap(_.events).map(_.kind.name) ==> Vector("ran", "edited")
      }
    }
  }
}
