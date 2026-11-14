package grit.claudecode

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant

import grit.commands.routes.RouteFixtures.got
import grit.commands.worker.Holders
import grit.core.edge.ScriptedSessions

import utest.*

/** The hook's start roles, `brief` and `opening`, on Claude Code's recorded start payloads, and
  * `holder`, against the commands edge: what they ask it and what they print.
  */
object BriefHookTests extends TestSuite {

  private val First = "11111111-1111-4111-8111-111111111111"
  private val Cleared = "33333333-3333-4333-8333-333333333333"
  private val Forked = "22222222-2222-4222-8222-222222222222"
  private val Agent = "a1111111111111111"

  private def encoded(s: String): String = URLEncoder.encode(s, UTF_8)

  /** Claude Code's hook JSON adding `text` to the window at `event`. */
  private def context(event: String, text: String): ujson.Value =
    ujson.Obj(
      "hookSpecificOutput" -> ujson.Obj("hookEventName" -> event, "additionalContext" -> text)
    )

  /** What `done` printed, read as JSON; `null` when it printed nothing. */
  private def printed(done: Bash.Done): ujson.Value =
    if (done.out.isEmpty) ujson.Null else ujson.read(done.out.mkString("\n"))

  /** The lines `opening` gives a session in the fixtures' checkout working as `holder`. */
  private def holderLines(f: HookFixtures, session: String, holder: String): String =
    s"grit: this session's claim holder is $holder.\n" +
      "grit: an agent this session starts has its own claim holder. This prints it, with the " +
      s"agent's type in place of TYPE: '${f.hook}' holder TYPE --dir '${f.proj}' --session '$session'"

  /** The session `First` recorded as having closed saying `Wrote the notes.` a minute ago. */
  private def worked(f: HookFixtures): Unit = {
    val at = Instant.now().minusSeconds(60)
    val line =
      s"""{"session":"$First","dir":"${f.proj}","n":"1","at":"$at","event":"closing","text":"Wrote the notes."}"""
    val _ = f.post("/events?harness=claude-code", line) ==> (
      "HTTP/1.1 200 OK",
      """{"recorded":1,"refused":[]}"""
    )
  }

  val tests = Tests {
    test(
      "a session's brief is asked fresh at startup and clear, resumed at resume, compact and fork, and printed as Claude Code's hook JSON"
    ) {
      HookFixtures { f =>
        f.script.brief(
          f.holderOf(),
          ScriptedSessions.document("Read the plan first.", Instant.now().minusSeconds(3600))
        )
        val sources = Vector(
          "startup" -> First,
          "clear" -> Cleared,
          "resume" -> First,
          "compact" -> First,
          "fork" -> Forked
        )
        val runs =
          sources.map((source, _) => f.run("brief", f.payload(s"session-start-$source.json")))
        val asked = f.briefed.map(b => (b.holder, b.besides))
        val brief = f.get(
          s"/brief?harness=claude-code&session=$First&dir=${encoded(f.proj.toString)}&start=resume"
        )
        (runs.map(r => (r.exit, printed(r), r.err)), asked) ==> (
          sources.map(_ => (0, context("SessionStart", brief._2), "")),
          Vector(
            (f.holderOf(), Some(f.session(First))),
            (f.holderOf(), Some(f.session(Cleared))),
            (f.holderOf(), None),
            (f.holderOf(), None),
            (f.holderOf(), None)
          )
        )
        assert(brief._2.contains("Read the plan first."))
      }
    }
    test(
      "an agent's brief is asked fresh, as the holder of its type under the starting session's checkout"
    ) {
      HookFixtures { f =>
        val holder = f.holderOf(Some("general-purpose" -> First))
        f.script.brief(
          holder,
          ScriptedSessions.document("Agents read this.", Instant.now().minusSeconds(3600))
        )
        val run = f.run("brief", f.payload("subagent-start-worktree.json"))
        val agentSession = got(Holders.agentSession(f.session(First), Agent))
        (
          run.exit,
          printed(run)("hookSpecificOutput")("hookEventName").str,
          f.briefed.map(b => (b.holder, b.besides))
        ) ==> (
          0,
          "SubagentStart",
          Vector((holder, Some(agentSession)))
        )
        assert(
          printed(run)("hookSpecificOutput")("additionalContext").str.contains("Agents read this.")
        )
      }
    }
    test("an empty brief prints nothing, and an agent of no type asks for none") {
      HookFixtures { f =>
        val empty = f.run("brief", f.payload("session-start-startup.json"))
        val untyped = f.run(
          "brief",
          f.payload("subagent-stop-compaction.json").replace("SubagentStop", "SubagentStart")
        )
        (empty.exit, empty.out, empty.err, untyped.exit, untyped.out, f.briefed.size) ==> (
          0,
          Vector.empty,
          "",
          0,
          Vector.empty,
          1
        )
      }
    }
    test(
      "opening prints the holder lines at every start, and the digest only after a compaction, resumed, and after a clear, fresh"
    ) {
      HookFixtures { f =>
        worked(f)
        val sources = Vector(
          "startup" -> First,
          "resume" -> First,
          "fork" -> Forked,
          "compact" -> First,
          "clear" -> Cleared
        )
        val runs =
          sources.map((source, _) => f.run("opening", f.payload(s"session-start-$source.json")))
        val asked = f.briefed.map(_.besides)
        val dir = encoded(f.proj.toString)
        val resumed =
          f.get(s"/brief?harness=claude-code&session=$First&dir=$dir&start=resume&part=digest")._2
        val fresh =
          f.get(s"/brief?harness=claude-code&session=$Cleared&dir=$dir&start=fresh&part=digest")._2
        val holder = f.holderOf().toString
        (runs.map(r => (r.exit, printed(r), r.err)), asked) ==> (
          Vector(
            (0, context("SessionStart", holderLines(f, First, holder)), ""),
            (0, context("SessionStart", holderLines(f, First, holder)), ""),
            (0, context("SessionStart", holderLines(f, Forked, holder)), ""),
            (0, context("SessionStart", holderLines(f, First, holder) + "\n\n" + resumed), ""),
            (0, context("SessionStart", holderLines(f, Cleared, holder) + "\n\n" + fresh), "")
          ),
          Vector(None, Some(f.session(Cleared)))
        )
        // A compacted window holds what its session last said; a cleared one does not.
        assert(
          resumed.nonEmpty,
          !resumed.contains("Wrote the notes."),
          fresh.contains("Wrote the notes.")
        )
      }
    }
    test(
      "holder prints the session's holder, and with a type, --dir and --session the agent's; a type with no session is exit 1"
    ) {
      HookFixtures { f =>
        val bash = Map("CLAUDE_CODE_SESSION_ID" -> First)
        val own = f.run("holder", env = bash)
        val agent = f.run(
          "holder",
          env = Map.empty,
          in = f.machine.root,
          args = Seq("general-purpose", "--dir", f.proj.toString, "--session", First)
        )
        val lost = f.run("holder", env = Map.empty, args = Seq("general-purpose"))
        (
          own.exit,
          own.out,
          agent.exit,
          agent.out,
          lost.exit,
          lost.out,
          lost.err.linesIterator.size
        ) ==> (
          0,
          Vector(f.holderOf().toString),
          0,
          Vector(f.holderOf(Some("general-purpose" -> First)).toString),
          1,
          Vector.empty,
          1
        )
      }
    }
  }
}
