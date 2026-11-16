package grit.claudecode

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions
import java.time.Instant

import grit.commands.routes.RouteFixtures.{AgentText, Credentialed}
import grit.core.clock.SetClock
import grit.core.edge.ScriptedSessions
import grit.core.id.ClaimHolder

import utest.*

/** A credential joined and sent: the join's `--credential`, what it checks and writes, and the
  * hook bearing it on every request to an edge that takes credentials alone.
  */
object CredentialJoinTests extends TestSuite {

  /** A credential's form, but none the edge knows. */
  private val Unknown = "unknown-0123456789abcdefghijklmnopqrstuvwxyz"

  private def mode(text: String) = PosixFilePermissions.fromString(text)

  val tests = Tests {
    test(
      "joined with a credential, the hook bears it on every request: the brief asked, the holder told, the event recorded"
    ) {
      HookFixtures.credentialed(AgentText) { f =>
        val start = f.payload("session-start-startup.json")
        val brief = f.run("brief", start)
        val holder = f.run("holder")
        val event = f.run("event", start)
        (
          (brief.exit, brief.err),
          (holder.exit, holder.out, holder.err),
          (event.exit, event.err),
          (f.briefed.size, f.recorded.size)
        ) ==> (
          (0, ""),
          (0, Vector(ClaimHolder.value(f.holderOf())), ""),
          (0, ""),
          (1, 1)
        )
      }
    }

    test("the header role prints the credential's header as Claude Code's headersHelper reads it") {
      HookFixtures.credentialed(AgentText) { f =>
        val done = f.run("header")
        (done.exit, done.out, done.err) ==>
          (0, Vector(s"""{"Authorization":"Bearer $AgentText"}"""), "")
      }
    }

    test(
      "a credential the edge refuses is said as 401, naming its file; one unreadable stops the hook, saying so"
    ) {
      HookFixtures.credentialed(AgentText) { f =>
        val file = f.machine.home.resolve(".config/grit/credential")
        val start = f.payload("session-start-startup.json")
        val _ = Files.writeString(file, Unknown)
        val refused = f.run("brief", start)
        val _ = Files.setPosixFilePermissions(file, mode("---------"))
        val unreadable = f.run("brief", start)
        (refused.err.trim, unreadable.err.trim) ==> (
          s"grit claude-code hook: the edge at ${f.edge} refused this machine's credential (401) for the brief: refused: the credential sent is not one this edge knows; ask whoever runs the edge for this credential and put it in $file",
          s"grit claude-code hook: cannot read the credential in $file; it did nothing"
        )
      }
    }

    test("with a credential, GRIT_EDGE naming another edge is refused, sending nothing anywhere") {
      HookFixtures.credentialed(AgentText) { f =>
        HookFixtures.holding { (port, accepted) =>
          val other = s"http://127.0.0.1:$port"
          val done = f.run(
            "brief",
            f.payload("session-start-startup.json"),
            f.claude + ("GRIT_EDGE" -> other)
          )
          (done.err.trim, accepted(), f.briefed.size) ==> (
            s"grit claude-code hook: GRIT_EDGE names $other, but this checkout's credential is sent only to the edge it was joined to, ${f.edge}; it did nothing",
            0,
            0
          )
        }
      }
    }

    test(
      "the join writes the credential's file into the hook, and the MCP entry asks the hook for its header"
    ) {
      HookFixtures.credentialed(AgentText) { f =>
        val file = f.machine.home.resolve(".config/grit/credential")
        (
          f.machine.read(f.hook).map(_.linesIterator.take(4).toVector),
          f.machine.claudeCalls.lastOption
        ) ==> (
          Some(
            Vector(
              "#!/usr/bin/env bash",
              "harness='claude-code'",
              s"edge='${f.edge}'",
              s"credential='$file'"
            )
          ),
          Some(
            s"""mcp add-json --scope local ${f.machine.Name} {"type":"http","url":"${f.edge}/mcp","headersHelper":"'${f.hook}' header"}"""
          )
        )
      }
    }

    test(
      "a join made with a credential prints an undo that, run as printed, fetches the script with the credential's file and undoes the join"
    ) {
      JoinFixtures { machine =>
        HookFixtures.served(
          new ScriptedSessions.Script,
          new SetClock(Instant.now()),
          Credentialed
        ) { port =>
          val proj = machine.checkout("proj")
          val file = HookFixtures.credentialOf(machine, AgentText)
          val joined = machine.join(
            proj,
            Seq(
              s"http://127.0.0.1:$port",
              "--credential",
              file.toString,
              "--mcp-name",
              machine.Name
            )
          )
          val undo = joined.out
            .collectFirst { case l if l.startsWith("Undo: ") => l.stripPrefix("Undo: ") }
            .getOrElse(throw new java.lang.AssertionError(s"no undo printed: ${joined.out}"))
          val ran = Bash.run(proj, machine.home, machine.bin, Seq("-c", undo))
          (
            ran.exit,
            ran.err,
            Files.exists(machine.hook(proj)),
            machine.read(machine.settings(proj)),
            machine.claudeCalls.lastOption,
            undo.contains(s"'$file'"),
            undo.contains(AgentText)
          ) ==> (
            (
              0,
              "",
              false,
              Some("{}\n"),
              Some(s"mcp remove --scope local ${machine.Name}"),
              true,
              false
            )
          )
        }
      }
    }

    test(
      "the join refuses a credential others may read, one that is no credential, and one the edge refuses, changing nothing"
    ) {
      JoinFixtures { machine =>
        HookFixtures.served(
          new ScriptedSessions.Script,
          new SetClock(Instant.now()),
          Credentialed
        ) { port =>
          val proj = machine.checkout("proj")
          val edge = s"http://127.0.0.1:$port"
          val file = HookFixtures.credentialOf(machine, AgentText)
          def joined() =
            machine.join(proj, Seq(edge, "--credential", file.toString, "--mcp-name", machine.Name))
          val before = machine.tree(proj)
          val _ = Files.setPosixFilePermissions(file, mode("rw-r--r--"))
          val open = joined()
          val _ = Files.setPosixFilePermissions(file, mode("rw-------"))
          val _ = Files.writeString(file, "short-0123456789")
          val short = joined()
          val _ = Files.writeString(file, Unknown)
          val refused = joined()
          (
            Vector(open, short, refused).map(d => (d.exit, d.err.trim)),
            machine.tree(proj) == before,
            Files.exists(machine.hook(proj))
          ) ==> (
            Vector(
              (
                1,
                s"grit claude-code: --credential $file is readable by others; make it 0600 first"
              ),
              (
                1,
                s"grit claude-code: --credential $file holds no credential: one line, at least 32 characters of letters, digits and -._~+/"
              ),
              (
                1,
                s"grit claude-code: the edge at $edge refused the credential in $file (401); nothing was changed"
              )
            ),
            true,
            false
          )
        }
      }
    }
  }
}
