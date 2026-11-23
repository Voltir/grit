package grit.kit.run

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Edges, Permit, Registration, ToolRequest}
import grit.core.host.{Allowlist, Argv, HostError}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.tool.{CallError, Outcome, Retry, ToolName}
import grit.host.checkout.Redaction
import grit.kit.deployment.Offered

import utest.*

/** [[LocalTools]] over an allowlisted offer, running real programs in a temporary checkout. */
object LocalToolsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Tools offering `offered`, given to `body`, closed after. */
  private def offering[A](offered: Offered)(body: LocalTools^ => A): A = {
    val tools = new LocalTools(offered, Redaction.of(Map.empty), Clock.system(), Fresh.random())
    try body(tools)
    finally tools.close()
  }

  /** `tool` called with `arguments` by tools offering `offered` over the checkout `root`. */
  private def run(
      offered: Offered,
      root: Path,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place] = Some(_)
  ): Outcome =
    offering(offered)(call(_, root, tool, arguments, destination))

  /** `tool` called with `arguments` by `tools` over the checkout `root`, its request's
    * destination what `destination` makes of the checkout's place.
    */
  private def call(
      tools: LocalTools^,
      root: Path,
      tool: String,
      arguments: ujson.Value,
      destination: Place => Option[Place] = _ => None
  ): Outcome = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    val workspace = Place.of(right(Directory.of(root.toString)))
    val q = ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      workspace,
      PrincipalId.Local,
      right(ToolName.of(tool)),
      Permit.Free,
      Retry.Interrupt,
      arguments,
      Set.empty,
      destination(workspace)
    )
    val route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Local, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    tools.run(route, q, _ => ())
  }

  val tests = Tests {
    test("a run's id reaches tail in a later request to the same checkout") {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("slow")
      Files.writeString(script, "#!/bin/sh\necho begun\nsleep 1\necho finished\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./slow")))
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
      offering(offered) { tools =>
        val started =
          call(tools, root, "run", ujson.Obj("program" -> "./slow", "wait" -> 0), Some(_))
        val id = started match {
          case Outcome.Done(text, _, _, _, _) =>
            "as `([0-9a-f]{8})`".r.findFirstMatchIn(text).map(_.group(1)).getOrElse("")
          case _ => ""
        }
        call(tools, root, "tail", ujson.Obj("id" -> id, "wait" -> 5)) ==>
          Outcome.Done("`./slow`\nExit code 0. Output:\nbegun\nfinished\n")
      }
    }

    test(
      "an allowlisted run starts a listed program named relative to the checkout's root there, and refuses an unlisted one, which leaves nothing"
    ) {
      val root = Files.createTempDirectory("allowlisted").toRealPath()
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\necho \"hello from $(pwd)\"\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
      val touch = right(Argv.of("touch", "x"))
      (
        run(offered, root, "run", ujson.Obj("program" -> "./hello")),
        run(offered, root, "run", ujson.Obj("program" -> "touch", "args" -> ujson.Arr("x"))),
        Files.exists(root.resolve("x"))
      ) ==> (
        Outcome.Done(s"Exit code 0. Output:\nhello from $root\n"),
        Outcome.Failed(HostError.Unlisted(touch).message),
        false
      )
    }

    test(
      "a change to the checkout runs only when its request's destination is the checkout: write, edit and run told none, or another place, are misdirected and change nothing"
    ) {
      val root = Files.createTempDirectory("changing").toRealPath()
      val script = root.resolve("hello")
      Files.writeString(script, "#!/bin/sh\necho hi > ran\n")
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      val listed = Allowlist.Rule.Leading(right(Argv.of("./hello")))
      val offered = Offered.Allowlisted(right(Allowlist.of(Vector(listed), 5.seconds)))
      val other = Place.of(right(Directory.of("/elsewhere")))
      def misdirected(tool: String, to: Option[Place]): Outcome =
        CallError.Misdirected(right(ToolName.of(tool)), to).outcome
      val made = ujson.Obj("path" -> "made.txt", "content" -> "made\n")
      val edited = ujson.Obj(
        "path" -> "made.txt",
        "edits" -> ujson.Arr(ujson.Obj("oldText" -> "made", "newText" -> "edited"))
      )
      val hello = ujson.Obj("program" -> "./hello")
      (
        run(offered, root, "write", made, _ => None),
        run(offered, root, "write", made, _ => Some(other)),
        Files.exists(root.resolve("made.txt")),
        run(offered, root, "write", made),
        run(offered, root, "edit", edited, _ => None),
        run(offered, root, "run", hello, _ => None),
        Files.readString(root.resolve("made.txt")),
        Files.exists(root.resolve("ran"))
      ) ==> (
        misdirected("write", None),
        misdirected("write", Some(other)),
        false,
        Outcome.Done("Wrote made.txt: 1 line."),
        misdirected("edit", None),
        misdirected("run", None),
        "made\n",
        false
      )
    }
  }
}
