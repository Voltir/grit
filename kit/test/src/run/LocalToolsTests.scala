package grit.kit.run

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt

import grit.core.edge.{Edges, Permit, Registration, ToolRequest}
import grit.core.host.{Allowlist, Argv, HostError}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.host.checkout.Redaction
import grit.kit.deployment.Offered

import utest.*

/** [[LocalTools]] over an allowlisted offer, running real programs in a temporary checkout. */
object LocalToolsTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** `tool` called with `arguments` by tools offering `offered` over the checkout `root`. */
  private def run(offered: Offered, root: Path, tool: String, arguments: ujson.Value): Outcome = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    val q = ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      Place.of(right(Directory.of(root.toString))),
      PrincipalId.Local,
      right(ToolName.of(tool)),
      Permit.Free,
      Retry.Interrupt,
      arguments,
      Set.empty,
      None
    )
    val route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Local, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    new LocalTools(offered, Redaction.of(Map.empty)).run(route, q, _ => ())
  }

  val tests = Tests {
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
  }
}
