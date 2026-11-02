package grit.app.main

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.{Allowlist, Argv, HostError}
import grit.core.id.{ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.kit.deployment.Offered
import grit.turn.Turn

import utest.*

/** A turn offered an allowlisted set, against Postgres and DBOS with the real coding tools over a
  * temporary checkout: its model writes a file, runs a listed program and an unlisted one, and
  * nothing asks.
  */
object AllowlistLiveTests extends TestSuite {
  import LiveTurn.*

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val touch = right(Argv.of("touch", "x"))

  /** The checkout's allowlist: its own `./hello` script, with any arguments. */
  private val allowlist =
    right(Allowlist.of(Vector(Allowlist.Rule.Leading(right(Argv.of("./hello")))), 30.seconds))

  /** Calls `write`, then `run` of `./hello`, then `run` of `touch x`, one per model call, then
    * answers with each result in order.
    */
  private final class Calling extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
      def calling(id: String, tool: String, args: ujson.Obj): Message.Assistant =
        Message.Assistant(
          Vector(AssistantBlock.ToolCall(ToolCallId(id), tool, args)),
          StopReason.ToolUse,
          usage,
          "m"
        )
      val results = request.messages.collect { case Message.ToolResult(_, content, _) => content }
      Right(results.size match {
        case 0 => calling("w", "write", ujson.Obj("path" -> "made.txt", "content" -> "made\n"))
        case 1 => calling("r", "run", ujson.Obj("program" -> "./hello"))
        case 2 => calling("t", "run", ujson.Obj("program" -> "touch", "args" -> ujson.Arr("x")))
        case _ =>
          Message.Assistant(
            Vector(AssistantBlock.Text(results.mkString("\n---\n"))),
            StopReason.EndTurn,
            usage,
            "m"
          )
      })
    }
  }

  /** A checkout holding an executable `./hello` that says where it ran, handed to `body`, then
    * deleted.
    */
  private def checkout[A](body: Path => A): A = {
    val root = Files.createTempDirectory("grit-allowlist").toRealPath()
    try {
      val hello = root.resolve("hello")
      val _ = Files.writeString(hello, "#!/bin/sh\necho \"hello from $(pwd)\"\n")
      val _ = Files.setPosixFilePermissions(hello, PosixFilePermissions.fromString("rwx------"))
      body(root)
    } finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

  private def own(engine: Engine^, turn: TurnRef): Vector[Entry] =
    engine.db
      .read(Subject.Public)(engine.entries.list(turn.conversationId))
      .getOrElse(Vector.empty)
      .filter(_.turnSeq == turn.turnSeq)

  val tests = Tests {
    test(
      "an allowlisted turn writes and runs a listed program unasked, and its unlisted run fails, running nothing"
    ) {
      checkout { root =>
        val engine = LiveEngine.open(TestPostgres.freshDatabase("allowlist_run"), Turn.Epoch)
        try {
          launchIn(
            engine,
            engine.entries,
            new Calling,
            root,
            offered = Offered.Allowlisted(allowlist)
          )
          val turn = say(engine, "make it")
          val _ = engine.awaitTurn(turn)
          val entries = own(engine, turn)
          (
            entries.collect { case Entry(_, _, _, _, _, a: Payload.Ask, _, _) => a },
            entries.collect {
              case Entry(
                    _,
                    _,
                    _,
                    _,
                    _,
                    Payload.Result(Message.ToolResult(_, content, failed), _),
                    _,
                    _
                  ) =>
                (content, failed)
            },
            Some(root.resolve("made.txt")).filter(Files.exists(_)).map(Files.readString),
            Files.exists(root.resolve("x"))
          ) ==> (
            Vector.empty,
            Vector(
              ("Wrote made.txt: 1 line.", false),
              (s"Exit code 0. Output:\nhello from $root\n", false),
              (HostError.Unlisted(touch).message, true)
            ),
            Some("made\n"),
            false
          )
        } finally engine.close()
      }
    }
  }
}
