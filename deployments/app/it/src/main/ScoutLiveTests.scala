package grit.app.main

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

import grit.core.clock.Clock
import grit.core.host.Argv
import grit.core.id.{ToolCallId, TurnRef}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.host.checkout.{LocalProcesses, Redaction}
import grit.kit.deployment.Offered
import grit.mcp.client.McpServer
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope
import grit.turn.Turn

import utest.*

/** A chat's turn calling scout through the edge its own process serves ([[McpEdge.local]]),
  * against Postgres and DBOS, with a script in scout's place. The call is let through by ADR
  * 0031's two checks with nothing declared: a directory's room is labelled public and an
  * undeclared service is cleared for public, so the call passes `Tx.sendsTo` and its answer
  * `Tx.readsFrom`. Under the reference deployment's cap at member, the answer is shown whole,
  * not hidden as an untrusted service's would be.
  */
object ScoutLiveTests extends TestSuite {

  import LiveTurn.*

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** An MCP server over stdio listing a nameless tool and `find`, read-only, taking `pattern`
    * and `root`, and answering each call "found <pattern> in <root>".
    */
  private val Script =
    """#!/bin/sh
      |while IFS= read -r line; do
      |  id=$(printf '%s' "$line" | sed -n 's/.*"id":\("[^"]*"\|[0-9]*\).*/\1/p')
      |  case "$line" in
      |    *'"tools/list"'*)
      |      printf '{"jsonrpc":"2.0","id":%s,"result":{"tools":[{"inputSchema":{"type":"object"}},{"name":"find","description":"Finds a definition by name.","inputSchema":{"type":"object","properties":{"pattern":{"type":"string"},"root":{"type":"string"}},"required":["pattern"]},"annotations":{"readOnlyHint":true}}]}}\n' "$id";;
      |    *'"tools/call"'*)
      |      root=$(printf '%s' "$line" | sed -n 's/.*"root":"\([^"]*\)".*/\1/p')
      |      pattern=$(printf '%s' "$line" | sed -n 's/.*"pattern":"\([^"]*\)".*/\1/p')
      |      printf '{"jsonrpc":"2.0","id":%s,"result":{"content":[{"type":"text","text":"found %s in %s"}],"isError":false}}\n' "$id" "$pattern" "$root";;
      |  esac
      |done
      |""".stripMargin

  /** Calls `scout_find` once, then answers with its result and the tools its second call was
    * offered.
    */
  private final class Calling extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
      val results = request.messages.collect { case Message.ToolResult(_, content, _) => content }
      Right(
        if (results.isEmpty)
          Message.Assistant(
            Vector(
              AssistantBlock
                .ToolCall(ToolCallId("f"), "scout_find", ujson.Obj("pattern" -> "Turn"))
            ),
            StopReason.ToolUse,
            usage,
            "m"
          )
        else
          Message.Assistant(
            Vector(
              AssistantBlock.Text(
                (results :+ request.tools.map(_.name).sorted.mkString(",")).mkString("\n")
              )
            ),
            StopReason.EndTurn,
            usage,
            "m"
          )
      )
    }
  }

  /** A checkout holding the script, executable, handed to `body`, then deleted. */
  private def checkout[A](body: Path => A): A = {
    val root = Files.createTempDirectory("grit-scout").toRealPath()
    try {
      val script = root.resolve("scout")
      val _ = Files.writeString(script, Script)
      val _ = Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
      body(root)
    } finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

  private def own(engine: Engine^, turn: TurnRef): Vector[Entry] =
    engine.db
      .read(Subject.Public)(engine.entries.list(turn.conversationId))
      .getOrElse(Vector.empty)
      .filter(_.turnSeq == turn.turnSeq)

  val tests: Tests = Tests {
    test(
      "a chat's turn calls scout through its process's edge, answered about its own directory, and its next round still offers edit"
    ) {
      checkout { root =>
        // The reference deployment with scout declared: its reach and its cap at member.
        val deployment = right(
          Deploying.deployment(
            Map(ChatEdges.ScoutVar -> root.resolve("scout").toString),
            Offered.Allowlisted(Offered.ReferenceAllowlist),
            Vector.empty,
            Vector.empty,
            java.time.ZoneOffset.UTC
          )
        )
        val engine = LiveEngine.open(
          TestPostgres.freshDatabase("scout_local"),
          Turn.Epoch,
          visibility = deployment.visibility
        )
        try {
          val scout = right(ChatEdges.ScoutAt)
          launchIn(
            engine,
            engine.entries,
            new Calling,
            root,
            offered = Offered.Allowlisted(Offered.ReferenceAllowlist),
            reaches = deployment.reaches
          )
          val server = right(
            Argv
              .of(root.resolve("scout").toString, "mcp")
              .flatMap(McpServer.stdio("scout", _, McpScope.Open, directoryArgument = Some("root")))
          )
          val local = McpEdge.local(
            scout,
            server,
            new LocalProcesses(root, sys.env, Redaction.of(Map.empty)),
            Clock.system(),
            Some(ChatEdges.ScoutInstructions),
            _ => ()
          ) match {
            case Right(local) => local
            case Left(why) => throw new java.lang.AssertionError(why)
          }
          try {
            local.serve(engine)
            val turn = say(engine, "find it")
            val _ = engine.awaitTurn(turn)
            own(engine, turn).collect {
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
              case Entry(
                    _,
                    _,
                    _,
                    _,
                    _,
                    Payload.Message(Message.Assistant(blocks, StopReason.EndTurn, _, _, _)),
                    _,
                    _
                  ) =>
                (blocks.collect { case AssistantBlock.Text(t) => t }.mkString, false)
            } ==> Vector(
              (s"found Turn in $root", false),
              (
                // Shown whole, believed at member: not hidden behind read_hidden.
                "<tool_output source=\"scout_find {&quot;pattern&quot;:&quot;Turn&quot;}\">\n" +
                  s"found Turn in $root\n</tool_output>\n" +
                  "edit,extract,list,read,read_hidden,run,scout_find,search,stop,tail,topic,write",
                false
              )
            )
          } finally local.close()
        } finally engine.close()
      }
    }
  }
}
