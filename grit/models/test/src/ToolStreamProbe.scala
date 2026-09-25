package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{Delta, ModelRequest}
import grit.core.tool.{Args, Field, ToolName, ToolSpec}

/** Live probe: does a pinned role reach its upstream, with its tool schema strict, and does
  * a streamed call tell `Delta.Calling` before its arguments are done? One streamed call per
  * pair below, each offered one small tool; a fraction of a cent in all. The request and
  * the fold are the provider's own; only the HTTP is repeated here, to read which upstream
  * served (`provider` in each chunk), which a reply does not keep.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.ToolStreamProbe}}}
  *
  * Not a test: `./mill __.test` makes no model calls.
  */
object ToolStreamProbe {

  private val Pairs: Vector[(String, String)] = Vector(
    ("deepseek/deepseek-v4-flash", "open-inference/fp8"),
    ("openai/gpt-oss-20b", "coreweave")
  )

  private val Read: ToolSpec[(path: String)] = ToolSpec(
    ToolName("read"),
    "Reads a file of the checkout. Fails when the path does not exist.",
    Args.of((path = Field.text("The file's path, relative to the checkout's root.")))
  )

  def main(args: Array[String]): Unit = {
    val _ = args
    Pairs.foreach { (model, upstream) =>
      val env = sys.env ++ Map(
        "GRIT_MODEL" -> model,
        "GRIT_PROVIDER" -> upstream,
        "GRIT_STRICT_TOOLS" -> "true",
        "GRIT_MAX_TOKENS" -> "400"
      )
      OpenRouterConfig.fromEnv(env, ModelRole.Turn) match {
        case Left(invalid) => println(invalid.message)
        case Right(config) => println(s"[probe] $model @ $upstream: ${run(config)}")
      }
    }
  }

  private def run(config: OpenRouterConfig): String = {
    val request = ModelRequest(
      "You are a coding agent. Use the tools offered.",
      Vector(Message.User("What does build.mill say? Read it.")),
      Vector(Read.schema(strict = config.routing.strictTools))
    )
    val body = OpenRouterJson.request(config.model, config.maxTokens, config.routing, request)
    body("stream") = true
    val sent = body("tools")(0)("function")
    val told = Vector.newBuilder[String]
    val served = scala.collection.mutable.LinkedHashSet.empty[String]
    val chunks = new java.util.concurrent.atomic.AtomicInteger
    try {
      val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()
      val response = http.send(
        HttpRequest
          .newBuilder(config.endpoint)
          .timeout(config.timeout)
          .header("Authorization", s"Bearer ${config.apiKey}")
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
          .build(),
        HttpResponse.BodyHandlers.ofLines()
      )
      val lines = response.body
      try {
        if (response.statusCode != 200) s"HTTP ${response.statusCode}"
        else {
          val tee = lines.iterator().asScala.map { line =>
            if (line.startsWith("data:")) { val _ = chunks.incrementAndGet() }
            if (line.startsWith("data:"))
              scala.util
                .Try(ujson.read(line.drop(5).trim))
                .toOption
                .flatMap(_.objOpt)
                .flatMap(_.get("provider"))
                .flatMap(_.strOpt)
                .foreach(p => served += p)
            line
          }
          // Where in the stream each delta came: a call told before its arguments ended
          // is followed by argument chunks, which tell nothing, so it shows as calls first.
          OpenRouterStream
            .fold(
              tee,
              {
                case Delta.Calling(name) => told += s"calling $name at chunk ${chunks.get}"
                case Delta.Text(_) => told += "text"
                case Delta.Reasoning(_) => told += "reasoning"
              }
            )
            .flatMap(OpenRouterJson.response) match {
            case Left(e) => s"FAILED $e"
            case Right(reply) =>
              val calls = reply.blocks.collect { case AssistantBlock.ToolCall(_, n, a) =>
                s"$n ${ujson.write(a)} -> ${Read.args.read(a).fold(_.message, r => s"read ${r.path}")}"
              }
              val deltas = told.result().foldLeft(Vector.empty[String]) { (acc, d) =>
                if (acc.lastOption.contains(d)) acc else acc :+ d
              }
              s"served by ${served.mkString(",")}; strict sent: ${sent.obj.get("strict")}; " +
                s"deltas ${deltas.mkString(" > ")}; calls ${calls.mkString("; ")}; " +
                s"of ${chunks.get} chunks; stop ${reply.stop}; cost ${reply.usage.costUsd.getOrElse("?")}"
          }
        }
      } finally lines.close()
    } catch {
      case NonFatal(e) => s"FAILED ${e.getClass.getSimpleName}"
    }
  }
}
