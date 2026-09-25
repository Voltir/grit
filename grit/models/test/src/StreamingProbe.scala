package grit.models

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{Delta, ModelRequest}

/** Live probe: does a streamed reply come back as the same message a plain call returns,
  * and does its reasoning replay? One question streamed and asked plainly, then a
  * follow-up carrying the streamed reply. Three small calls, a fraction of a cent.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.StreamingProbe}}}
  *
  * Not a test: `./mill __.test` makes no model calls.
  */
object StreamingProbe {

  def main(args: Array[String]): Unit = {
    val _ = args
    OpenRouterConfig.fromEnv(sys.env, ModelRole.Turn) match {
      case Left(invalid) => println(invalid.message)
      case Right(config) => run(new OpenRouterProvider(config.copy(maxTokens = 300)))
    }
  }

  private def shape(m: Message.Assistant): String =
    m.blocks
      .map {
        case AssistantBlock.Text(_) => "text"
        case AssistantBlock.Reasoning(_, replay) => s"reasoning(replay=${replay.isDefined})"
        case AssistantBlock.ToolCall(_, name, _) => s"tool:$name"
      }
      .mkString(", ") + s"; stop ${m.stop}; ${m.model}"

  private def run(provider: OpenRouterProvider): Unit = {
    val asked =
      ModelRequest("You are terse.", Vector(Message.User("Name two runes of the Elder Futhark.")))
    var pieces = 0
    var first = -1L
    val start = System.nanoTime()
    val streamed = provider.stream(
      asked,
      {
        case Delta.Text(_) | Delta.Reasoning(_) =>
          if (first < 0) first = (System.nanoTime() - start) / 1000000
          pieces += 1
        case Delta.Calling(_) => ()
      }
    )
    val total = (System.nanoTime() - start) / 1000000
    val plain = provider.complete(asked)
    (streamed, plain) match {
      case (Right(s), Right(p)) =>
        println(s"[probe] streamed: $pieces pieces, first after ${first}ms of ${total}ms")
        println(s"[probe] streamed shape: ${shape(s)}; usage ${s.usage}")
        println(s"[probe] plain shape:    ${shape(p)}; usage ${p.usage}")
        val follow =
          ModelRequest(asked.system, asked.messages ++ Vector(s, Message.User("And a third?")))
        provider.complete(follow) match {
          case Right(r) => println(s"[probe] follow-up with the streamed reply: ok, ${shape(r)}")
          case Left(e) => println(s"[probe] follow-up with the streamed reply FAILED: $e")
        }
      case other => println(s"[probe] FAILED: $other")
    }
  }
}
