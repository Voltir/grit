package grit.models

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.util.control.NonFatal

import grit.core.message.{AssistantBlock, Cost, Message, Tokens}
import grit.core.provider.ModelRequest

/** Live probe: does the model bill an earlier reply's reasoning as input? One real first
  * call, then the same follow-up sent with and without that reply's reasoning, twice each,
  * all pinned to the upstream host that served the first call: OpenRouter spreads a model
  * over hosts whose chat templates differ, so unpinned counts are not comparable. Costs a
  * fraction of a cent on the default model.
  *
  * {{{set -a; . ./.env; set +a; ./mill grit.models.test.runMain grit.models.ReasoningBillingProbe}}}
  *
  * Not a test: `./mill __.test` makes no model calls.
  */
object ReasoningBillingProbe {

  private val System = "You are terse."

  def main(args: Array[String]): Unit = {
    val _ = args
    OpenRouterConfig.fromEnv(sys.env, ModelRole.Turn) match {
      case Left(invalid) => println(invalid.message)
      case Right(config) => run(config.copy(maxTokens = 600))
    }
  }

  private def run(config: OpenRouterConfig): Unit = {
    val http = HttpClient.newBuilder().connectTimeout(config.timeout).build()

    /** The host that served `request`, and the reply; pinned to `host` when given. */
    def send(
        request: ModelRequest,
        host: Option[String]
    ): Either[String, (String, Message.Assistant)] = {
      val body = OpenRouterJson.request(config.model, config.maxTokens, request)
      host.foreach(h =>
        body("provider") = ujson.Obj("order" -> ujson.Arr(h), "allow_fallbacks" -> false)
      )
      try {
        val response = http.send(
          HttpRequest
            .newBuilder(config.endpoint)
            .timeout(config.timeout)
            .header("Authorization", s"Bearer ${config.apiKey}")
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
            .build(),
          HttpResponse.BodyHandlers.ofString()
        )
        if (response.statusCode != 200) Left(s"HTTP ${response.statusCode}")
        else {
          val json = ujson.read(response.body)
          val served = json.obj.get("provider").flatMap(_.strOpt).getOrElse("?")
          OpenRouterJson.response(json).left.map(_.toString).map(served -> _)
        }
      } catch {
        case NonFatal(e) => Left(e.getClass.getSimpleName)
      }
    }

    val first = Message.User("Pick a whole number between 1 and 100. Reply with the number only.")
    val next = Message.User("Now double it. Reply with the number only.")
    val probe = for {
      served <- send(ModelRequest(System, Vector(first)), None)
      (host, reply) = served
      bare = reply.copy(blocks = reply.blocks.filter {
        case AssistantBlock.Reasoning(_, _) => false
        case _ => true
      })
      runs <- Vector(reply, bare, reply, bare).foldLeft[Either[String, Vector[Message.Assistant]]](
        Right(Vector())
      ) { (acc, earlier) =>
        acc.flatMap(done =>
          send(ModelRequest(System, Vector(first, earlier, next)), Some(host)).map(done :+ _._2)
        )
      }
    } yield (host, reply, runs)

    probe match {
      case Left(e) => println(s"probe failed: $e")
      case Right((host, reply, runs)) =>
        val text = reply.blocks.collect { case AssistantBlock.Reasoning(t, _) => t.length }.sum
        val replay = reply.blocks.collect { case AssistantBlock.Reasoning(_, Some(r)) =>
          ujson.write(r).length
        }.sum
        def inputs(parity: Int) =
          runs.zipWithIndex.collect {
            case (r, i) if i % 2 == parity => Tokens.value(r.usage.input)
          }
        println(s"model ${reply.model}, pinned to host $host")
        println(s"first reply's reasoning: $text chars of text, $replay chars of replay JSON")
        println(s"follow-up input, reasoning sent:    ${inputs(0).mkString(", ")}")
        println(s"follow-up input, reasoning dropped: ${inputs(1).mkString(", ")}")
        println(s"probe cost: ${Cost.total((reply +: runs).map(_.usage))}")
    }
  }
}
