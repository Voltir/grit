package grit.act.phase

import grit.core.clock.Clock
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{ArgRepair, NameRepair, StrictSchemas}
import grit.core.provider.{ModelRequest, Provider, ToolSchema, ToolUse}
import grit.core.schema.{Conforming, JsonSchema, Mismatch, Typed}
import grit.core.tool.Toolbox

/** A JSON ask's phases: its request, its reply read, and its one repair. */
object Shaping {

  /** `system`, `messages` and `schema` as the request that requires the model to call the one
    * tool `tool`, whose parameters are `schema`, described to the model as "Your reply: its
    * arguments are your answer."; reads only `strict`: sent `strict` when it is `Enforced` or
    * `WhenRequired`.
    */
  def request(
      system: String,
      messages: Vector[Message],
      tool: String,
      schema: JsonSchema,
      strict: StrictSchemas
  ): ModelRequest =
    ModelRequest(
      system,
      messages,
      Vector(ToolSchema(tool, Described, schema.json, strict = held(strict))),
      ToolUse.Required
    )

  /** `request` sent to `provider` through [[Asking.reply]], its retries included; its reply
    * the arguments of its first call of `tool` (a call's name as `names` repairs it), checked
    * by `reply`'s schema with `repairs` and read by `reply`. A reply that does not read is
    * answered with an error tool result for each call it made, giving the [[Mismatch]] or
    * `reply`'s refusal, or with a user message to call `tool` when it made none, and asked
    * again with the reply and its answer appended, up to `retries` times; then it is
    * `Unread`, naming the last failure. Each response is in `calls`, in order, whether or
    * not it read.
    */
  def shaped[T](
      provider: Provider^,
      request: ModelRequest,
      tool: String,
      reply: Typed[T],
      names: NameRepair,
      repairs: Set[ArgRepair],
      retries: Int,
      clock: Clock^
  ): Shaped[T] = {
    def ask(asked: ModelRequest, left: Int, calls: Vector[Message.Assistant]): Shaped[T] =
      Asking.reply(provider, asked, Hearing.silent(), clock) match {
        case Left(why) => Shaped.Failed(why, calls)
        case Right(answer) =>
          val made = calls :+ answer
          readOf(answer, tool, reply, names, repairs) match {
            case Right((conforming, value)) => Shaped.Read(conforming, value, made)
            case Left(failure) if left > 0 =>
              val told = asked.messages :+ answer :++ failure.told(answer, tool)
              ask(asked.copy(messages = told), left - 1, made)
            case Left(failure) => Shaped.Unread(failure.why(tool), made)
          }
      }
    ask(request, retries, Vector.empty)
  }

  /** Why a reply did not read. */
  private enum Failure {

    /** No call of the tool. */
    case NoCall

    /** A call whose arguments its schema refused. */
    case Mismatched(mismatch: Mismatch)

    /** A call whose conforming arguments its reader refused. */
    case Refused(why: String)

    /** As the shaped result says it. */
    def why(tool: String): String = this match {
      case NoCall => s"no call of `$tool`"
      case Mismatched(m) => s"its arguments do not match its schema: ${m.message}"
      case Refused(why) => s"its arguments do not read: $why"
    }

    /** As the model is told it, after `answer`: an error result for each call it made, or a
      * user message when it made none.
      */
    def told(answer: Message.Assistant, tool: String): Vector[Message] = {
      val text = this match {
        case NoCall => s"Your reply did not call `$tool`. Call `$tool` with your reply."
        case Mismatched(m) =>
          s"Your reply does not match its schema: ${m.message}. Call `$tool` again."
        case Refused(why) => s"Your reply could not be read: $why. Call `$tool` again."
      }
      answer.blocks.collect { case AssistantBlock.ToolCall(id, _, _) =>
        Message.ToolResult(id, text, isError = true)
      } match {
        case Vector() => Vector(Message.User(text))
        case results => results
      }
    }
  }

  /** `answer`'s first call of `tool`, named as `names` repairs it, checked and read. */
  private def readOf[T](
      answer: Message.Assistant,
      tool: String,
      reply: Typed[T],
      names: NameRepair,
      repairs: Set[ArgRepair]
  ): Either[Failure, (Conforming, T)] =
    for {
      args <- answer.blocks
        .collectFirst {
          case AssistantBlock.ToolCall(_, name, args) if Toolbox.named(name, names) == tool => args
        }
        .toRight(Failure.NoCall)
      conforming <- reply.schema.check(args, repairs).left.map(Failure.Mismatched(_))
      value <- reply.read(conforming).left.map(Failure.Refused(_))
    } yield (conforming, value)

  /** Whether an upstream treating schemas as `strict` says holds a forced call to its schema. */
  private def held(strict: StrictSchemas): Boolean = strict match {
    case StrictSchemas.Enforced | StrictSchemas.WhenRequired => true
    case StrictSchemas.Ignored | StrictSchemas.Rejected => false
  }

  private val Described = "Your reply: its arguments are your answer."
}

/** What a JSON ask came to: `calls`, the responses in order, and its conforming reply, or why
  * none read; `Failed` when the provider failed, after the responses before it.
  */
enum Shaped[T] extends caps.Pure {
  case Read(reply: Conforming, value: T, calls: Vector[Message.Assistant])
  case Unread(why: String, calls: Vector[Message.Assistant])
  case Failed(why: String, calls: Vector[Message.Assistant])
}
