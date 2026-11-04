package grit.eval.harness.sent

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.{ModelRequest, TokenEstimator}

/** What of a request nothing the model wrote later points back at: a proxy, never proof that a
  * part did not matter. A part steers a model without being quoted (a rule obeyed, a failure
  * that ruled a move out), and a name the model repeats may have come from elsewhere: this
  * ranks what to try cutting, and only rerunning the task without it shows whether it mattered.
  *
  * The model's *later output*, after a message of the request: each of its assistant messages
  * after that one, and the reply, read as their text, their reasoning and every string in their
  * calls' arguments. A *reference token* is a path (a run of letters, digits and `_.-/` holding
  * a `/`, a trailing `.` dropped) and its last segment; or an identifier of at least four
  * characters holding an upper-case letter, a digit or an underscore (`FileArea`,
  * `bench_trials`), or a dotted run of identifiers (`FileArea.sweep`, `schema.sql`); never a
  * plain lower-case word. A token is *new* at a message when the system prompt, the tools'
  * definitions and every message before it lack it.
  */
object Referenced {

  /** Lines of a result this long or longer count when the later output repeats them whole. */
  val LineChars: Int = 24

  /** A tool offered, whether any call the request holds or the reply makes names it, and what
    * offering it costs (the estimate of the tools less the estimate without it).
    */
  final case class Tool(name: String, called: Boolean, cost: Tokens) extends caps.Pure

  /** The tool result at message `at` (from 0), answering `call` to `tool` (the name its call
    * gave, or the id when no call in the request has it), costing `cost`: of its `chars`
    * characters, `pointed` are on lines the later output points back at.
    */
  final case class Result(
      at: Int,
      call: ToolCallId,
      tool: String,
      cost: Tokens,
      chars: Int,
      pointed: Int
  ) extends caps.Pure

  /** A request's tools ([[Referenced.tools]]) and tool results ([[Referenced.results]]). */
  final case class Read(tools: Vector[Tool], results: Vector[Result]) extends caps.Pure

  /** Each tool `request` offers, in order. */
  def tools(
      request: ModelRequest,
      reply: Option[Message.Assistant],
      estimator: TokenEstimator
  ): Vector[Tool] = {
    val named = (request.messages ++ reply.toVector).flatMap {
      case a: Message.Assistant => a.blocks.collect { case AssistantBlock.ToolCall(_, n, _) => n }
      case _ => Vector.empty
    }.toSet
    val all = Tokens.value(estimator.tools(request.tools))
    request.tools.map(t =>
      Tool(
        t.name,
        named.contains(t.name),
        Tokens(all - Tokens.value(estimator.tools(request.tools.filterNot(_ == t))))
      )
    )
  }

  /** Each tool result of `request`, in order, with the reply as the last of the later output.
    * A line of a result, stripped of a leading line number (digits and a tab, as `read` shows
    * one) or `path:line:` (as `search` shows one) and trimmed, is pointed back at when the later
    * output holds one of its new reference tokens, or, at [[LineChars]] or longer, the line
    * itself as a line of its own.
    */
  def results(
      request: ModelRequest,
      reply: Option[Message.Assistant],
      estimator: TokenEstimator
  ): Vector[Result] = {
    val messages = request.messages
    val toolOf = messages.flatMap {
      case a: Message.Assistant =>
        a.blocks.collect { case AssistantBlock.ToolCall(id, n, _) => id -> n }
      case _ => Vector.empty
    }.toMap
    // What the model wrote after each message: later(i) is the later output after message i.
    val written = messages.map {
      case a: Message.Assistant => Some(output(a))
      case _ => None
    }
    val last = reply.fold(Vector.empty[String])(output)
    val later = written.indices.reverse
      .scanLeft(Written(Set.empty, Set.empty).and(last)) { (after, i) =>
        written.lift(i + 1).flatten.fold(after)(w => after.and(w))
      }
      .reverse
    // The tokens sent before each message.
    val prior = messages
      .scanLeft(tokens(request.system) ++ tokens(TokenEstimator.definitions(request.tools))) {
        (seen, m) => seen ++ tokens(text(m))
      }
    messages.zipWithIndex.collect { case (r: Message.ToolResult, i) =>
      val seen = prior.lift(i).getOrElse(Set.empty)
      val after = later.lift(i).getOrElse(Written(Set.empty, Set.empty))
      val pointed = r.content.linesIterator.filter { line =>
        val bare = stripped(line)
        (bare.length >= LineChars && after.lines.contains(bare)) ||
        (tokens(line) -- seen).exists(after.tokens.contains)
      }
      Result(
        i,
        r.callId,
        toolOf.getOrElse(r.callId, ToolCallId.value(r.callId)),
        estimator.message(r),
        r.content.length,
        pointed.map(_.length).sum
      )
    }
  }

  /** The reference tokens and the lines (each [[stripped]]) of some later output. */
  private final case class Written(tokens: Set[String], lines: Set[String]) {
    def and(texts: Vector[String]): Written =
      Written(
        texts.foldLeft(tokens)(_ ++ Referenced.tokens(_)),
        texts.foldLeft(lines)(_ ++ Referenced.lines(_))
      )
  }

  /** What `a` wrote: its text, its reasoning and every string in its calls' arguments. */
  private def output(a: Message.Assistant): Vector[String] = a.blocks.flatMap {
    case AssistantBlock.Text(t) => Vector(t)
    case AssistantBlock.Reasoning(t, _) => Vector(t)
    case AssistantBlock.ToolCall(_, _, arguments) => strings(arguments)
  }

  private def strings(v: ujson.Value): Vector[String] = v match {
    case ujson.Str(s) => Vector(s)
    case ujson.Arr(items) => items.toVector.flatMap(strings)
    case ujson.Obj(fields) => fields.values.toVector.flatMap(strings)
    case _ => Vector.empty
  }

  private def text(m: Message): String = m match {
    case Message.User(t) => t
    case a: Message.Assistant => output(a).mkString("\n")
    case r: Message.ToolResult => r.content
  }

  private val PathRun = "[A-Za-z0-9_./-]+".r
  private val Identifier = "[A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)*".r
  private val Numbered = "^\\s*\\d+\t".r
  private val Located = "^[^\\s:]+:\\d+:".r

  /** `text`'s reference tokens. */
  private def tokens(text: String): Set[String] = {
    val paths = PathRun
      .findAllIn(text)
      .map(_.stripSuffix("."))
      .filter(p => p.contains('/') && p.exists(_.isLetterOrDigit))
      .flatMap(p => p +: p.split('/').lastOption.filter(_.nonEmpty).toVector)
    val names = Identifier.findAllIn(text).flatMap { run =>
      val whole = if (run.contains('.') && run.length >= 4) Vector(run) else Vector.empty
      whole ++ run.split('.').filter(named)
    }
    (paths ++ names).toSet
  }

  private def named(s: String): Boolean =
    s.length >= 4 && s.exists(c => c.isUpper || c.isDigit || c == '_')

  /** `text`'s lines, each [[stripped]]. */
  private def lines(text: String): Vector[String] = text.linesIterator.map(stripped).toVector

  /** `line` without a leading line number or `path:line:`, trimmed. */
  private def stripped(line: String): String =
    Located.replaceFirstIn(Numbered.replaceFirstIn(line, ""), "").trim
}
