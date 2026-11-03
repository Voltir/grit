package grit.eval.harness.bench

import java.nio.charset.StandardCharsets

import scala.util.Try

import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.CallLine
import grit.models.{AnthropicJson, AnthropicRates}

/** One model call of a run: its message id, the model that answered it, and what it consumed,
  * priced by [[AnthropicRates]] (`costUsd` `None` for a model it has no rates for). Its usage is
  * the one its message began with: input and cache final, output not, since a message's
  * `output_tokens` grows as it streams.
  */
final case class Call(id: String, model: String, usage: Usage) extends caps.Pure

/** The grit coder's turn as its database recorded it: its conversation's id (its session), the
  * messages its last model call was sent, oldest first, the status DBOS gives its workflow
  * (`SUCCESS`, `PENDING`, `ERROR`, …), and the turn as a person reads it, as the eval's `sent`
  * renders it.
  */
final case class GritTurn(
    session: String,
    messages: Vector[Message],
    status: String,
    rendered: String
)

/** A tool call the run was refused, by tool and its input as JSON. */
final case class Denial(tool: String, input: String) extends caps.Pure

/** How a run ended: its reason (Claude Code's, as `success` or `error_max_turns`; for grit, its
  * turn's status or why it was stopped), whether it ended in error, the cost Claude Code estimated
  * for it (`None` for grit, which estimates none of its own), each tool call it was refused, the
  * run's usage (`None` when Claude Code's result gives none), and each model's.
  */
final case class Ended(
    reason: String,
    error: Boolean,
    claimedUsd: Option[BigDecimal],
    denials: Vector[Denial],
    usage: Option[Usage],
    byModel: Vector[(String, Usage)]
) extends caps.Pure

/** What a coder's run consumed, read from Claude Code's `stream-json` output ([[Consumed.of]]) or
  * from grit's call lines and turn ([[Consumed.ofTurn]]): its session, its model
  * calls in the order they began, and before its first edit of a file under its checkout the calls
  * made and the UTF-8 bytes its tools returned (both `None`: it made no such edit); and how it
  * ended (`None`: it wrote no result, as when it was killed); and the source of the key its init
  * line names (`None`: the line names none).
  */
final case class Consumed(
    session: Option[String],
    calls: Vector[Call],
    callsBeforeEdit: Option[Int],
    readBeforeEdit: Option[Long],
    ended: Option[Ended],
    apiKeySource: Option[String]
) extends caps.Pure {

  /** The run's usage: its result's, else every call's together (its output a lower bound, since a
    * run with no result, or whose result gives no usage, has only its calls' events); its cost
    * unknown when any call's is.
    */
  def usage: Usage = ended.flatMap(_.usage).getOrElse(Usage.total(calls.map(_.usage)))

  /** The run's cost by grit's published rates, the cross-check beside Claude Code's, summed over
    * the models of the result. A model with calls in the events costs those calls, plus the output
    * its result counts beyond their events at their output rate. A model with none (side calls)
    * costs its totals, when one card holds for them. `Left` in place of a figure, naming the model,
    * where a call's model has no usage or no rates, or where a model's calls or side calls may
    * straddle a card. `Left` alone when the run has no result.
    */
  def gritCost: Either[String, BigDecimal] = {
    def outputRate(c: Call): Option[BigDecimal] = {
      def at(n: Long) =
        AnthropicRates.cost(c.model, c.usage.copy(output = Tokens(n), costUsd = None))
      for {
        hi <- at(1000000L)
        lo <- at(0L)
      } yield (hi - lo) / BigDecimal(1000000)
    }
    // One model's cost: its calls' costs and its output beyond their events, at their rate when
    // they share one; or, with no call of its in the events, its side calls' totals when one card
    // holds for them.
    def priced(model: String, total: Usage): Either[String, BigDecimal] = {
      val own = calls.filter(c => AnthropicJson.isOrSnapshotOf(c.model, model))
      if (own.isEmpty)
        if (AnthropicRates.oneCard(model, Tokens.value(total.input)))
          AnthropicRates.cost(model, total).toRight("a model it has no rates for")
        else Left(s"$model's side calls may straddle a card")
      else {
        val costs = own.flatMap(_.usage.costUsd)
        val remaining = Tokens.value(total.output) - own.map(c => Tokens.value(c.usage.output)).sum
        val rates = own.map(outputRate)
        if (costs.size != own.size) Left("a model it has no rates for")
        else if (remaining == 0L) Right(costs.sum)
        else
          rates.flatten.distinct match {
            case Vector(rate) if rates.forall(_.isDefined) =>
              Right(costs.sum + BigDecimal(remaining) * rate)
            case _ => Left(s"$model's calls straddle a card")
          }
      }
    }
    ended match {
      case None => Left("the run's result gives no usage")
      case Some(e) =>
        calls
          .map(_.model)
          .distinct
          .find(m => !e.byModel.exists(k => AnthropicJson.isOrSnapshotOf(m, k._1))) match {
          case Some(m) => Left(s"$m's calls have no usage in the result")
          case None =>
            val parts = e.byModel.map { case (model, total) => priced(model, total) }
            parts.collectFirst { case Left(reason) => reason } match {
              case Some(reason) => Left(reason)
              case None => Right(parts.collect { case Right(cost) => cost }.sum)
            }
        }
    }
  }
}

object Consumed {

  /** The tools that edit a file. */
  val Edits: Set[String] = Set("Edit", "Write", "MultiEdit", "NotebookEdit")

  /** The calls of the grit coder whose output is `lines`: each line [[CallLine.answered]] wrote,
    * in order, named `call-1` on, its model's `anthropic/` left off.
    */
  def gritCalls(lines: Vector[String]): Vector[Call] =
    lines.flatMap(CallLine.read).zipWithIndex.map { case ((model, usage), i) =>
      val m = model.stripPrefix("anthropic/")
      Call(s"call-${i + 1}", m, usage.copy(costUsd = AnthropicRates.cost(m, usage)))
    }

  /** The tools of grit's that edit a file. */
  val GritEdits: Set[String] = Set("write", "edit")

  /** What the grit coder whose output is `lines` consumed, in its turn `turn`, stopped early for
    * `stopped` (as the report says it) when `Some`. Its calls are [[gritCalls]], side calls among
    * them; before its first edit, the calls
    * its turn made and the UTF-8 bytes its tools returned; its ending the turn's status as its
    * reason, or why it was stopped, in error unless it succeeded unstopped, each `run` that failed
    * a denial; its usage and each model's, its calls' together; no figure of its own beside
    * grit's, and no key source.
    */
  def ofTurn(lines: Vector[String], turn: GritTurn, stopped: Option[String]): Consumed = {
    val calls = gritCalls(lines)
    val toolCalls = turn.messages.collect { case a: Message.Assistant =>
      a.blocks.collect { case c: AssistantBlock.ToolCall => c }
    }.flatten
    val firstEdit = turn.messages.indexWhere {
      case a: Message.Assistant =>
        a.blocks.exists {
          case c: AssistantBlock.ToolCall => GritEdits(c.name)
          case _ => false
        }
      case _ => false
    }
    val before = if (firstEdit < 0) None else Some(turn.messages.take(firstEdit))
    val denials = turn.messages
      .collect { case Message.ToolResult(id, _, true) =>
        toolCalls.find(_.id == id)
      }
      .flatten
      .collect {
        case c if c.name == "run" => Denial(c.name, ujson.write(c.arguments))
      }
    val byModel = calls.map(_.model).distinct.map { m =>
      m -> Usage.total(calls.filter(_.model == m).map(_.usage))
    }
    Consumed(
      Some(turn.session),
      calls,
      before.map(_.count {
        case _: Message.Assistant => true
        case _ => false
      }),
      before.map(_.collect { case r: Message.ToolResult => bytes(r.content) }.sum),
      Some(
        Ended(
          stopped.getOrElse(turn.status),
          stopped.isDefined || turn.status != "SUCCESS",
          None,
          denials,
          Some(Usage.total(calls.map(_.usage))),
          byModel
        )
      ),
      None
    )
  }

  /** What the run whose output is `lines` consumed, its checkout at `checkout` (an absolute
    * path); a call counted once, at the last usage its message gave. `Left` naming the first line
    * that is not a JSON object.
    */
  def of(lines: Vector[String], checkout: String): Either[String, Consumed] =
    objects(lines).map(read(_, checkout))

  private def objects(lines: Vector[String]): Either[String, Vector[ujson.Obj]] =
    lines.zipWithIndex.foldLeft[Either[String, Vector[ujson.Obj]]](Right(Vector.empty)) {
      case (Right(read), (line, i)) =>
        Try(ujson.read(line)).toOption match {
          case Some(o: ujson.Obj) => Right(read :+ o)
          case _ => Left(s"line ${i + 1} of the run's output is not a JSON object")
        }
      case (refused, _) => refused
    }

  private def read(events: Vector[ujson.Obj], checkout: String): Consumed = {
    var order = Vector.empty[String]
    var calls = Map.empty[String, Call]
    var read = 0L
    var edit = Option.empty[(Int, Long)]
    var ended = Option.empty[Ended]
    events.foreach { e =>
      text(e, "type") match {
        case Some("assistant") =>
          for {
            m <- obj(e, "message")
            id <- text(m, "id")
          } {
            if (!calls.contains(id)) order = order :+ id
            calls = calls.updated(id, call(id, m))
            if (edit.isEmpty && edits(m, checkout)) edit = Some((order.indexOf(id), read))
          }
        case Some("user") if edit.isEmpty =>
          read += obj(e, "message").toVector.flatMap(blocks).map(returned).sum
        case Some("result") =>
          val claimed = e.value.get("total_cost_usd").flatMap(_.numOpt).map(BigDecimal(_))
          val models = byModel(e)
          ended = Some(
            Ended(
              text(e, "subtype").getOrElse(""),
              e.value.get("is_error").flatMap(_.boolOpt).getOrElse(false),
              claimed,
              e.value.get("permission_denials").flatMap(_.arrOpt).toVector.flatten.collect {
                case d: ujson.Obj =>
                  Denial(
                    text(d, "tool_name").getOrElse(""),
                    d.value.get("tool_input").fold("{}")(ujson.write(_))
                  )
              },
              resultUsage(e, claimed, models),
              models
            )
          )
        case _ => ()
      }
    }
    Consumed(
      events.iterator.flatMap(text(_, "session_id")).nextOption(),
      order.flatMap(calls.get),
      edit.map(_._1),
      edit.map(_._2),
      ended,
      events.iterator.flatMap(text(_, "apiKeySource")).nextOption()
    )
  }

  /** The run's usage from its result message: `modelUsage` summed over its models when it has
    * one, else its `usage`; its cost the result's `total_cost_usd`. `None` when it gives neither.
    */
  private def resultUsage(
      e: ujson.Obj,
      claimed: Option[BigDecimal],
      models: Vector[(String, Usage)]
  ): Option[Usage] =
    if (models.nonEmpty) Some(Usage.total(models.map(_._2)).copy(costUsd = claimed))
    else
      obj(e, "usage").map(o => AnthropicJson.usageOf(Some(o.value)).copy(costUsd = claimed))

  /** What the result's `modelUsage` gives each model, unpriced; empty when it gives none. */
  private def byModel(e: ujson.Obj): Vector[(String, Usage)] =
    obj(e, "modelUsage").fold(Vector.empty[(String, Usage)])(
      _.value.toVector.collect { case (model, o: ujson.Obj) =>
        model -> tokens(o)
      }
    )

  /** A `modelUsage` entry's tokens, its fields camelCase; unpriced. */
  private def tokens(o: ujson.Obj): Usage = {
    def count(k: String) = Tokens(o.value.get(k).flatMap(_.numOpt).fold(0L)(_.toLong))
    val read = count("cacheReadInputTokens")
    val write = count("cacheCreationInputTokens")
    Usage(count("inputTokens") + read + write, count("outputTokens"), read, None, write)
  }

  /** The call of the assistant message `m`, priced by its model. */
  private def call(id: String, m: ujson.Obj): Call = {
    val model = text(m, "model").getOrElse("")
    val usage = AnthropicJson.usageOf(obj(m, "usage").map(_.value))
    Call(id, model, usage.copy(costUsd = AnthropicRates.cost(model, usage)))
  }

  /** Whether `m` edits a file under `checkout`, or a relative path, which is under it. */
  private def edits(m: ujson.Obj, checkout: String): Boolean =
    blocks(m).exists { b =>
      text(b, "type").contains("tool_use") && text(b, "name").exists(Edits) &&
      obj(b, "input").exists(i =>
        (text(i, "file_path") orElse text(i, "notebook_path")).exists(p =>
          !p.startsWith("/") || p == checkout || p.startsWith(checkout + "/")
        )
      )
    }

  /** The UTF-8 bytes of the text the tool result `b` returned; 0 for any other block. */
  private def returned(b: ujson.Obj): Long =
    if (!text(b, "type").contains("tool_result")) 0L
    else
      b.value.get("content") match {
        case Some(ujson.Str(s)) => bytes(s)
        case Some(ujson.Arr(parts)) =>
          parts.collect { case p: ujson.Obj => text(p, "text").fold(0L)(bytes) }.sum
        case _ => 0L
      }

  private def bytes(s: String): Long = s.getBytes(StandardCharsets.UTF_8).length.toLong

  private def blocks(m: ujson.Obj): Vector[ujson.Obj] =
    m.value.get("content").flatMap(_.arrOpt).toVector.flatten.collect { case b: ujson.Obj => b }

  private def obj(o: ujson.Obj, k: String): Option[ujson.Obj] =
    o.value.get(k).collect { case v: ujson.Obj => v }

  private def text(o: ujson.Obj, k: String): Option[String] = o.value.get(k).flatMap(_.strOpt)
}
