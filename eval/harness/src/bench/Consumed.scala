package grit.eval.harness.bench

import java.nio.charset.StandardCharsets

import scala.util.Try

import grit.core.message.{Tokens, Usage}
import grit.models.{AnthropicJson, AnthropicRates}

/** One model call of a run: its message id, the model that answered it, and what it consumed,
  * priced by [[AnthropicRates]] (`costUsd` `None` for a model it has no rates for). Its usage is
  * the one its message began with: input and cache final, output not, since a message's
  * `output_tokens` grows as it streams.
  */
final case class Call(id: String, model: String, usage: Usage) extends caps.Pure

/** A tool call the run was refused, by tool and its input as JSON. */
final case class Denial(tool: String, input: String) extends caps.Pure

/** How a run ended, from its result message: Claude Code's reason (`success`, or a cap's or an
  * error's, such as `error_max_turns`), whether it ended in error, the cost Claude Code estimated
  * for it, each tool call it was refused, and the run's usage as its result gives it (`None`
  * when the result gives none).
  */
final case class Ended(
    reason: String,
    error: Boolean,
    claimedUsd: Option[BigDecimal],
    denials: Vector[Denial],
    usage: Option[Usage],
    byModel: Vector[(String, Usage)]
) extends caps.Pure

/** What a Claude Code run consumed, read from its `stream-json` output: its session, its model
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
      obj(e, "usage").map(
        tokens(
          _,
          "input_tokens",
          "output_tokens",
          "cache_read_input_tokens",
          "cache_creation_input_tokens"
        ).copy(costUsd = claimed)
      )

  /** What the result's `modelUsage` gives each model, unpriced; empty when it gives none. */
  private def byModel(e: ujson.Obj): Vector[(String, Usage)] =
    obj(e, "modelUsage").fold(Vector.empty[(String, Usage)])(
      _.value.toVector.collect { case (model, o: ujson.Obj) =>
        model -> tokens(
          o,
          "inputTokens",
          "outputTokens",
          "cacheReadInputTokens",
          "cacheCreationInputTokens"
        )
      }
    )

  /** A result's tokens, read by the names of their fields; unpriced. */
  private def tokens(
      o: ujson.Obj,
      input: String,
      output: String,
      read: String,
      write: String
  ): Usage = {
    def count(k: String) = o.value.get(k).flatMap(_.numOpt).fold(0L)(_.toLong)
    Usage(
      Tokens(count(input) + count(read) + count(write)),
      Tokens(count(output)),
      Tokens(count(read)),
      None,
      Tokens(count(write))
    )
  }

  /** The call of the assistant message `m`, priced by its model. */
  private def call(id: String, m: ujson.Obj): Call = {
    val model = text(m, "model").getOrElse("")
    def count(k: String) =
      obj(m, "usage").flatMap(_.value.get(k)).flatMap(_.numOpt).fold(0L)(_.toLong)
    val read = count("cache_read_input_tokens")
    val write = count("cache_creation_input_tokens")
    val usage = Usage(
      Tokens(count("input_tokens") + read + write),
      Tokens(count("output_tokens")),
      Tokens(read),
      None,
      Tokens(write)
    )
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
