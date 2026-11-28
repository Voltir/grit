package grit.eval.harness.bench

import java.nio.charset.StandardCharsets

import scala.util.Try

import grit.core.host.{Allowlisted, Argv, Sources}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.model.{Pinned, Pricing}
import grit.core.provider.CallLine
import grit.models.{AnthropicJson, AnthropicRates}
import grit.turn.TurnRecord

/** One model call of a run, its usage priced by [[AnthropicRates]], or when `reported` (a grit
  * call of a model it has no rates for) by the cost its provider reported, as grit's line printed
  * it ([[CallLine.read]]'s `rounded`); `costUsd` is None when neither prices it. Its usage is the
  * one its message began with: input and cache tokens final, output tokens not.
  */
final case class Call(id: String, model: String, usage: Usage, reported: Boolean = false)
    extends caps.Pure

/** The grit coder's task as its database recorded it: its conversation's id (its session), every
  * turn's own messages, oldest first, as `sent`'s `ConversationSent.own` gives them, the status DBOS gives its
  * last turn's workflow (`SUCCESS`, `PENDING`, `ERROR`, …), every turn of it as a person reads
  * them, as the eval's `sent` renders a turn, and why its last turn failed, as
  * `<step>: <reason>`, or as `grit work`'s own line ([[Consumed.reported]]), when it ended
  * without a reply: a turn fails inside a workflow DBOS records as `SUCCESS`; and what its last
  * turn's calls ran under, as that turn recorded it.
  */
final case class GritTurn(
    session: String,
    messages: Vector[Message],
    status: String,
    rendered: String,
    failed: Option[String],
    ranUnder: RanUnder
)

/** What a grit coder's turn recorded of what its calls ran under: `pin`, the pin they were made
  * under, its allotment's effort laid over its own (`TurnRecord.pinned`), and `inputLimit`, the
  * input its requests were held to (`TurnRecord.limits`, as `TurnLoop.inputLimit` decided it),
  * and `endedBy`, what ended it (`TurnRecord.endedBy`); each `None` when the turn recorded none,
  * `Left` saying why when its record does not read.
  */
final case class RanUnder(
    pin: Either[String, Option[Pinned]],
    inputLimit: Either[String, Option[Tokens]],
    endedBy: Either[String, Option[TurnRecord.EndedBy]]
)

object RanUnder {

  /** Nothing recorded: Claude Code's run, which keeps no grit turn. */
  val Unrecorded: RanUnder = RanUnder(Right(None), Right(None), Right(None))
}

object GritTurn {

  /** The task of the conversation `session`, its last turn's workflow under `status`, when its
    * turns could not be read back for `why`: no messages, rendered as a line saying so, and
    * failed for that reason, so its trial is kept, in error, priced from its output alone.
    */
  def unread(session: String, status: String, why: String): GritTurn =
    GritTurn(
      session,
      Vector.empty,
      status,
      s"Its turns could not be read back: $why",
      Some(why),
      RanUnder(Left(why), Left(why), Left(why))
    )
}

/** A tool call the run was refused, by tool and its input as JSON. */
final case class Denial(tool: String, input: String) extends caps.Pure

/** How a run ended: its reason (Claude Code's, as `success` or `error_max_turns`; for grit, why it
  * was stopped, why its turn failed, or its workflow's status), whether it ended in error, the
  * cost Claude Code estimated for it (`None` for grit, which estimates none of its own), each tool
  * call it was refused, the
  * run's usage (`None` when Claude Code's result gives none), and each model's.
  */
final case class CoderEnded(
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
  * line names (`None`: the line names none); and the work its tool calls did.
  */
final case class Consumed(
    session: Option[String],
    calls: Vector[Call],
    callsBeforeEdit: Option[Int],
    readBeforeEdit: Option[Long],
    ended: Option[CoderEnded],
    apiKeySource: Option[String],
    work: Consumed.Work
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
    // USD per output token, at the tier the call's prompt took.
    def outputRate(c: Call): Option[BigDecimal] =
      AnthropicRates.pricing(c.model).map(_.at(c.usage.input).outputPerToken)
    // One model's cost: its calls' costs and its output beyond their events, at their rate when
    // they share one; or, with no call of its in the events, its side calls' totals when one card
    // holds for them.
    def priced(model: String, total: Usage): Either[String, BigDecimal] = {
      val own = calls.filter(c => AnthropicJson.isOrSnapshotOf(c.model, model))
      if (own.isEmpty)
        AnthropicRates
          .ofTotal(model, total)
          .map(_.cost(total))
          .toRight(s"$model's side calls may straddle a card")
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

  /** Counts of a transcript's tool calls, as [[Tried.work]] names them: every call, its edits,
    * those of them whose result was an error, its suite runs, its commits, and its runs the
    * machine guard refused, by its lead ([[Allowlisted.GuardLead]]; none for a harness without
    * one), and its commits refused, by theirs ([[Allowlisted.CommitLead]]; likewise); its shell reads of Scala sources ([[Sources.Scala]]), those of them the
    * rule points at (what a run with no classifier gives the line to), those answered after the
    * line ([[Sources.Lead]]), and those whose next call names a tool the line names.
    */
  final case class Work private (
      tools: Int,
      edits: Int,
      failedEdits: Int,
      suiteRuns: Int,
      commits: Int,
      guarded: Int,
      refusedCommits: Int,
      sourceReads: Int,
      ruled: Int,
      pointed: Int,
      taken: Int
  ) extends caps.Pure

  object Work {

    /** One tool call: whether it is an edit, whether its result was an error, the command line
      * it ran when it is a shell run, whether the machine guard refused it, whether it was a
      * commit refused, and how it stands to the line naming what reads Scala sources.
      */
    final case class Used(
        edit: Boolean,
        failed: Boolean,
        command: Option[String],
        guarded: Boolean = false,
        source: Source = Source.None,
        refusedCommit: Boolean = false
    ) extends caps.Pure

    /** How a tool call stands to the line naming what reads Scala sources. */
    enum Source extends caps.Pure {

      /** It was no shell read of Scala sources. */
      case None

      /** It was one: whether the rule points at it, whether its answer began with the line, and
        * whether the next call named a tool the line names.
        */
      case Read(ruled: Boolean, pointed: Boolean, taken: Boolean)
    }

    /** The counts of `used`: a suite run is a command running `scripts/check` with an argument
      * not starting with `-` before the first shell separator or redirect (a suite or module
      * named, not a flag alone), a commit one holding `git commit` (so `git -C d commit` is not
      * one).
      */
    def of(used: Vector[Used]): Work = {
      val commands = used.flatMap(_.command)
      val reads = used.collect { case Used(_, _, _, _, r: Source.Read, _) => r }
      Work(
        used.size,
        used.count(_.edit),
        used.count(u => u.edit && u.failed),
        commands.count(suiteRun),
        commands.count(_.contains("git commit")),
        used.count(_.guarded),
        used.count(_.refusedCommit),
        reads.size,
        reads.count(_.ruled),
        reads.count(_.pointed),
        reads.count(_.taken)
      )
    }

    /** Words that end a command, or redirect its output, rather than give it an argument. */
    private val Ends = Set("&&", "||", ";", "|", "&")

    private def suiteRun(command: String): Boolean = {
      val words = command.split("\\s+").toVector.filter(_.nonEmpty)
      words.zipWithIndex.exists { (w, i) =>
        (w == "scripts/check" || w.endsWith("/scripts/check")) &&
        words
          .drop(i + 1)
          .takeWhile(w => !Ends(w) && !w.exists(c => c == '>' || c == '<'))
          .exists(!_.startsWith("-"))
      }
    }
  }

  /** The tools that edit a file. */
  val Edits: Set[String] = Set("Edit", "Write", "MultiEdit", "NotebookEdit")

  /** The calls of the grit coder whose output is `lines`: each line [[CallLine.answered]] wrote,
    * in order, named `call-1` on, its model's `anthropic/` left off, priced as [[Call]] says.
    */
  def gritCalls(lines: Vector[String]): Vector[Call] =
    lines.flatMap(CallLine.read).zipWithIndex.map { case ((model, usage, rounded), i) =>
      val m = model.stripPrefix("anthropic/")
      val id = s"call-${i + 1}"
      AnthropicRates.pricing(m) match {
        case Some(p) => Call(id, m, Pricing.priced(usage, Some(p)))
        case None => Call(id, m, usage.copy(costUsd = rounded), reported = rounded.isDefined)
      }
    }

  /** The tools of grit's that edit a file. */
  val GritEdits: Set[String] = Set("write", "edit")

  /** `turn`, its failure `grit work`'s own report when its steps name none and `grit work` exited
    * `exit`, not 0, writing `errors`: their last line that begins `[main]`. A turn that stalls on
    * a reply (silent, or out of output) records no failure in its steps, only in its outcome,
    * which `grit work` prints.
    */
  def reported(turn: GritTurn, exit: Int, errors: String): GritTurn =
    if (turn.failed.isDefined || exit == 0) turn
    else turn.copy(failed = errors.linesIterator.filter(_.startsWith("[main]")).toVector.lastOption)

  /** What the grit coder whose output is `lines` consumed, in its turn `turn`, stopped early for
    * `stopped` (as the report says it) when `Some`. Its calls are [[gritCalls]], side calls among
    * them; before its first edit, the calls
    * its turn made and the UTF-8 bytes its tools returned; its ending's reason why it was stopped,
    * else why its turn failed, else its workflow's status, in error unless its turn succeeded
    * unstopped under a workflow that succeeded, each `run` that failed
    * a denial; its usage and each model's, its calls' together; no figure of its own beside
    * grit's, and no key source; its work its turn's tool calls, its edits those of
    * [[GritEdits]] and its shell runs those of `run`.
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
    val failed = turn.messages.collect { case Message.ToolResult(id, _, true) => id }.toSet
    val results = turn.messages.collect { case Message.ToolResult(id, text, _) => id -> text }.toMap
    val guarded = turn.messages.collect {
      case Message.ToolResult(id, text, true) if text.startsWith(Allowlisted.GuardLead) => id
    }.toSet
    val refusedCommits = turn.messages.collect {
      case Message.ToolResult(id, text, true) if text.startsWith(Allowlisted.CommitLead) => id
    }.toSet
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
        CoderEnded(
          stopped.orElse(turn.failed).getOrElse(turn.status),
          stopped.isDefined || turn.failed.isDefined || turn.status != "SUCCESS",
          None,
          denials,
          Some(Usage.total(calls.map(_.usage))),
          byModel
        )
      ),
      None,
      Work.of(toolCalls.zipWithIndex.map { (c, i) =>
        def field(k: String) = c.arguments.objOpt.flatMap(_.get(k)).toVector
        val program = field("program").flatMap(_.strOpt)
        val args = field("args").flatMap(_.arrOpt).flatten.flatMap(_.strOpt)
        val argv =
          program.headOption.flatMap(p => Argv.of(p, args*).toOption).filter(_ => c.name == "run")
        val source = argv
          .flatMap(a => Sources.Scala.read(a).map(r => (a, r)))
          .fold(Work.Source.None) { (a, r) =>
            val answer = results.getOrElse(c.id, "")
            val pointed = answer.startsWith(Sources.Lead)
            val line = answer.linesIterator.nextOption().getOrElse("")
            Work.Source.Read(
              Sources.Scala.ruled(a, r),
              pointed,
              pointed && toolCalls.lift(i + 1).exists(n => line.contains(s"`${n.name}`"))
            )
          }
        Work.Used(
          GritEdits(c.name),
          failed(c.id),
          Option.when(c.name == "run")((program ++ args).mkString(" ")),
          guarded(c.id),
          source,
          refusedCommits(c.id)
        )
      })
    )
  }

  /** What the run whose output is `lines` consumed, its checkout at `checkout` (an absolute
    * path); a call counted once, at the last usage its message gave; its work its `tool_use`
    * blocks, its edits those of [[Edits]] and its shell runs those of `Bash`. `Left`
    * naming the first line that is not a JSON object.
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
    var ended = Option.empty[CoderEnded]
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
            CoderEnded(
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
      events.iterator.flatMap(text(_, "apiKeySource")).nextOption(),
      work(events)
    )
  }

  /** A tool call failed when its `tool_result` says `is_error`. */
  private def work(events: Vector[ujson.Obj]): Work = {
    def blocksOf(kind: String) = events
      .filter(text(_, "type").contains(kind))
      .flatMap(e => obj(e, "message").toVector.flatMap(blocks))
    val failed = blocksOf("user")
      .filter(b =>
        text(b, "type").contains("tool_result") &&
          b.value.get("is_error").flatMap(_.boolOpt).contains(true)
      )
      .flatMap(text(_, "tool_use_id"))
      .toSet
    Work.of(blocksOf("assistant").filter(text(_, "type").contains("tool_use")).map { b =>
      val name = text(b, "name").getOrElse("")
      Work.Used(
        Edits(name),
        text(b, "id").exists(failed),
        if (name == "Bash") obj(b, "input").flatMap(text(_, "command")) else None
      )
    })
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
    Call(id, model, Pricing.priced(usage, AnthropicRates.pricing(model)))
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
