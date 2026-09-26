package grit.tools

import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{Assignment, CatalogVersion, ModelRef, Pinned, Settings}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, ToolSchema, ToolUse}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec}

/** The tool that measures how a model behaves at an upstream: `probe_pair`, a small battery
  * of calls to the pair, each check made once per run, reported as counts a model can turn
  * into facts ([[Facts.propose]]).
  */
object Probes {

  /** The most runs one probe makes. */
  val MaxRuns = 5

  /** The model calls one run makes: the strict and names check, the quoted-number check,
    * the replay check and the after-result check.
    */
  val CallsPerRun = 4

  /** The output budget of each call. */
  val MaxTokens = 300

  type ProbeArgs = (ref: ModelRef, runs: Int)

  /** `probe_pair`, calling each pair it probes through `models`. */
  def probe(models: Models^): Tool[ProbeArgs]^{models} =
    new Tool(
      ToolSpec(
        ToolName("probe_pair"),
        s"Measure how a model behaves at one upstream, over `runs` runs (1 to $MaxRuns) of " +
          s"$CallsPerRun calls each, at up to $MaxTokens output tokens a call; a person approves " +
          "the spend first. Reports, for each of strict, names, repairs, replay and " +
          "afterResult, in how many answered runs the behaviour showed. A call that fails " +
          "leaves its check unanswered for that run; when nothing answers, the probe fails " +
          "with the last error.",
        Args
          .of(
            (
              model =
                Field.text("The model's OpenRouter id, the dated snapshot where there is one."),
              upstream = Field.text("The one upstream to probe, as its slug.").optional,
              runs = Field.count("How many runs.", 1, MaxRuns)
            )
          )
          .refine(a => Facts.pair(a.model, a.upstream).map(ref => (ref = ref, runs = a.runs)))
      ),
      Gate.Ask(a =>
        s"Probe ${a.ref}: ${a.runs} runs, about ${a.runs * CallsPerRun} model calls at up to " +
          s"$MaxTokens output tokens each."
      ),
      a => a.ref.toString,
      a => {
        val provider = models.provider(pinned(a.ref))
        report(a.ref, a.runs, Vector.fill(a.runs)(once(provider)))
      }
    )

  /** What one run found: each check's finding, `None` when its call did not answer, and the
    * last error seen.
    */
  private final case class Run(
      offList: Option[Boolean],
      leaked: Option[Boolean],
      quoted: Option[Boolean],
      replayAccepted: Option[Boolean],
      userRefused: Option[Boolean],
      error: Option[String]
  )

  /** A pin for `ref` alone: every unmeasured setting as grit behaves without a profile. */
  private def pinned(ref: ModelRef): Pinned =
    Pinned(CatalogVersion("probe"), Assignment(ref, MaxTokens, None), Settings.of(None))

  private val System = "You are being probed. Call the tool you are offered when asked."

  private val Colors: Vector[String] = Vector("red", "green", "blue")

  private val pick = ToolSchema(
    "pick",
    "Pick a color.",
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "color" -> ujson.Obj("type" -> "string", "enum" -> ujson.Arr.from(Colors.map(ujson.Str(_))))
      ),
      "required" -> ujson.Arr("color"),
      "additionalProperties" -> false
    ),
    strict = true
  )

  private val count = ToolSchema(
    "count",
    "Count to a number.",
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj("n" -> ujson.Obj("type" -> "integer")),
      "required" -> ujson.Arr("n")
    )
  )

  private val PickAsked = Message.User("Call `pick` with the color purple.")

  private def calls(m: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    m.blocks.collect { case c: AssistantBlock.ToolCall => c }

  /** One run of every check against `provider`. */
  private def once(provider: Provider^): Run = {
    var error: Option[String] = None
    def ask(request: ModelRequest): Option[Either[ProviderError.Refused, Message.Assistant]] =
      provider.complete(request) match {
        case Right(m) => Some(Right(m))
        case Left(r: ProviderError.Refused) => error = Some(r.cause); Some(Left(r))
        case Left(e) => error = Some(e.cause); None
      }
    val picked = ask(ModelRequest(System, Vector(PickAsked), Vector(pick), ToolUse.Auto))
      .flatMap(_.toOption)
      .flatMap(m => calls(m).headOption.map(c => (m, c)))
    val offList =
      picked.map((_, c) => !c.arguments.obj.get("color").flatMap(_.strOpt).exists(Colors.contains))
    val leaked = picked.map((_, c) => c.name.contains("<|"))
    val counted = ask(
      ModelRequest(
        System,
        Vector(Message.User("Call `count` with n = 5.")),
        Vector(count),
        ToolUse.Auto
      )
    ).flatMap(_.toOption).flatMap(m => calls(m).headOption)
    val quoted = counted.map(c => c.arguments.obj.get("n").exists(_.strOpt.isDefined))
    val result = picked.map((m, c) => (m, Message.ToolResult(c.id, "picked", isError = false)))
    val replayAccepted = result
      .filter((m, _) =>
        m.blocks.exists { case AssistantBlock.Reasoning(_, Some(_)) => true; case _ => false }
      )
      .flatMap((m, r) =>
        ask(ModelRequest(System, Vector(PickAsked, m, r), Vector(pick), ToolUse.Auto))
      )
      .map(_.isRight)
    val userRefused = result
      .flatMap((m, r) =>
        ask(
          ModelRequest(
            System,
            Vector(PickAsked, m, r, Message.User("Now say done.")),
            Vector(pick),
            ToolUse.Auto
          )
        )
      )
      .map(_.isLeft)
    Run(offList, leaked, quoted, replayAccepted, userRefused, error)
  }

  private def report(ref: ModelRef, runs: Int, found: Vector[Run]): Outcome = {
    def tally(of: Run => Option[Boolean]): (Int, Int) = {
      val answered = found.flatMap(of)
      (answered.count(identity), answered.size)
    }
    val lines = Vector(
      ("strict", "a value off the schema's list came back", tally(_.offList)),
      ("names", "a tool name carried `<|`", tally(_.leaked)),
      ("repairs", "a number came back quoted", tally(_.quoted)),
      ("replay", "reasoning sent back was accepted", tally(_.replayAccepted)),
      ("afterResult", "a user message after a tool result was refused", tally(_.userRefused))
    )
    if (lines.forall(_(2)(1) == 0))
      Outcome.Failed(
        s"No check was answered in $runs runs; the last error: ${found.flatMap(_.error).lastOption.getOrElse("none")}"
      )
    else {
      val shown = lines.map { case (name, what, (k, n)) =>
        val verdict =
          if (name != "strict" || n == 0) ""
          else if (k == n) " (strict ignored)"
          else if (k == 0) " (strict held)"
          else " (strict held sometimes)"
        s"- $name: $what in $k of $n answered runs$verdict."
      }
      Outcome.Done(
        (s"Probed $ref, $runs runs:" +: shown :+
          "Propose each with propose_fact, probe `probe_pair`, runs the answered runs and held the count that bore it out.")
          .mkString("\n")
      )
    }
  }
}
