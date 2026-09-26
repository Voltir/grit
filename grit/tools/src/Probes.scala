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

  /** The model calls one run makes: the strict check on `pick`; then on `count`, the names
    * and quoted-number checks, the replay check and the after-result check.
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
          "afterResult, in how many answered runs the behaviour showed and the fact to propose " +
          "from it. A call that fails " +
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

  /** What one run found: each check's finding, `None` when its call did not answer, whether
    * the count call's reply carried reasoning to send back, and the last error seen.
    */
  private final case class Run(
      offList: Option[Boolean],
      leaked: Option[Boolean],
      quoted: Option[Boolean],
      reasoned: Boolean,
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

  private val CountAsked = Message.User("Call `count` with n = 5.")

  private def calls(m: Message.Assistant): Vector[AssistantBlock.ToolCall] =
    m.blocks.collect { case c: AssistantBlock.ToolCall => c }

  /** One run of every check against `provider`: strict on `pick`, the rest on `count`. */
  private def once(provider: Provider^): Run = {
    var error: Option[String] = None
    def ask(request: ModelRequest): Option[Either[ProviderError.Refused, Message.Assistant]] =
      provider.complete(request) match {
        case Right(m) => Some(Right(m))
        case Left(r: ProviderError.Refused) => error = Some(r.cause); Some(Left(r))
        case Left(e) => error = Some(e.cause); None
      }
    def called(request: ModelRequest) =
      ask(request).flatMap(_.toOption).flatMap(m => calls(m).headOption.map(c => (m, c)))
    val picked = called(ModelRequest(System, Vector(PickAsked), Vector(pick), ToolUse.Auto))
    val offList =
      picked.map((_, c) => !c.arguments.obj.get("color").flatMap(_.strOpt).exists(Colors.contains))
    val counted = called(ModelRequest(System, Vector(CountAsked), Vector(count), ToolUse.Auto))
    val leaked = counted.map((_, c) => c.name.contains("<|"))
    val quoted = counted.map((_, c) => c.arguments.obj.get("n").exists(_.strOpt.isDefined))
    val result = counted.map((m, c) => (m, Message.ToolResult(c.id, "counted", isError = false)))
    val reasoned = counted.exists((m, _) =>
      m.blocks.exists {
        case AssistantBlock.Reasoning(_, Some(_)) => true
        case _ => false
      }
    )
    val replayAccepted = result
      .filter(_ => reasoned)
      .flatMap((m, r) =>
        ask(ModelRequest(System, Vector(CountAsked, m, r), Vector(count), ToolUse.Auto))
      )
      .map(_.isRight)
    val userRefused = result
      .flatMap((m, r) =>
        ask(
          ModelRequest(
            System,
            Vector(CountAsked, m, r, Message.User("Now say done.")),
            Vector(count),
            ToolUse.Auto
          )
        )
      )
      .map(_.isLeft)
    Run(offList, leaked, quoted, reasoned, replayAccepted, userRefused, error)
  }

  /** A check: its setting, what it looks for, and, from `k` of `n` answered runs showing it,
    * the value to propose and how many runs bore that value out; `None`, nothing to propose.
    */
  private final case class Check(
      setting: String,
      showed: String,
      of: Run => Option[Boolean],
      propose: (Int, Int) => Option[(String, Int)]
  )

  private val Checks: Vector[Check] = Vector(
    Check(
      "strict",
      "a value off the schema's list came back",
      _.offList,
      (k, n) => Some(if (k == 0) ("enforced", n) else ("ignored", k))
    ),
    Check(
      "names",
      "a tool name carried `<|`",
      _.leaked,
      (k, n) => Some(if (k > 0) ("harmony-cut", k) else ("as-sent", n))
    ),
    Check(
      "repairs",
      "a number came back quoted",
      _.quoted,
      (k, _) => Option.when(k > 0)(("quoted-number", k))
    ),
    Check(
      "replay",
      "reasoning sent back was accepted",
      _.replayAccepted,
      (k, n) => Some(if (k == 0) ("dropped", n) else ("details", k))
    ),
    Check(
      "afterResult",
      "a user message after a tool result was refused",
      _.userRefused,
      (k, n) => Some(if (k > 0) ("in-last-result", k) else ("user-message", n))
    )
  )

  private def report(ref: ModelRef, runs: Int, found: Vector[Run]): Outcome = {
    val lines = Checks.map { c =>
      val answered = found.flatMap(c.of)
      val (k, n) = (answered.count(identity), answered.size)
      if (n == 0)
        if (c.setting == "replay" && found.exists(_.leaked.isDefined) && !found.exists(_.reasoned))
          (0, s"- replay: no reasoning to send back in $runs runs.")
        else (0, s"- ${c.setting}: not answered in $runs runs.")
      else
        (
          n,
          s"- ${c.setting}: ${c.showed} in $k of $n answered runs → " +
            c.propose(k, n)
              .fold("nothing to propose.")((v, held) =>
                s"propose ${c.setting} = $v, held $held of $n."
              )
        )
    }
    if (lines.forall(_(0) == 0) && !found.exists(_.leaked.isDefined))
      Outcome.Failed(
        s"No check was answered in $runs runs; the last error: ${found.flatMap(_.error).lastOption.getOrElse("none")}"
      )
    else
      Outcome.Done(
        (s"Probed $ref, $runs runs:" +: lines.map(_(1)) :+
          "Propose each as written with propose_fact, probe `probe_pair`; a check with no answered run, or nothing to propose, is left out.")
          .mkString("\n")
      )
  }
}
