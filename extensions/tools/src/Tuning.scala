package grit.tools

import grit.core.model.{CatalogJson, ModelId, ModelRef, ModelSetting, ModelSettings, Upstream}
import grit.core.tool.{Args, ArgsError, Field, Gate, Outcome, Tool, ToolName, ToolSpec}

/** The tool that proposes how grit should call a model: `propose_model_setting`, a measured
  * setting of one (model, upstream) pair, kept once a person approves it.
  */
object Tuning {

  /** The most runs a proposed setting may claim. */
  val MaxRuns = 10000

  /** `propose_model_setting`, keeping in `book` what a person approves. */
  def propose(book: ModelSettings^): Tool[ModelSetting]^{book} =
    new Tool(
      ToolSpec(
        ToolName("propose_model_setting"),
        "Propose one measured setting of a model served by one upstream, kept once " +
          "approved; the next turn's model catalog includes it, this one does not. " +
          "`setting` is one of " + CatalogJson.SettingNames.map(n => s"`$n`").mkString(", ") +
          "; `value` is in that setting's words (" +
          CatalogJson.SettingNames
            .map(n => s"$n: ${CatalogJson.settingWords(n).mkString(" | ")}")
            .mkString("; ") +
          "), and for `repairs` a comma-separated list, empty for none. `probe` names what " +
          "measured it; the setting held in `held` of `runs` runs. Refused, before anyone is asked, " +
          "when a value is not one of those, or `held` is more than `runs`.",
        Args
          .of(
            (
              model =
                Field.text("The model's OpenRouter id, the dated snapshot where there is one."),
              upstream = Field.text("The one upstream that served it, as its slug.").optional,
              setting = Field.oneOf("Which setting.", "strict", CatalogJson.SettingNames.drop(1)*),
              value = Field.text("The setting's value, in its words."),
              probe = Field.text("What measured it."),
              runs = Field.count("How many runs measured it.", 1, MaxRuns),
              held = Field.count("In how many of them it held.", 0, MaxRuns)
            )
          )
          .refine(a =>
            for {
              ref <- pair(a.model, a.upstream)
              setting <- CatalogJson
                .setting(a.setting, a.value)
                .left
                .map(_ =>
                  invalid(
                    "value",
                    s"one of ${CatalogJson.settingWords(a.setting).mkString(", ")}",
                    a.value
                  )
                )
              _ <- Either.cond(
                a.held <= a.runs,
                (),
                ArgsError.Invalid("held", s"at most `runs` (${a.runs})", a.held.toString)
              )
            } yield ModelSetting(ref, setting, a.probe, a.runs, a.held)
          )
      ),
      Gate.Ask(f => {
        val (name, value) = CatalogJson.spelled(f.setting)
        s"Keep a model setting for ${f.ref}: $name = $value, held in ${f.held} of ${f.runs} runs of ${f.probe}."
      }),
      f => s"${f.ref} ${CatalogJson.spelled(f.setting)(0)}",
      f =>
        book.keep(f) match {
          case Right(()) =>
            Outcome.Done(
              "Kept. The next turn's catalog has it; this turn keeps the one it started with."
            )
          case Left(why) => Outcome.Failed(s"Not kept: $why")
        }
    )

  /** The pair `model` at `upstream` names, or which of them is not an id. */
  private[tools] def pair(model: String, upstream: Option[String]): Either[ArgsError, ModelRef] =
    for {
      m <- ModelId
        .of(model)
        .toRight(invalid("model", "an OpenRouter model id, such as openai/gpt-oss-120b", model))
      u <- upstream match {
        case None => Right(None)
        case Some(up) =>
          Upstream
            .of(up)
            .map(Some(_))
            .toRight(invalid("upstream", "one OpenRouter upstream slug, such as fireworks", up))
      }
    } yield ModelRef(m, u)

  private def invalid(field: String, accepts: String, got: String): ArgsError =
    ArgsError.Invalid(field, accepts, ujson.Str(got).render())
}
