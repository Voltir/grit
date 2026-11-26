package grit.core.provider

import grit.core.message.{Message, Tokens}
import grit.core.model.{Density, Settings}

/** What a request to a model costs in input tokens, as its provider will bill them. An
  * estimate: only the provider's count is exact, and the usage ledger keeps both, so an
  * estimator can be checked against it. Each implementation knows a model's tokenizer, or
  * approximates one.
  */
trait TokenEstimator {

  /** The cost of `message` as part of a request. */
  def message(message: Message): Tokens

  /** The cost of a request's system prompt `prompt`. */
  def system(prompt: String): Tokens

  /** The cost of `tools`' definitions in a request offering them, with whatever the provider
    * adds to a request that offers any; zero for none. By default their names, descriptions
    * and parameter schemas ([[TokenEstimator.definitions]]), costed as system text.
    */
  def tools(tools: Vector[ToolSchema]): Tokens =
    if (tools.isEmpty) Tokens.Zero else system(TokenEstimator.definitions(tools))

  /** The cost of `request` as a whole: its system prompt, its [[tools]] and every message. */
  final def request(request: ModelRequest): Tokens =
    request.messages.map(message).foldLeft(system(request.system) + tools(request.tools))(_ + _)

  /** This estimator for a tokenizer that averages `density` characters of text a token. */
  def withDensity(density: Density): TokenEstimator

  /** This estimator for a call made under `settings`: [[withDensity]] at their density, or this
    * one when they give none.
    */
  final def withSettings(settings: Settings): TokenEstimator^{this} =
    settings.density.fold(this)(withDensity)
}

object TokenEstimator {

  /** `tools`' definitions as one text: each tool's name, description and parameter schema
    * (compact JSON) on lines of their own, the tools apart by a line break.
    */
  def definitions(tools: Vector[ToolSchema]): String =
    tools.map(t => s"${t.name}\n${t.description}\n${ujson.write(t.parameters)}").mkString("\n")
}
