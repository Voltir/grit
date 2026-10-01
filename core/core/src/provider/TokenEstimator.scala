package grit.core.provider

import grit.core.message.{Message, Tokens}

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

  /** The cost of `request` as a whole: its system prompt, its tools' definitions (costed as
    * system text) and every message.
    */
  final def request(request: ModelRequest): Tokens = {
    val tools =
      if (request.tools.isEmpty) Tokens.Zero
      else
        system(
          request.tools
            .map(t => s"${t.name}\n${t.description}\n${ujson.write(t.parameters)}")
            .mkString("\n")
        )
    request.messages.map(message).foldLeft(system(request.system) + tools)(_ + _)
  }
}
