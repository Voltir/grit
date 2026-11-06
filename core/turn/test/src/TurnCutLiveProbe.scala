package grit.turn

import grit.core.durable.InMemoryDurable
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Policy, Upstream}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.recipe.TurnRecipe
import grit.core.secret.Secret
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger}
import grit.models.{AnthropicProvider, StubProvider}

/** Live probe of a turn its input limit cuts, on Claude Opus 5.5 at its default effort, thinking
  * on: the check that every call of a cut turn sends the same tools and tool choice, which a
  * thinking block's signature binds and the cache needs (ADR 0052). One turn briefed for one
  * task, over the fixtures' in-memory stores, offered `peek` and `poke` over four files each
  * naming the next, the first padded to about 3,000 tokens, its input limit 7,000 tokens so the
  * limit cuts it after its second reply: four calls, a fifth if its last reply calls a tool.
  *
  * It passes when the API refuses no call, every call after the first reads from the cache at
  * least all the call before it read and wrote, and the turn ends through `task_status` or
  * text. It prints each call's usage and cost, and how many of the turn's calls were refused
  * in code ([[TurnLoop.EndingRefused]], [[TurnLoop.LastRefused]], [[TaskStatus.Early]]). It
  * stops before any call once its calls have cost [[Cap]].
  *
  * {{{set -a; . ~/Projects/kalandra/.env; set +a; ./mill grit.turn.test.runMain grit.turn.TurnCutLiveProbe}}}
  *
  * Not a test: `./mill __.test` makes no model calls. Without `ANTHROPIC_API_KEY` it says so
  * and makes none. It exits 1 when a check fails.
  */
object TurnCutLiveProbe {
  import TurnFixtures.*

  /** No call is made once the calls made have cost this much, in USD: 10¢. */
  private val Cap = BigDecimal("0.10")

  /** The turn's output budget per call, thinking included. */
  private val Output = 1500

  private val Limit = Tokens(7000)

  def main(args: Array[String]): Unit =
    Secret.read(sys.env, AnthropicProvider.KeyVar) match {
      case Left(why) =>
        println(
          s"[probe] skipped: ${AnthropicProvider.KeyVar} not read ($why), so no call was made"
        )
      case Right(key) =>
        val catalog = Catalog.of(
          Policy.one(
            Assignment(
              ModelRef(
                ModelId.of("anthropic/claude-opus-5-5").getOrElse(sys.error("no such model id")),
                Some(Upstream.AnthropicApi)
              ),
              Output,
              None
            ),
            role("summary"),
            role("query"),
            role("heard")
          ),
          Vector.empty
        )
        val metered = new Metered(new AnthropicProvider(key, catalog.pin.turn.first))
        val entries = new InMemoryEntryStore
        val pad = (1 to 120)
          .map(i =>
            s"Line $i of the notes: a card names its source, the day it was read, and who may read it."
          )
          .mkString("\n")
        val ws = new Files(
          Map(
            "a.txt" -> s"$pad\nThe next file is b.txt.",
            "b.txt" -> "The next file is c.txt.",
            "c.txt" -> "The next file is d.txt.",
            "d.txt" -> "This is the last file. The word is: lantern."
          )
        )
        val turn = say(
          entries,
          "Find the word: start at a.txt, and read each file the one before it names, one file " +
            "per call, with peek. Then tell me the word."
        )
        val said = new InMemoryDurable().run(turn.workflowId)(
          tooledBody(
            entries,
            metered,
            new InMemoryUsageLedger,
            new StubProvider(),
            NoClassifier,
            ws,
            tools(ws),
            12,
            limit = Some(Limit),
            catalog = catalog,
            recipe = TurnRecipe.Tasked
          )
        )
        val calls = metered.calls
        println(s"[probe] the turn: $said")
        calls.zipWithIndex.foreach { case ((_, reply), i) =>
          val u = reply.usage
          println(
            s"[probe] call $i: input ${u.input} (read ${u.cacheRead}, written ${u.cacheWrite}), " +
              s"output ${u.output}, ${cents(u.costUsd.getOrElse(BigDecimal(0)))}; stop ${reply.stop}"
          )
        }
        val refusals = Vector(TurnLoop.EndingRefused, TurnLoop.LastRefused).collect {
          case grit.core.tool.Outcome.Failed(why) => why
        } :+ TaskStatus.Early
        val refused = calls.lastOption.toVector.flatMap(_._1.messages).count {
          case Message.ToolResult(_, content, true) => refusals.exists(content.contains)
          case _ => false
        }
        println(s"[probe] calls refused in code: $refused")
        println(s"[probe] spent ${cents(metered.spent)} in all")
        val failures = Vector(
          Option.when(metered.failed.nonEmpty)(
            s"the API refused a call: ${metered.failed.mkString("; ")}"
          ),
          Option.when(calls.size < 4)(s"the turn made ${calls.size} calls, under the cut's four"),
          Option.when(!said.startsWith("replied:"))(s"the turn did not answer: $said")
        ).flatten ++ calls.zip(calls.drop(1)).zipWithIndex.collect {
          case (((_, before), (_, after)), i)
              if Tokens.value(after.usage.cacheRead) <
                Tokens.value(before.usage.cacheRead) + Tokens.value(before.usage.cacheWrite) =>
            s"call ${i + 1} read ${after.usage.cacheRead} from the cache, under the " +
              s"${Tokens.value(before.usage.cacheRead) + Tokens.value(before.usage.cacheWrite)} " +
              s"call $i read and wrote"
        }
        failures.foreach(f => println(s"[probe] FAILED: $f"))
        if (failures.nonEmpty) sys.exit(1)
    }

  /** A role no call of this probe reaches a model for: the stub answers it. */
  private def role(name: String): Assignment =
    Assignment(ModelRef(ModelId.of(s"test/$name").getOrElse(sys.error(name)), None), 1024, None)

  /** `underlying`, each request and its reply kept, refusing every call once [[Cap]] is spent. */
  private final class Metered(underlying: Provider^) extends Provider {
    // Replaced whole by the turn's own calls, made one after another on main's one thread, and
    // read by main once the turn has ended.
    @caps.unsafe.untrackedCaptures
    var calls = Vector.empty[(ModelRequest, Message.Assistant)]

    // As `calls`: replaced whole by the turn's calls on main's one thread.
    @caps.unsafe.untrackedCaptures
    var failed = Vector.empty[String]

    // As `calls`: replaced whole by the turn's calls on main's one thread.
    @caps.unsafe.untrackedCaptures
    var spent = BigDecimal(0)

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      if (spent >= Cap) {
        failed = failed :+ s"not called, the probe has spent ${cents(spent)}"
        Left(ProviderError.Refused(s"the probe's cap of ${cents(Cap)} is spent"))
      } else
        underlying.complete(request) match {
          case Right(reply) =>
            calls = calls :+ (request -> reply)
            spent += reply.usage.costUsd.getOrElse(BigDecimal(0))
            Right(reply)
          case Left(e) =>
            failed = failed :+ e.toString
            Left(e)
        }
  }

  private def cents(usd: BigDecimal): String =
    s"${(usd * 100).bigDecimal.stripTrailingZeros.toPlainString}¢"
}
