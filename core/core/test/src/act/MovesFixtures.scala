package grit.core.act

import grit.core.durable.InMemoryDurable
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator, ToolUse}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Subject, Visibility}
import grit.dbos.sql.TestTx

/** What the moves' tests and a job's run's tests both stand their moves on: the models, the
  * catalog, the estimator and the database a move reads through.
  */
object MovesFixtures {

  /** Models whose every provider answers `answer`, at a priced cost, counting its calls and
    * keeping each request in [[requests]]; required to call a tool, it calls the request's first
    * tool, its arguments the next of `shapes` (the last again once they run out;
    * `{"answer": answer}` when there are none), the call's id `call-{n}` for the `n`th call.
    * With `crash`, the process dies inside the first call.
    */
  final class Answering(
      answer: String,
      crash: Boolean = false,
      shapes: Vector[ujson.Value] = Vector()
  ) extends Models {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    @caps.unsafe.untrackedCaptures
    var requests: Vector[ModelRequest] = Vector()
    @caps.unsafe.untrackedCaptures
    private var armed = crash
    @caps.unsafe.untrackedCaptures
    private var shaped = 0
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = new Provider {
      def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
        calls += 1
        requests = requests :+ r
        if (armed) { armed = false; throw new InMemoryDurable.Crash }
        val (blocks, stop) = r.use match {
          case ToolUse.Required =>
            val args =
              shapes.lift(shaped).orElse(shapes.lastOption).getOrElse(ujson.Obj("answer" -> answer))
            shaped += 1
            val tool = r.tools.headOption.fold("none")(_.name)
            (
              Vector(
                AssistantBlock.ToolCall(ToolCallId(s"call-$calls"), tool, ujson.read(args.render()))
              ),
              StopReason.ToolUse
            )
          case ToolUse.Auto | ToolUse.Off =>
            (Vector(AssistantBlock.Text(answer)), StopReason.EndTurn)
        }
        Right(
          Message.Assistant(
            blocks,
            stop,
            Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
            "test/summary"
          )
        )
      }
    }
  }

  /** Each role pinned to `test/<role>`, 1024 tokens out. */
  val TestCatalog: Catalog = {
    def role(name: String) =
      Assignment(
        ModelRef(
          ModelId.of(s"test/$name").getOrElse(throw new java.lang.AssertionError(name)),
          None
        ),
        1024,
        None
      )
    Catalog.of(Policy(role("turn"), role("summary"), role("query"), role("summary")), Vector.empty)
  }

  /** A token for each character of a message's or prompt's written form. */
  object PerChar extends TokenEstimator {
    def message(message: Message): Tokens = Tokens(message.toString.length.toLong)
    def system(prompt: String): Tokens = Tokens(prompt.length.toLong)
  }

  /** Every read in one transaction opened at `clearance`, whatever its subject, under
    * `visibility`.
    */
  final class FakeDb(
      clearance: Clearance = Clearance.of(Label.Public),
      visibility: Visibility = Visibility.Shipped
  ) extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(clearance, visibility))
  }
}
