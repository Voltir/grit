package grit.core.act

import grit.core.durable.InMemoryDurable
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Subject, Visibility}
import grit.dbos.sql.TestTx

/** What the moves' tests and a job's run's tests both stand their moves on: the models, the
  * catalog, the estimator and the database a move reads through.
  */
object MovesFixtures {

  /** Models whose every provider answers `answer`, at a priced cost, counting its calls; with
    * `crash`, the process dies inside the first.
    */
  final class Answering(answer: String, crash: Boolean = false) extends Models {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    @caps.unsafe.untrackedCaptures
    private var armed = crash
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = new Provider {
      def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
        calls += 1
        if (armed) { armed = false; throw new InMemoryDurable.Crash }
        Right(
          Message.Assistant(
            Vector(AssistantBlock.Text(answer)),
            StopReason.EndTurn,
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
