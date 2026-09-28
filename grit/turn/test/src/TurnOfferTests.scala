package grit.turn

import grit.core.edge.InMemoryEdges
import grit.core.id.{ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.prompt.SystemPrompt
import grit.core.store.{InMemoryConversationStore, InMemoryPrincipals, InMemoryVoiceStore, Origin}
import grit.core.tool.{ToolSet, Toolbox}
import grit.dbos.sql.TestTx

import utest.*

/** [[TurnOffer.decide]]: the prompt a turn is offered, by where it happens. */
object TurnOfferTests extends TestSuite {
  import TurnFixtures.{FakeJot, Prompts, ToolSets, budget}

  private val slack = Origin.Slack("T1", "C1", "1.0")

  /** The prompt `origin`'s first turn is offered, with `principals` saying who is who. */
  private def offered(origin: Origin, principals: InMemoryPrincipals): String = {
    given grit.core.store.Tx = TestTx.fake
    val conversations = new InMemoryConversationStore
    val c: ConversationId = conversations
      .findOrCreate(origin, PrincipalId.Local)
      .fold(e => throw new java.lang.AssertionError(e.toString), _.id)
    val edges = new InMemoryEdges
    val hosting = TurnHosting(
      conversations,
      Prompts,
      ToolSets,
      edges,
      edges,
      new InMemoryVoiceStore,
      principals
    )
    val tooling = TurnTooling[{}](
      Toolbox.of[{}]().fold(d => throw new java.lang.AssertionError(d.toString), identity),
      Vector.empty,
      new FakeJot,
      budget(5)
    )
    TurnOffer
      .decide(hosting, tooling, TurnRef(c, TurnSeq.First))
      .flatMap(r => Prompts.prompt(r.prompt).left.map(e => TurnFailure.Store(e.toString)))
      .fold(f => throw new java.lang.AssertionError(f.toString), _.render)
  }

  private def expected(origin: Origin, called: Option[String]): String =
    SystemPrompt
      .of(
        Vector(TurnPrompt.Base, TurnPrompt.edge(origin)) ++ called.map(TurnPrompt.called) :+
          TurnPrompt.reach(None, ToolSet.Empty)
      )
      .render

  val tests = Tests {
    test("a Slack turn's prompt says what its workspace calls the assistant, after its edge") {
      val principals = new InMemoryPrincipals
      principals.enrollAssistant(PrincipalId("slack:T1"), "Bort")(using TestTx.fake) ==> Right(())
      offered(slack, principals) ==> expected(slack, Some("Bort"))
    }

    test("with no name given in its workspace, a Slack turn's prompt has no name fragment") {
      offered(slack, new InMemoryPrincipals) ==> expected(slack, None)
    }
  }
}
