package grit.turn

import grit.core.edge.InMemoryEdges
import grit.core.id.{ConversationId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.Directory
import grit.core.prompt.SystemPrompt
import grit.core.store.{InMemoryConversationStore, InMemoryPrincipals, InMemoryVoiceStore, Origin}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSet, ToolSpec, Toolbox}
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
      Toolbox.Empty,
      Vector.empty,
      new FakeJot,
      budget(5)
    )
    TurnOffer
      .decide(hosting, tooling, TurnRef(c, TurnSeq.First))
      .flatMap(r => Prompts.prompt(r.prompt).left.map(e => TurnFailure.Store(e.toString)))
      .fold(f => throw new java.lang.AssertionError(f.toString), _.render)
  }

  /** A tool named `name` that says `name` back, asking first when `asks`. */
  private def tool(name: ToolName, asks: Boolean): Tool[String] =
    new Tool(
      ToolSpec(
        name,
        s"Says ${ToolName.value(name)}.",
        Args.of((text = Field.text("What."))).map(_.text)
      ),
      if (asks) Gate.Ask(t => t) else Gate.Free,
      t => t,
      _ => Outcome.Done(ToolName.value(name))
    )

  private def box(tools: Tool[String]*): Toolbox[{}] =
    Toolbox.of[{}](tools*).fold(d => throw new java.lang.AssertionError(d.toString), identity)

  /** The names of the tools `origin`'s first turn is offered, `about` offered everywhere and
    * `propose` and `probe` to the operator.
    */
  private def names(origin: Origin): Vector[String] = {
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
      new InMemoryPrincipals
    )
    val tooling = TurnTooling[{}](
      box(tool(ToolName("about"), asks = false)),
      box(tool(ToolName("propose"), asks = true), tool(ToolName("probe"), asks = true)),
      Vector.empty,
      new FakeJot,
      budget(5)
    )
    TurnOffer
      .decide(hosting, tooling, TurnRef(c, TurnSeq.First))
      .flatMap(r => ToolSets.get(r.tools).left.map(e => TurnFailure.Store(e.toString)))
      .fold(
        f => throw new java.lang.AssertionError(f.toString),
        _.tools.map(t => ToolName.value(t.name))
      )
  }

  private def expected(origin: Origin, called: Option[String]): String =
    SystemPrompt
      .of(
        Vector(TurnPrompt.Base, TurnPrompt.edge(origin)) ++ called.map(TurnPrompt.called) :+
          TurnPrompt.reach(None, ToolSet.Empty)
      )
      .render

  val tests = Tests {
    test("a TUI turn is offered the operator's tools, after those offered everywhere") {
      val dir = Directory.of("/work").fold(e => throw new java.lang.AssertionError(e), identity)
      names(Origin.Tui(dir, "default")) ==> Vector("about", "propose", "probe")
    }

    test("a Slack thread's turn and a task's run are offered none of the operator's tools") {
      names(slack) ==> Vector("about")
      names(Origin.Task("nightly", "1")) ==> Vector("about")
    }

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
