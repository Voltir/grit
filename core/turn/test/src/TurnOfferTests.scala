package grit.turn

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.edge.InMemoryEdges
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  KnowledgeSourceName,
  PrincipalId,
  QuestionName,
  ToolCallId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.period.Probability
import grit.core.persona.Persona
import grit.core.place.{Directory, Place, Reaches, Service, WorksIn}
import grit.core.prompt.SystemPrompt
import grit.core.recipe.{ByFocus, Offering, ServiceOffer, Shaping, TurnRecipe}
import grit.core.store.{
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryVoiceStore,
  Origin,
  Payload
}
import grit.core.tool.{
  Args,
  Bound,
  Field,
  Gate,
  Hosted,
  Outcome,
  Repairs,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.core.triage.{
  Bound as TriageBound,
  Gate as TriageGate,
  KnowledgeSource,
  KnowledgeSources,
  Reading,
  Tags
}
import grit.dbos.sql.TestTx

import utest.*

/** [[TurnOffer.decide]]: the prompt a turn is offered, by where it happens. */
object TurnOfferTests extends TestSuite {
  import TurnFixtures.{FakeJot, Prompts, ToolSets, budget}

  private val slack = Origin.Slack("T1", "C1", "1.0")

  /** The prompt `origin`'s first turn is offered, and
    * the root it was recorded with; the turn starts with `root`.
    */
  private def offeredAs(
      origin: Origin,
      root: Payload,
      persona: Persona = Persona.Grit,
      weighed: Option[Tags] = None
  ): (String, TurnOffer.Root) = {
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
      new InMemoryVoiceStore
    )
    val tooling = TurnTooling[{}](
      Toolbox.of[{}]().fold(d => throw new java.lang.AssertionError(d.toString), identity),
      Toolbox.Empty,
      Vector.empty,
      new FakeJot,
      budget(5),
      persona = persona
    )
    val entries = new InMemoryEntryStore
    entries.insert(Entry(EntryId("root"), c, TurnSeq.First, None, EntrySeq(0), root, Instant.EPOCH))
    TurnOffer
      .decide(hosting, entries, tooling, TurnRef(c, TurnSeq.First), weighed)
      .flatMap(r =>
        Prompts
          .prompt(r.prompt)
          .left
          .map(e => TurnFailure.Store(e.toString))
          .map(_.render -> r.root)
      )
      .fold(f => throw new java.lang.AssertionError(f.toString), identity)
  }

  /** The prompt `origin`'s first turn, a person's message to grit, is offered. */
  private def offered(origin: Origin): String =
    offeredAs(origin, Payload.Message(Message.User("hi")))._1

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
      new InMemoryVoiceStore
    )
    val tooling = TurnTooling[{}](
      box(tool(ToolName("about"), asks = false)),
      box(tool(ToolName("propose"), asks = true), tool(ToolName("probe"), asks = true)),
      Vector.empty,
      new FakeJot,
      budget(5)
    )
    TurnOffer
      .decide(hosting, new InMemoryEntryStore, tooling, TurnRef(c, TurnSeq.First), None)
      .flatMap(r => ToolSets.get(r.tools).left.map(e => TurnFailure.Store(e.toString)))
      .fold(
        f => throw new java.lang.AssertionError(f.toString),
        _.tools.map(t => ToolName.value(t.name))
      )
  }

  private val github: Service =
    Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)

  private def named(name: String): ToolName =
    ToolName.of(name).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** An advert's entry for a tool named `name`, asking first when `asks`. */
  private def advert(name: String, asks: Boolean = false): ToolSet.Entry =
    ToolSet.Entry(named(name), s"Does $name.", ujson.Obj("type" -> "object"), asks, Retry.Rerun)

  /** `hosted`, a hosted tool the engine describes, named `name`. */
  private def described(name: String): Hosted[String] =
    new Hosted(
      ToolSpec(named(name), s"Does $name.", Args.of((text = Field.text("What."))).map(_.text)),
      Gate.Free,
      t => t
    )

  /** The recorded offer of `slack`'s first turn, linked by `links`, and its set's names, while
    * an edge at `github`'s place advertises `entries`; the engine has `about` of its own and
    * describes the hosted `fetch`.
    */
  private def linked(
      links: Vector[WorksIn],
      entries: Vector[ToolSet.Entry]
  ): (TurnOffer.Recorded, Vector[String]) = {
    given grit.core.store.Tx = TestTx.fake
    val conversations = new InMemoryConversationStore
    val c: ConversationId = conversations
      .findOrCreate(slack, PrincipalId.Local)
      .fold(e => throw new java.lang.AssertionError(e.toString), _.id)
    val edges = new InMemoryEdges
    val set =
      ToolSet.of(entries).fold(d => throw new java.lang.AssertionError(d.toString), identity)
    ToolSets.keep(set)
    edges.advertiseAs(edges.register(Set(github.place)), github.place, set, Vector.empty)
    val hosting = TurnHosting(
      conversations,
      Prompts,
      ToolSets,
      edges,
      edges,
      new InMemoryVoiceStore
    )
    val tooling = TurnTooling[{}](
      box(tool(ToolName("about"), asks = false)),
      Toolbox.Empty,
      Vector(described("fetch")),
      new FakeJot,
      budget(5),
      worksIn = links
    )
    TurnOffer
      .decide(hosting, new InMemoryEntryStore, tooling, TurnRef(c, TurnSeq.First), None)
      .flatMap(r =>
        ToolSets
          .get(r.tools)
          .left
          .map(e => TurnFailure.Store(e.toString))
          .map(set => r -> set.tools.map(t => ToolName.value(t.name)))
      )
      .fold(f => throw new java.lang.AssertionError(f.toString), identity)
  }

  private val elsewhere: Service =
    Service.of("elsewhere").fold(e => throw new java.lang.AssertionError(e), identity)

  /** The recorded offer of `slack`'s first turn, rooted on `root`, its set's names and its
    * rendered prompt: the conversation works in `github`, whose edge advertises
    * `github_search`, and reaches `elsewhere`, whose edge advertises `entries`; the engine has
    * `about` of its own.
    */
  private def reaching(
      root: Payload,
      entries: Vector[ToolSet.Entry],
      recipe: TurnRecipe = TurnRecipe.Shipped,
      knowledge: KnowledgeSources = KnowledgeSources.Empty,
      weighed: Option[Tags] = None
  ): (TurnOffer.Recorded, Vector[String], String) = {
    given grit.core.store.Tx = TestTx.fake
    val conversations = new InMemoryConversationStore
    val c: ConversationId = conversations
      .findOrCreate(slack, PrincipalId.Local)
      .fold(e => throw new java.lang.AssertionError(e.toString), _.id)
    val edges = new InMemoryEdges
    def advertising(at: Place, tools: Vector[ToolSet.Entry]): Unit = {
      val set =
        ToolSet.of(tools).fold(d => throw new java.lang.AssertionError(d.toString), identity)
      ToolSets.keep(set)
      edges.advertiseAs(edges.register(Set(at)), at, set, Vector.empty)
    }
    advertising(github.place, Vector(advert("github_search")))
    advertising(elsewhere.place, entries)
    val hosting = TurnHosting(
      conversations,
      Prompts,
      ToolSets,
      edges,
      edges,
      new InMemoryVoiceStore
    )
    val slackTeams = Place.under(grit.core.place.Namespace.Slack, Vector.empty)
    val tooling = TurnTooling[{}](
      box(tool(ToolName("about"), asks = false)),
      Toolbox.Empty,
      Vector.empty,
      new FakeJot,
      budget(5),
      worksIn = Vector(WorksIn(slackTeams, github)),
      reaches = Vector(Reaches(slackTeams, elsewhere)),
      recipe = recipe,
      knowledge = knowledge
    )
    val log = new InMemoryEntryStore
    log.insert(Entry(EntryId("root"), c, TurnSeq.First, None, EntrySeq(0), root, Instant.EPOCH))
    TurnOffer
      .decide(hosting, log, tooling, TurnRef(c, TurnSeq.First), weighed)
      .flatMap(r =>
        (for {
          set <- ToolSets.get(r.tools)
          prompt <- Prompts.prompt(r.prompt)
        } yield (r, set.tools.map(t => ToolName.value(t.name)), prompt.render)).left
          .map(e => TurnFailure.Store(e.toString))
      )
      .fold(f => throw new java.lang.AssertionError(f.toString), identity)
  }

  private val repo: KnowledgeSourceName =
    KnowledgeSourceName.of("repo").fold(e => throw new java.lang.AssertionError(e), identity)

  /** A catalog whose one source, `repo`, is supplied by `github`'s tools. */
  private val repoInGithub: KnowledgeSources = KnowledgeSources
    .of(Vector(KnowledgeSource(repo, "the repository", Place.Everywhere, Some(github))))
    .fold(n => throw new java.lang.AssertionError(n), identity)

  /** Triage's tags for a root whose `repo` source reads `p`. */
  private def repoReads(p: Double): Option[Tags] = Some(
    Tags.Weighed(
      VectorMap(QuestionName.per(Tags.V2.sourcePrefix, repo) -> Answer.YesNo(p)),
      "jev",
      Usage.Zero
    )
  )

  /** Heard turns offered a service's tools when one of its sources reads at least 0.2, their
    * windows drawn `narrow`; addressed turns as shipped.
    */
  private val narrow = Width.Within(Tokens(9000), 4)
  private val heardBySource = TurnRecipe(
    ByFocus.both(Shaping(narrow, Offering.BySource(Probability.clamped(0.2)))),
    TurnRecipe.Shipped.addressed
  )

  private val heardRoot = Payload.Heard("someone should look at the repo")

  private def set(entries: ToolSet.Entry*): ToolSet =
    ToolSet.of(entries.toVector).fold(d => throw new java.lang.AssertionError(d.toString), identity)

  private def expected(origin: Origin, persona: Persona): String =
    SystemPrompt
      .of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.Candour,
          TurnPrompt.Answering,
          TurnPrompt.edge(origin)
        ) ++
          TurnPrompt.destination(origin) ++ TurnPrompt.called(persona, origin) :+
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

    test(
      "every turn is told candour after the base; a Slack turn is told where its reply goes after its edge, a TUI turn is not"
    ) {
      // Joined by hand, not through SystemPrompt.of, so the order within each layer is pinned.
      offered(slack) ==> Vector(
        TurnPrompt.Base.text,
        TurnPrompt.Candour.text,
        TurnPrompt.Answering.text,
        TurnPrompt.edge(slack).text,
        "Your reply is posted in this thread and nowhere else. You can post anywhere else " +
          "only by calling a tool that does it, and only if one is offered to you.",
        "In this workspace you are called grit.",
        TurnPrompt.reach(None, ToolSet.Empty).text
      ).mkString("\n\n")
      val dir = Directory.of("/work").fold(e => throw new java.lang.AssertionError(e), identity)
      val tui = Origin.Tui(dir, "default")
      offered(tui) ==> Vector(
        TurnPrompt.Base.text,
        TurnPrompt.Candour.text,
        TurnPrompt.Answering.text,
        TurnPrompt.edge(tui).text,
        TurnPrompt.reach(Some(Place.of(dir)), ToolSet.Empty).text
      ).mkString("\n\n")
    }

    test("a Slack turn's prompt says the name its deployment's persona declares, after its edge") {
      val pip = Persona.of("Pip").fold(e => throw new java.lang.AssertionError(e), identity)
      val (prompt, _) =
        offeredAs(slack, Payload.Message(Message.User("hi")), pip)
      prompt ==> expected(slack, pip)
    }

    test(
      "a heard message triage read as directed at grit is recorded named, and told so in the unprompted fragment's place"
    ) {
      def triaged(toGrit: Double) = Some(
        Tags.Weighed(
          VectorMap(
            Tags.V2.gap -> Answer.Choice("asks", Vector(Answer.Weight("asks", 1.0)), 1.0),
            Tags.V2.open -> Answer.YesNo(0.9),
            Tags.V2.to -> Answer.YesNo(0.9),
            Tags.V3.toGrit -> Answer.YesNo(toGrit)
          ),
          "jev",
          Usage.Zero
        )
      )
      def prompt(fragment: grit.core.prompt.Fragment) = SystemPrompt
        .of(
          Vector(
            TurnPrompt.Base,
            TurnPrompt.Candour,
            TurnPrompt.Answering,
            TurnPrompt.edge(slack)
          ) ++ TurnPrompt.destination(slack) ++ TurnPrompt.called(Persona.Grit, slack) ++
            Vector(fragment, TurnPrompt.reach(None, ToolSet.Empty))
        )
        .render
      val heard = Payload.Heard("bort, is it Thursday?")
      // At the bound the gate's directed branch reads, and just under it.
      offeredAs(slack, heard, weighed = triaged(0.5)) ==> (
        prompt(TurnPrompt.named),
        TurnOffer.Root.Named
      )
      offeredAs(slack, heard, weighed = triaged(0.49)) ==>
        (prompt(TurnPrompt.unprompted), TurnOffer.Root.Heard)
    }

    test("a turn rooted on a heard message is recorded so, and told it was not addressed") {
      val (prompt, root) =
        offeredAs(slack, Payload.Heard("is it Thursday?"))
      root ==> TurnOffer.Root.Heard
      prompt ==> SystemPrompt
        .of(
          Vector(
            TurnPrompt.Base,
            TurnPrompt.Candour,
            TurnPrompt.Answering,
            TurnPrompt.edge(slack)
          ) ++
            TurnPrompt.destination(slack) ++ TurnPrompt.called(Persona.Grit, slack) ++
            Vector(TurnPrompt.unprompted, TurnPrompt.reach(None, ToolSet.Empty))
        )
        .render
    }

    test(
      "a conversation with no directory works in the service a link holding it names; else none"
    ) {
      val t1 = Place.under(grit.core.place.Namespace.Slack, Vector("T1"))
      val t2 = Place.under(grit.core.place.Namespace.Slack, Vector("T2"))
      linked(Vector(WorksIn(t1, github)), Vector.empty)._1.workspace ==> Some(github.place)
      linked(Vector(WorksIn(t2, github)), Vector.empty)._1.workspace ==> None
    }

    test("a TUI conversation works in its directory, though a link holds its place") {
      val dir = Directory.of("/work").fold(e => throw new java.lang.AssertionError(e), identity)
      TurnOffer.workspaceOf(
        Origin.Tui(dir, "default"),
        Vector(WorksIn(Place.Everywhere, github))
      ) ==>
        Some(Place.of(dir))
    }

    test(
      "the advertised tools the engine does not describe are offered after its hosted ones and recorded; one asking first, or named as an engine tool, is not"
    ) {
      val (recorded, offered) = linked(
        Vector(WorksIn(Place.Everywhere, github)),
        Vector(
          advert("github_search"),
          advert("fetch"),
          advert("github_merge", asks = true),
          advert("about"),
          advert("github_issue")
        )
      )
      offered ==> Vector("fetch", "github_search", "github_issue", "about")
      recorded.advertised ==> Vector(ToolName("github_search"), ToolName("github_issue"))
    }

    test(
      "a tool the offer took from an advert is rebuilt from the recorded set as a hosted call; one this build lacks is gone"
    ) {
      val set = ToolSet
        .of(Vector(advert("github_search"), advert("github_issue")))
        .fold(d => throw new java.lang.AssertionError(d.toString), identity)
      val offer = TurnOffer(
        Some(github.place),
        set,
        SystemPrompt.of(Vector.empty),
        TurnOffer.Root.Addressed,
        Vector(named("github_search")),
        Map.empty,
        Width.Deployed
      )
      val tooling = TurnTooling[{}](
        box(tool(ToolName("about"), asks = false)),
        Toolbox.Empty,
        Vector.empty,
        new FakeJot,
        budget(5)
      )
      val rebuilt = TurnOffer.toolbox(tooling, offer)
      def bound(name: String): String =
        rebuilt.bind(
          AssistantBlock.ToolCall(ToolCallId("c1"), name, ujson.Obj("q" -> "grit")),
          Repairs.All
        ) match {
          case Right(h: Bound.Hosted) => s"hosted ${h.shown}"
          case Right(f: Bound.Free) => s"free ${f()}"
          case other => other.toString
        }
      Vector("github_search", "github_issue").map(bound) ==> Vector(
        "hosted github_search {\"q\":\"grit\"}",
        "free Failed(The tool github_issue is gone; nothing ran.)"
      )
    }

    test(
      "an addressed turn is offered the tools advertised at a service it reaches, after its workspace's, each recorded at that place, and told so after its reach"
    ) {
      val (recorded, offered, prompt) =
        reaching(Payload.Message(Message.User("post it")), Vector(advert("post_x")))
      offered ==> Vector("github_search", "post_x", "about")
      recorded.advertised ==> Vector(named("github_search"))
      recorded.reached ==> Map(named("post_x") -> elsewhere.place)
      // Joined by hand, not through SystemPrompt.of, so the order within each layer is pinned.
      prompt ==> (Vector(
        TurnPrompt.Base,
        TurnPrompt.Candour,
        TurnPrompt.Answering,
        TurnPrompt.edge(slack)
      ) ++ TurnPrompt.destination(slack) ++ TurnPrompt.called(Persona.Grit, slack) ++ Vector(
        TurnPrompt.reach(Some(github.place), set(advert("github_search")))
      ) ++ TurnPrompt.reached(elsewhere, set(advert("post_x")))).map(_.text).mkString("\n\n")
    }

    test("a turn rooted on a heard message is offered none of a reached service's tools") {
      // Unprompted drafts run tools, and only their reply is gated: a reached tool offered
      // here would act although grit was configured not to speak unprompted.
      val (recorded, offered, prompt) =
        reaching(Payload.Heard("someone should post it"), Vector(advert("post_x")))
      offered ==> Vector("github_search", "about")
      recorded.reached ==> Map.empty
      assert(!prompt.contains("You also reach"))
    }

    test(
      "a reached tool named as one the turn already has is not offered again, and the rest are"
    ) {
      val (recorded, offered, _) = reaching(
        Payload.Message(Message.User("post it")),
        Vector(advert("github_search"), advert("about"), advert("post_x"))
      )
      offered ==> Vector("github_search", "post_x", "about")
      recorded.reached ==> Map(named("post_x") -> elsewhere.place)
    }

    test(
      "a heard turn whose workspace's every source reads below the recipe's threshold is offered none of its tools nor told of them; its shape keeps them and the verdict"
    ) {
      val (recorded, offered, prompt) = reaching(
        heardRoot,
        Vector(advert("post_x")),
        heardBySource,
        repoInGithub,
        repoReads(0.1)
      )
      offered ==> Vector("about")
      recorded.advertised ==> Vector.empty
      assert(!prompt.contains("github"))
      val shape = recorded.shaped.getOrElse(throw new java.lang.AssertionError("no shape"))
      val read = Reading.Yes(QuestionName.per(Tags.V2.sourcePrefix, repo))
      shape.width ==> narrow
      shape.services ==> Vector(
        TurnShape.Took(
          ServiceOffer(
            github,
            Vector(repo),
            ServiceOffer.Verdict.Checked(
              TriageGate.Checked.Fails(
                TriageGate.Failed(
                  TriageBound.AtLeast(read, Probability.clamped(0.2)),
                  Probability.clamped(0.1)
                ),
                Vector.empty
              )
            )
          ),
          TurnShape.Via.Workspace,
          Vector(named("github_search"))
        )
      )
      ToolSets.get(shape.whole)(using TestTx.fake).map(_.tools.map(t => ToolName.value(t.name))) ==>
        Right(Vector("github_search", "about"))
    }

    test(
      "a heard turn is offered its workspace's tools when a source passes, when the answers do not read it, and when it has no answers"
    ) {
      def verdict(weighed: Option[Tags]) = {
        val (recorded, offered, _) =
          reaching(heardRoot, Vector.empty, heardBySource, repoInGithub, weighed)
        (offered, recorded.shaped.map(_.services.map(_.offer.verdict)))
      }
      val read = Reading.Yes(QuestionName.per(Tags.V2.sourcePrefix, repo))
      val unrelated =
        Some(Tags.Weighed(VectorMap(Tags.V2.open -> Answer.YesNo(0.9)), "jev", Usage.Zero))
      Vector(repoReads(0.2), unrelated, None, Some(Tags.Unanswered("unavailable"))).map(verdict) ==>
        Vector(
          (
            Vector("github_search", "about"),
            Some(Vector(ServiceOffer.Verdict.Checked(TriageGate.Checked.Passes)))
          ),
          (
            Vector("github_search", "about"),
            Some(Vector(ServiceOffer.Verdict.Checked(TriageGate.Checked.Unread(read))))
          ),
          (Vector("github_search", "about"), Some(Vector(ServiceOffer.Verdict.Unweighed))),
          (Vector("github_search", "about"), Some(Vector(ServiceOffer.Verdict.Unweighed)))
        )
    }

    test(
      "under the shipped recipe a turn is offered exactly what it was before recipes, whatever its answers, and its shape records every service ungated at the deployed width"
    ) {
      val addressed = Payload.Message(Message.User("post it"))
      val before = reaching(addressed, Vector(advert("post_x")))
      val shipped =
        reaching(
          addressed,
          Vector(advert("post_x")),
          TurnRecipe.Shipped,
          repoInGithub,
          repoReads(0.0)
        )
      (shipped._1.copy(shaped = None), shipped._2, shipped._3) ==>
        (before._1.copy(shaped = None), before._2, before._3)
      val heardBefore = reaching(heardRoot, Vector.empty)
      val heardShipped =
        reaching(heardRoot, Vector.empty, TurnRecipe.Shipped, repoInGithub, repoReads(0.0))
      (heardShipped._1.copy(shaped = None), heardShipped._2, heardShipped._3) ==>
        (heardBefore._1.copy(shaped = None), heardBefore._2, heardBefore._3)
      shipped._1.shaped ==> Some(
        TurnShape(
          Width.Deployed,
          shipped._1.tools,
          Vector(
            TurnShape.Took(
              ServiceOffer(github, Vector(repo), ServiceOffer.Verdict.Ungated),
              TurnShape.Via.Workspace,
              Vector(named("github_search"))
            ),
            TurnShape.Took(
              ServiceOffer(elsewhere, Vector.empty, ServiceOffer.Verdict.Ungated),
              TurnShape.Via.Reached,
              Vector(named("post_x"))
            )
          )
        )
      )
    }

    test(
      "an addressed turn is offered a reached service's tools by the recipe's addressed offering"
    ) {
      // The workspace's source fails at 0.2; the reached service has no source, so is ungated.
      val bySource = TurnRecipe(
        ByFocus.both(TurnRecipe.Shipped.addressed),
        Shaping(Width.Deployed, Offering.BySource(Probability.clamped(0.2)))
      )
      val (recorded, offered, prompt) = reaching(
        Payload.Message(Message.User("post it")),
        Vector(advert("post_x")),
        bySource,
        repoInGithub,
        repoReads(0.1)
      )
      offered ==> Vector("post_x", "about")
      recorded.reached ==> Map(named("post_x") -> elsewhere.place)
      assert(!prompt.contains("works in github"))
      assert(prompt.contains("You also reach elsewhere"))
    }

    test("a turn rooted on a person's message to grit is recorded as addressed") {
      offeredAs(slack, Payload.Message(Message.User("hi")))._2 ==>
        TurnOffer.Root.Addressed
    }
  }
}
