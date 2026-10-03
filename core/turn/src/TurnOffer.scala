package grit.turn

import grit.core.context.Width
import grit.core.edge.Advert
import grit.core.id.TurnRef
import grit.core.place.{Place, Reaches, Service, WorksIn}
import grit.core.prompt.{FragmentId, SystemPrompt, Voice}
import grit.core.recipe.{Offering, Rooted, ServiceOffer}
import grit.core.store.{Db, EntryStore, Origin, Payload, Position, StoreError, Tx}
import grit.core.tool.{DuplicateName, Hosted, Tool, ToolName, ToolSet, ToolSetId, Toolbox}
import grit.core.triage.{KnowledgeSources, Tags}

/** What a turn offers its model, as its `offer` step decided (ADR 0016, 0017): the workspace
  * its hosted calls are addressed to, none when its conversation has none
  * ([[TurnOffer.workspaceOf]]); its tool set; its system prompt; its `root`, what it answers;
  * `advertised`, the tools of its set taken from its workspace's edge's advert
  * ([[TurnOffer.Recorded]]); `reached`, the tools of the set taken from a reached
  * service's advert, each with the place its calls are addressed to ([[placeOf]]); and the
  * `width` its window is drawn at.
  */
final case class TurnOffer(
    workspace: Option[Place],
    tools: ToolSet,
    prompt: SystemPrompt,
    root: TurnOffer.Root,
    advertised: Vector[ToolName],
    reached: Map[ToolName, Place],
    width: Width
) {

  /** The system text every model call of the turn is sent. */
  def system: String = prompt.render

  /** Where a hosted call of `name` is addressed: the reached place its tool came from, else
    * the workspace; `None` when the conversation has neither.
    */
  def placeOf(name: ToolName): Option[Place] = reached.get(name).orElse(workspace)
}

object TurnOffer {

  /** The `offer` step's output: references only, the texts kept by id; the turn's `root`,
    * which decides the steps it takes after its reply; `advertised`, the offered tools taken
    * from the workspace's serving edge's advert ([[grit.core.tool.Hosted.advertised]]); and
    * `reached`, those taken from a reached service's, each with its place. [[toolbox]]
    * rebuilds both from the recorded set. `shaped`, what its recipe made of it; `None` for an
    * offer recorded before recipes, which was drawn at the deployed width with nothing
    * withheld.
    */
  final case class Recorded(
      workspace: Option[Place],
      tools: ToolSetId,
      prompt: Vector[FragmentId],
      root: Root,
      advertised: Vector[ToolName] = Vector.empty,
      reached: Map[ToolName, Place] = Map.empty,
      shaped: Option[TurnShape] = None
  )

  /** What a turn answers: a message said to grit, or one grit heard and chose to draft a
    * reply to ([[grit.core.speech.Speech.decide]]). An offer recorded before roots existed is
    * `Addressed`.
    */
  enum Root {
    case Addressed, Heard
  }

  /** What `turn` is offered now, kept, and recorded as `turn`'s prompt: its conversation's
    * workspace ([[workspaceOf]]); the hosted tools of `tooling` that the live edge serving
    * that workspace advertises, then the others it advertises that
    * [[grit.core.tool.Hosted.advertised]] offers, less any whose name one of `tooling`'s has,
    * then, on an addressed turn, the tools advertised at each service `tooling.reaches` links
    * its place to ([[grit.core.place.Reaches.of]]) that [[grit.core.tool.Hosted.advertised]]
    * offers, less any whose name an earlier tool has (a heard-rooted turn is offered none of
    * them), then `tooling`'s own, then its operator tools when
    * its origin is the operator's ([[grit.core.store.Audience.operator]]); and its prompt: the base,
    * [[TurnPrompt.Candour]], [[TurnPrompt.Answering]], its edge's fragment, where its reply goes
    * ([[TurnPrompt.destination]]), what its workspace calls the assistant (when it has named it,
    * [[grit.core.store.Origin.assistant]]), [[TurnPrompt.unprompted]] when its root is heard,
    * what it may reach there, what it reaches besides ([[TurnPrompt.reached]], for each
    * reached service offering tools), the voice's fragment (none for plain), and the
    * instruction files the edge read there. Its root is `Heard` when its first entry in
    * `entries` is a heard message ([[grit.core.store.Payload.Heard]]), else `Addressed`.
    *
    * `tooling.recipe` shapes it by its root ([[grit.core.recipe.Rooted]]: a heard message at
    * the focus it was said at, [[grit.core.store.Origin.focus]]): the shaping's width is
    * recorded for its window, and its offering decides each service it takes tools from (its
    * workspace's, then those it reaches) by the sources `tooling.knowledge` says supply it and
    * `weighed`'s answers (none when `weighed` is `None` or `Unanswered`,
    * [[grit.core.recipe.Offering.decide]]). A withheld service's tools are left out of the set,
    * `advertised` and `reached`, and what it may reach there or besides is not said; all of it
    * is in `shaped`. `TurnFailure.Store` when a store fails, the conversation is gone, or two
    * tools offered share a name. A stored voice this build does not know is the default,
    * never a failure.
    */
  def decide[C^](
      hosting: TurnHosting,
      entries: EntryStore,
      tooling: TurnTooling[C]^,
      turn: TurnRef,
      weighed: Option[Tags]
  )(using
      Tx^
  ): Either[TurnFailure, Recorded] =
    (for {
      found <- hosting.conversations.get(turn.conversationId)
      conversation <- found.toRight(
        StoreError.Invalid(s"conversation of ${turn.workflowId} is gone")
      )
      all <- entries.list(turn.conversationId)
      first = all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq)
      root = first.map(_.payload) match {
        case Some(Payload.Heard(_)) => Root.Heard
        case _ => Root.Addressed
      }
      rooted = root match {
        case Root.Heard =>
          val opening = all.minByOption(_.seq).map(_.id) == first.map(_.id)
          Rooted.Heard(
            conversation.origin.focus(if (opening) Position.Opening else Position.Reply)
          )
        case Root.Addressed => Rooted.Addressed
      }
      shaping = tooling.recipe.at(rooted)
      workspace = workspaceOf(conversation.origin, tooling.worksIn)
      advert <- workspace.fold[Either[StoreError, Option[Advert]]](Right(None))(
        hosting.edges.serving
      )
      served <- advert.fold[Either[StoreError, ToolSet]](Right(ToolSet.Empty))(a =>
        hosting.toolSets.get(a.tools)
      )
      names = served.tools.map(_.name).toSet
      described = tooling.hosted.filter(h => names.contains(h.name))
      engines = (tooling.tools.names ++ tooling.operator.names ++ tooling.hosted.map(_.name)).toSet
      advertised = served.tools.filterNot(e => engines.contains(e.name)).flatMap(Hosted.advertised)
      hosted: Vector[Tool.Offered] = described ++ advertised
      // A heard-rooted turn is offered none: its draft runs tools, and only its reply is gated.
      services = root match {
        case Root.Addressed => Reaches.of(tooling.reaches, conversation.origin.place)
        case Root.Heard => Vector.empty
      }
      reaching <- reachedFrom(hosting, services, engines ++ hosted.map(_.name))
      took = shape(shaping.offering, tooling.knowledge, weighed, workspace, hosted, reaching)
      (atWorkspace, reachedTook) = took.partition(_.via == TurnShape.Via.Workspace)
      kept = !atWorkspace.exists(_.offer.withheld)
      offeredHosted = if (kept) hosted else Vector.empty
      // shape takes the reached in reaching's order, one each.
      offeredReaching = reaching.zip(reachedTook).collect {
        case (r, t) if !t.offer.withheld => r
      }
      whole <- setOf(tooling, conversation.origin, hosted, reaching).map(_._2)
      offered <- setOf(tooling, conversation.origin, offeredHosted, offeredReaching)
      (hostedSet, set) = offered
      _ <- hosting.toolSets.keep(set)
      _ <- if (whole.id == set.id) Right(()) else hosting.toolSets.keep(whole)
      place <- advert.fold[Either[StoreError, SystemPrompt]](Right(SystemPrompt.of(Vector.empty)))(
        a => hosting.prompts.prompt(a.instructions)
      )
      voice <- hosting.voices.current()
      called <- conversation.origin.assistant.fold[Either[StoreError, Option[String]]](
        Right(None)
      )(hosting.principals.name)
      prompt = SystemPrompt.of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.Candour,
          TurnPrompt.Answering,
          TurnPrompt.edge(conversation.origin)
        ) ++ TurnPrompt.destination(conversation.origin) ++ called.map(TurnPrompt.called) ++
          Option.when(root == Root.Heard)(TurnPrompt.unprompted) ++
          Option.when(kept)(TurnPrompt.reach(workspace, hostedSet)) ++
          offeredReaching.flatMap((service, set) => TurnPrompt.reached(service, set)) ++
          Voice.fragment(voice) ++ place.fragments
      )
      _ <- hosting.prompts.record(turn.workflowId, prompt)
    } yield Recorded(
      workspace,
      set.id,
      prompt.ids,
      root,
      if (kept) advertised.map(_.name) else Vector.empty,
      offeredReaching.flatMap((service, set) => set.tools.map(_.name -> service.place)).toMap,
      Some(TurnShape(shaping.width, whole.id, took))
    )).left.map(e => TurnFailure.Store(describe(e)))

  /** Each service a turn takes tools from, in order, as `offering` decides it by the sources
    * `knowledge` says supply it and `weighed`'s answers: `workspace`'s, when it is a service,
    * with `hosted`, then each of `reaching` with its set.
    */
  private def shape(
      offering: Offering,
      knowledge: KnowledgeSources,
      weighed: Option[Tags],
      workspace: Option[Place],
      hosted: Vector[Tool.Offered],
      reaching: Vector[(Service, ToolSet)]
  ): Vector[TurnShape.Took] = {
    val names: Vector[ToolName] = hosted.map(_.name)
    val from: Vector[(Service, TurnShape.Via, Vector[ToolName])] =
      workspace.flatMap(_.service).toVector.map((_, TurnShape.Via.Workspace, names)) ++
        reaching.map((service, set) => (service, TurnShape.Via.Reached, set.tools.map(_.name)))
    val answers = weighed.collect { case Tags.Weighed(answers, _, _) => answers }
    // decide gives one verdict per service, in order.
    Offering
      .decide(offering, knowledge.supplied, from.map(_._1), answers)
      .zip(from)
      .map((offer: ServiceOffer, f: (Service, TurnShape.Via, Vector[ToolName])) =>
        TurnShape.Took(offer, f._2, f._3)
      )
  }

  /** The hosted tools of a turn from `origin` as a set, and its whole set: `hosted`, then the
    * tools of `reaching` that [[Hosted.advertised]] offers, then `tooling`'s own, then its
    * operator tools for the operator. Why not, naming a name two of them share.
    */
  private def setOf[C^](
      tooling: TurnTooling[C]^,
      origin: Origin,
      hosted: Vector[Tool.Offered],
      reaching: Vector[(Service, ToolSet)]
  ): Either[StoreError, (ToolSet, ToolSet)] =
    Toolbox
      .of[{}](hosted*)
      .map(_.set)
      .flatMap(h =>
        (if (origin.audience.operator) Toolbox.joined(tooling.tools, tooling.operator)
         else Right(tooling.tools))
          .flatMap(_.preceded(hosted ++ reaching.flatMap(_._2.tools).flatMap(Hosted.advertised)))
          .map(all => (h, all.set))
      )
      .left
      .map { case DuplicateName(n) =>
        StoreError.Invalid(s"two tools are named ${ToolName.value(n)}")
      }

  /** The offer `recorded` read back through `db`: its tool set and prompt, by id.
    * `TurnFailure.Store` naming what is not kept.
    */
  def load(hosting: TurnHosting, db: Db^, recorded: Recorded): Either[TurnFailure, TurnOffer] =
    db.read { (tx: Tx^) ?=>
      for {
        set <- hosting.toolSets.get(recorded.tools)
        prompt <- hosting.prompts.prompt(recorded.prompt)
      } yield TurnOffer(
        recorded.workspace,
        set,
        prompt,
        recorded.root,
        recorded.advertised,
        recorded.reached,
        recorded.shaped.fold(Width.Deployed)(_.width)
      )
    }.left
      .map(e => TurnFailure.Store(describe(e)))

  /** The tools `offer`'s set names, in its order: each `tooling`'s own tool or operator
    * tool, else its hosted one, else, for a name `offer` took from an advert, its
    * workspace's or a reached service's, that tool as
    * [[grit.core.tool.Hosted.advertised]] makes it from the set's entry, else a stand-in for
    * a tool this build no longer has ([[Tool.gone]]).
    */
  def toolbox[C^](tooling: TurnTooling[C]^, offer: TurnOffer): Toolbox[C] = {
    val chosen: Vector[Tool.Offered^{C}] = offer.tools.tools.map { entry =>
      tooling.tools.tool(entry.name).orElse(tooling.operator.tool(entry.name)) match {
        case Some(own) => own
        case None =>
          tooling.hosted.find(_.name == entry.name) match {
            case Some(hosted) => hosted
            case None =>
              Option
                .when(offer.advertised.contains(entry.name) || offer.reached.contains(entry.name))(
                  entry
                )
                .flatMap(Hosted.advertised)
                .getOrElse(Tool.gone(entry))
          }
      }
    }
    // The set's names are distinct (ToolSet.of), and so are these.
    Toolbox.of[C](chosen*).getOrElse(tooling.tools)
  }

  /** Where a conversation from `origin` works: a TUI session's directory; else the service the
    * first of `links` holding its place names ([[WorksIn.of]]); else none.
    */
  def workspaceOf(origin: Origin, links: Vector[WorksIn]): Option[Place] = origin match {
    case Origin.Tui(dir, _) => Some(Place.of(dir))
    case other => WorksIn.of(links, other.place).map(_.place)
  }

  /** Each of `services` with the entries its live edge advertises that
    * [[Hosted.advertised]] offers and whose name neither `taken` nor an earlier service's
    * has. A service no live edge serves has none.
    */
  private def reachedFrom(
      hosting: TurnHosting,
      services: Vector[Service],
      taken: Set[ToolName]
  )(using Tx^): Either[StoreError, Vector[(Service, ToolSet)]] =
    services
      .foldLeft[Either[StoreError, (Vector[(Service, ToolSet)], Set[ToolName])]](
        Right((Vector.empty, taken))
      ) { (acc, service) =>
        for {
          done <- acc
          advert <- hosting.edges.serving(service.place)
          set <- advert.fold[Either[StoreError, ToolSet]](Right(ToolSet.Empty))(a =>
            hosting.toolSets.get(a.tools)
          )
          entries = set.tools
            .filter(e => !done._2.contains(e.name) && Hosted.advertised(e).nonEmpty)
          // A ToolSet's entries have distinct names, so a subset's do too.
          offered = ToolSet.of(entries).getOrElse(ToolSet.Empty)
        } yield (done._1 :+ (service, offered), done._2 ++ entries.map(_.name))
      }
      .map(_._1)

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${grit.core.id.EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
