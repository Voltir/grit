package grit.turn

import grit.core.context.Width
import grit.core.edge.Advert
import grit.core.id.TurnRef
import grit.core.place.{Place, Reaches, Service, WorksIn}
import grit.core.prompt.{FragmentId, SystemPrompt, Voice}
import grit.core.recipe.{Offering, Rooted, ServiceOffer}
import grit.core.store.{EntryStore, Origin, Payload, Position, Reads, StoreError, Tx}
import grit.core.tool.{DuplicateName, Hosted, Tool, ToolName, ToolSet, ToolSetId, Toolbox}
import grit.core.triage.{Corpora, Tags}

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

  /** What a turn answers: a message said to grit (`Addressed`); a heard one triage decided to
    * answer as said to grit, having read it as put to grit by name
    * ([[grit.core.speech.Decision.Answering]]): offered, told, shaped, written and summarised
    * as an `Addressed` one, its reply posted by its edge and its speech row kept
    * ([[grit.core.speech.Outcome.Replied]]) (`ByName`); or a heard one grit chose to draft a
    * reply to (`Heard`, [[grit.core.speech.Decision.Drafting]]). `Named` is a heard message
    * put to grit by name as an offer recorded it before such messages were answered as said
    * to grit: drafted, unjudged, and posted through speech
    * ([[grit.core.speech.Speech.postNamed]]); no offer records it now. An offer recorded
    * before roots existed is `Addressed`.
    */
  enum Root {
    case Addressed, Heard, Named, ByName
  }

  /** What `turn` is offered now, kept, and recorded as `turn`'s prompt: its conversation's
    * workspace ([[workspaceOf]]); the hosted tools of `tooling` that the live edge serving
    * that workspace advertises, then the others it advertises that
    * [[grit.core.tool.Hosted.advertised]] offers, less any whose name one of `tooling`'s has,
    * then, on an addressed turn, the tools advertised at each service `tooling.reaches` links
    * its place to ([[grit.core.place.Reaches.of]]) that [[grit.core.tool.Hosted.advertised]]
    * offers, less any whose name an earlier tool has (a heard-rooted turn is offered none of
    * them), then `tooling`'s own, then its operator tools when
    * its origin is the operator's ([[grit.core.store.Audience.operator]]). A service place, its
    * workspace or one it reaches, offers nothing when the turn does not read from it
    * ([[grit.core.store.Tx.readsFrom]]): a reached one is not named, and of a workspace an edge
    * serves the prompt says only that its tools are not available ([[TurnPrompt.Serving]]); and its tools that declare
    * no destination only when the turn also sends to it ([[grit.core.store.Tx.sendsTo]]):
    * their arguments go to it. An advertised tool that writes
    * ([[grit.core.tool.ToolSet.Entry.writes]]) is offered only the places the turn writes to,
    * and not at all when none is; one the engine describes, only when it writes to all of its
    * places. Its prompt: the base,
    * [[TurnPrompt.Candour]], [[TurnPrompt.Answering]], its edge's
    * fragment, where its reply goes ([[TurnPrompt.destination]]), what it is called
    * (`tooling.persona`, [[TurnPrompt.called]]), [[TurnPrompt.unprompted]] when its root is
    * heard, its own room's label (the transaction's floor, [[TurnPrompt.room]]), what it may
    * reach there, what it reaches besides ([[TurnPrompt.reached]], for each
    * reached service offering tools), the voice's fragment (none for plain), and the
    * instruction files the edge read there. Its root is `ByName` when its first entry in
    * `entries` is a heard message ([[grit.core.store.Payload.Heard]]) and `answering` (triage
    * decided to answer it as said to grit, [[grit.core.speech.Decision.Answering]]), `Heard`
    * for any other heard message, else `Addressed`; a `ByName` turn is offered, told and
    * shaped as an `Addressed` one.
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
      weighed: Option[Tags],
      answering: Boolean
  )(using
      tx: Tx^
  ): Either[TurnFailure, Recorded] =
    (for {
      found <- hosting.conversations.get(turn.conversationId)
      conversation <- found.toRight(
        StoreError.Invalid(s"conversation of ${turn.workflowId} is gone")
      )
      all <- entries.list(turn.conversationId)
      first = all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq)
      // The root, how a recipe tells it apart, and what it is told of it.
      (root, rooted, unasked) = first.map(_.payload) match {
        case Some(Payload.Heard(_)) if answering => (Root.ByName, Rooted.Addressed, None)
        case Some(Payload.Heard(_)) =>
          val opening = all.minByOption(_.seq).map(_.id) == first.map(_.id)
          (
            Root.Heard,
            Rooted.Heard(
              conversation.origin.focus(if (opening) Position.Opening else Position.Reply)
            ),
            Some(TurnPrompt.unprompted)
          )
        case _ => (Root.Addressed, Rooted.Addressed, None)
      }
      shaping = tooling.recipe.at(rooted)
      workspace = workspaceOf(conversation.origin, tooling.worksIn)
      servedAt <- workspace.fold[Either[StoreError, Option[Advert]]](Right(None))(
        hosting.edges.serving
      )
      // Nothing of a workspace the turn does not read from is taken, as if no edge served it.
      advert = servedAt.filter(_ => workspace.exists(Tx.readsFrom(_)))
      served <- advert.fold[Either[StoreError, ToolSet]](Right(ToolSet.Empty))(a =>
        hosting.toolSets.get(a.tools)
      )
      // A directory is this machine, not outside grit: only a service is sent to.
      sends = workspace.flatMap(_.service).forall(Tx.sendsTo(_))
      names = served.tools.map(_.name).toSet
      // An engine-described tool is offered only as it is described, never narrowed.
      described = tooling.hosted.filter { h =>
        val whole = h.entry
        names.contains(h.name) &&
        offerable(whole, sends).map(_.writes.map(_.to)) == Some(whole.writes.map(_.to))
      }
      engines = (tooling.tools.names ++ tooling.operator.names ++ tooling.hosted.map(_.name)).toSet
      advertised = served.tools
        .filterNot(e => engines.contains(e.name))
        .flatMap(offerable(_, sends))
        .flatMap(Hosted.advertised)
      hosted: Vector[Tool.Offered] = described ++ advertised
      // A heard-rooted turn is offered none: its draft runs tools, and only its reply is gated.
      services = rooted match {
        case Rooted.Addressed => Reaches.of(tooling.reaches, conversation.origin.place)
        case Rooted.Heard(_) => Vector.empty
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
      prompt = SystemPrompt.of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.Candour,
          TurnPrompt.Answering,
          TurnPrompt.edge(conversation.origin)
        ) ++ TurnPrompt.destination(conversation.origin) ++
          TurnPrompt.called(tooling.persona, conversation.origin) ++
          unasked ++
          Vector(TurnPrompt.room(conversation.origin, Tx.floor(tx))) ++
          Option.when(kept)(
            TurnPrompt.reach(
              workspace,
              servedAt.fold(TurnPrompt.Serving.Unserved)(_ =>
                TurnPrompt.Serving.Offering(hostedSet)
              )
            )
          ) ++
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
      knowledge: Corpora,
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
  def load(hosting: TurnHosting, db: Reads^, recorded: Recorded): Either[TurnFailure, TurnOffer] =
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

  /** Whether an addressed turn from `origin` would read its root's answers: some service its
    * conversation links (its workspace's, [[workspaceOf]], or one it reaches,
    * [[grit.core.place.Reaches.of]]), whether or not an edge serves it now, is offered by
    * `tooling.recipe`'s addressed offering through a gate on a source `tooling.knowledge` has
    * covering its place. Under [[grit.core.recipe.Offering.All]], never.
    */
  def weighs[C^](tooling: TurnTooling[C]^, origin: Origin): Boolean = {
    val place = origin.place
    val linked = workspaceOf(origin, tooling.worksIn).flatMap(_.service).toVector ++
      Reaches.of(tooling.reaches, place)
    val covering = tooling.knowledge.at(place).supplied
    val offering = tooling.recipe.at(Rooted.Addressed).offering
    linked.exists(s => offering.gate(covering.getOrElse(s, Vector.empty)).nonEmpty)
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
          // A service the turn does not read from is as if no edge served it.
          advert <-
            if (Tx.readsFrom(service.place)) hosting.edges.serving(service.place)
            else Right(None)
          set <- advert.fold[Either[StoreError, ToolSet]](Right(ToolSet.Empty))(a =>
            hosting.toolSets.get(a.tools)
          )
          sends = Tx.sendsTo(service)
          entries = set.tools
            .filter(e => !done._2.contains(e.name))
            .flatMap(offerable(_, sends))
            .filter(Hosted.advertised(_).nonEmpty)
          // A ToolSet's entries have distinct names, so a subset's do too.
          offered = ToolSet.of(entries).getOrElse(ToolSet.Empty)
        } yield (done._1 :+ (service, offered), done._2 ++ entries.map(_.name))
      }
      .map(_._1)

  /** `entry` as a turn offers it at a service it `sends` to or not: one that declares no
    * destination only when it does, its arguments going to that service; one that writes, less
    * the places the turn does not write to ([[grit.core.store.Tx.writesTo]]); `None` when
    * neither is left.
    */
  private def offerable(entry: ToolSet.Entry, sends: Boolean)(using Tx^): Option[ToolSet.Entry] =
    entry.writes match {
      case None => Option.when(sends)(entry)
      case Some(writes) => writes.narrowed(Tx.writesTo(_)).map(n => entry.copy(writes = Some(n)))
    }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${grit.core.id.EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
