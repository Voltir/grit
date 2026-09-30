package grit.turn

import grit.core.edge.Advert
import grit.core.id.TurnRef
import grit.core.place.Place
import grit.core.prompt.{FragmentId, SystemPrompt, Voice}
import grit.core.store.{Db, EntryStore, Origin, Payload, StoreError, Tx}
import grit.core.tool.{DuplicateName, Tool, ToolName, ToolSet, ToolSetId, Toolbox}

/** What a turn offers its model, as its `offer` step decided (ADR 0016, 0017): the workspace
  * its hosted calls are addressed to, none for a conversation with no directory; its tool
  * set; its system prompt; and its `root`, what it answers.
  */
final case class TurnOffer(
    workspace: Option[Place],
    tools: ToolSet,
    prompt: SystemPrompt,
    root: TurnOffer.Root
) {

  /** The system text every model call of the turn is sent. */
  def system: String = prompt.render
}

object TurnOffer {

  /** The `offer` step's output: references only, the texts kept by id; and the turn's
    * `root`, which decides the steps it takes after its reply.
    */
  final case class Recorded(
      workspace: Option[Place],
      tools: ToolSetId,
      prompt: Vector[FragmentId],
      root: Root
  )

  /** What a turn answers: a message said to grit, or one grit heard and chose to draft a
    * reply to ([[grit.core.speech.Speech.decide]]). An offer recorded before roots existed is
    * `Addressed`.
    */
  enum Root {
    case Addressed, Heard
  }

  /** What `turn` is offered now, kept, and recorded as `turn`'s prompt: its conversation's
    * workspace (a TUI session's directory); the hosted tools of `tooling` that the live edge
    * serving that workspace advertises, then `tooling`'s own, then its operator tools when
    * its origin is the operator's ([[grit.core.store.Origin.operator]]); and its prompt: the base, its
    * edge's fragment, what its workspace calls the assistant (when it has named it,
    * [[grit.core.store.Origin.assistant]]), [[TurnPrompt.unprompted]] when its root is heard,
    * the voice's fragment (none for plain), what it may reach there, and the
    * instruction files the edge read there. Its root is `Heard` when its first entry in
    * `entries` is a heard message ([[grit.core.store.Payload.Heard]]), else `Addressed`. `TurnFailure.Store` when a store fails, the
    * conversation is gone, or two tools offered share a name. A stored voice this build does not know is the default, never a
    * failure.
    */
  def decide[C^](
      hosting: TurnHosting,
      entries: EntryStore,
      tooling: TurnTooling[C]^,
      turn: TurnRef
  )(using
      Tx^
  ): Either[TurnFailure, Recorded] =
    (for {
      found <- hosting.conversations.get(turn.conversationId)
      conversation <- found.toRight(
        StoreError.Invalid(s"conversation of ${turn.workflowId} is gone")
      )
      workspace = workspaceOf(conversation.origin)
      advert <- workspace.fold[Either[StoreError, Option[Advert]]](Right(None))(
        hosting.edges.serving
      )
      served <- advert.fold[Either[StoreError, ToolSet]](Right(ToolSet.Empty))(a =>
        hosting.toolSets.get(a.tools)
      )
      names = served.tools.map(_.name).toSet
      hosted = tooling.hosted.filter(h => names.contains(h.name))
      offered <- Toolbox
        .of[{}](hosted*)
        .map(_.set)
        .flatMap(h =>
          (if (conversation.origin.operator) Toolbox.joined(tooling.tools, tooling.operator)
           else Right(tooling.tools))
            .flatMap(_.preceded(hosted))
            .map(all => (h, all.set))
        )
        .left
        .map { case DuplicateName(n) =>
          StoreError.Invalid(s"two tools are named ${ToolName.value(n)}")
        }
      (hostedSet, set) = offered
      _ <- hosting.toolSets.keep(set)
      place <- advert.fold[Either[StoreError, SystemPrompt]](Right(SystemPrompt.of(Vector.empty)))(
        a => hosting.prompts.prompt(a.instructions)
      )
      voice <- hosting.voices.current()
      called <- conversation.origin.assistant.fold[Either[StoreError, Option[String]]](
        Right(None)
      )(hosting.principals.name)
      all <- entries.list(turn.conversationId)
      root = all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq).map(_.payload) match {
        case Some(Payload.Heard(_)) => Root.Heard
        case _ => Root.Addressed
      }
      prompt = SystemPrompt.of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.edge(conversation.origin)
        ) ++ called.map(TurnPrompt.called) ++
          Option.when(root == Root.Heard)(TurnPrompt.unprompted) ++ Vector(
            TurnPrompt.reach(workspace.flatMap(_.directory), hostedSet)
          ) ++ Voice.fragment(voice) ++ place.fragments
      )
      _ <- hosting.prompts.record(turn.workflowId, prompt)
    } yield Recorded(workspace, set.id, prompt.ids, root)).left.map(e =>
      TurnFailure.Store(describe(e))
    )

  /** The offer `recorded` read back through `db`: its tool set and prompt, by id.
    * `TurnFailure.Store` naming what is not kept.
    */
  def load(hosting: TurnHosting, db: Db^, recorded: Recorded): Either[TurnFailure, TurnOffer] =
    db.read { (tx: Tx^) ?=>
      for {
        set <- hosting.toolSets.get(recorded.tools)
        prompt <- hosting.prompts.prompt(recorded.prompt)
      } yield TurnOffer(recorded.workspace, set, prompt, recorded.root)
    }.left
      .map(e => TurnFailure.Store(describe(e)))

  /** The tools `offer`'s set names, in its order: each `tooling`'s own tool or operator
    * tool, else its hosted one, else a stand-in for a tool this build no longer has ([[Tool.gone]]).
    */
  def toolbox[C^](tooling: TurnTooling[C]^, offer: TurnOffer): Toolbox[C] = {
    val chosen: Vector[Tool.Offered^{C}] = offer.tools.tools.map { entry =>
      tooling.tools.tool(entry.name).orElse(tooling.operator.tool(entry.name)) match {
        case Some(own) => own
        case None =>
          tooling.hosted.find(_.name == entry.name) match {
            case Some(hosted) => hosted
            case None => Tool.gone(entry)
          }
      }
    }
    // The set's names are distinct (ToolSet.of), and so are these.
    Toolbox.of[C](chosen*).getOrElse(tooling.tools)
  }

  /** Where a conversation from `origin` works: a TUI session's directory; none for others,
    * until a conversation can be linked to a workspace.
    */
  def workspaceOf(origin: Origin): Option[Place] = origin match {
    case Origin.Tui(dir, _) => Some(Place.of(dir))
    case _ => None
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${grit.core.id.EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
