package grit.kit.run

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.act.phase.Placing
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.classify.ClearedClassifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.durable.Durable
import grit.core.id.{ShadowName, WorkflowId}
import grit.core.inbox.Inbox
import grit.core.provider.Models
import grit.core.stitch.{Placements, StitchReads, Tuning}
import grit.core.store.{Db, Jot}
import grit.dbos.engine.Stores
import grit.job.run.{Run, RunEnv, RunRecords}
import grit.kit.deployment.{Assembly, Deployment}
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.lifecycle.shadow.{Shadow, ShadowAsking, ShadowEnv}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Mentions, Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.turn.{
  TurnEnv,
  TurnHosting,
  TurnRecords,
  TurnRouting,
  TurnSpeech,
  TurnStitching,
  TurnWeighing
}

/** The envs and workflow bodies of one launch of `d`, over `stores` and the capabilities of one
  * engine: `db` and `jot` its openers, `inbox` its inbox, `placements` its placements, `clock`
  * the time. Each body is named as [[grit.dbos.engine.Engine.launch]]'s parameter it is passed
  * to; the turn's is `Turn.body` over [[turnEnv]] and the launch's tooling. Every method builds
  * afresh from its arguments and keeps nothing, so a launch and a rehearsal build them alike.
  */
final class Wiring(
    d: Deployment,
    stores: (Stores & caps.SharedCapability)^,
    db: Db^,
    jot: Jot^,
    inbox: Inbox^,
    placements: Placements^,
    clock: Clock^
) extends caps.SharedCapability {

  /** The context assembler `d` names, reading `stores`. */
  def assembler: ContextAssembler = d.assembly match {
    case Assembly.Retrieval(budget, tail) =>
      new RetrievalAssembler(
        stores.entries,
        stores.conversations,
        stores.periods,
        stores.principals,
        stores.lifecycle,
        stores.search,
        stores.documents,
        stores.stitches,
        CharEstimate,
        budget,
        tail,
        Tuning.Default
      )
    case Assembly.Linear(budget) =>
      new LinearAssembler(stores.entries, stores.periods, stores.principals, CharEstimate, budget)
  }

  /** The turn's env: its window drawn by `assembler`, its calls made through `models` and
    * `classifier` (its moves' too, their costs kept by `moveRecords`), and a said root weighed,
    * and its task questions asked, through `weighing`. Its fresh ids are random.
    */
  def turnEnv(
      moveRecords: MoveRecords,
      assembler: ContextAssembler,
      models: Models^,
      classifier: ClearedClassifier^,
      weighing: ClearedClassifier^
  ): TurnEnv^ = {
    // One Mentions both weighs a said root before its offer and asks its task questions in route.
    val mentions =
      new Mentions(
        reads,
        stores.rooms,
        d.knowledge,
        d.triage,
        weighing,
        placements,
        db,
        clock,
        CharEstimate,
        Tuning.Default
      )
    TurnEnv(
      TurnRecords(
        stores.entries,
        stores.ledger,
        CharEstimate,
        stores.profiles,
        stores.principals,
        stores.documents
      ),
      TurnHosting(
        stores.conversations,
        stores.prompts,
        stores.toolSets,
        stores.requests,
        stores.edgeDirectory,
        stores.voices,
        stores.askers
      ),
      moveRecords,
      assembler,
      classifier,
      models,
      db,
      clock,
      Fresh.random(),
      TurnSpeech(d.speaking, stores.speech, stores.deliveries),
      TurnStitching(stores.stitches, stores.search, stores.lifecycle, Tuning.Default, placing),
      TurnWeighing(stores.triage, mentions),
      TurnRouting(mentions, d.budget, stores.spending)
    )
  }

  /** The body of every attempt to close a period, its asks made by `moves`. */
  def close(moves: MovesEnv^): WorkflowId ->{this, moves} Durable^ ?->{this, moves} String =
    Close.body(
      CloseEnv(
        CloseRecords(
          stores.entries,
          stores.periods,
          stores.lifecycle,
          stores.tombstones,
          stores.principals,
          stores.triage,
          stores.conversations
        ),
        moves,
        d.budget,
        db,
        clock
      )
    )

  /** The body of every question whether anyone waits on a quiet period, its asks by `moves`. */
  def settle(moves: MovesEnv^): WorkflowId ->{this, moves} Durable^ ?->{this, moves} String =
    Settle.body(
      SettleEnv(
        SettleRecords(stores.entries, stores.periods, stores.lifecycle, stores.principals),
        db,
        clock,
        moves,
        d.budget
      )
    )

  /** The body of every posting run, to `d`'s plugins. */
  def post: WorkflowId ->{this} Durable^ ?->{this} String =
    Posting.body(
      d.plugins,
      PostEnv(
        stores.periods,
        stores.cursors,
        stores.cache,
        stores.keeper,
        stores.tombstones,
        jot,
        clock
      )
    )

  /** The body of every heard message's triage, its asks made by `moves`; a turn it starts goes
    * to `inbox`.
    */
  def triage(moves: MovesEnv^): WorkflowId ->{this, moves} Durable^ ?->{this, moves} String =
    Triage.body(
      TriageEnv(
        TriageRecords(
          stores.entries,
          stores.triage,
          stores.principals,
          stores.conversations,
          stores.speech,
          stores.spending,
          stores.acknowledgements,
          stores.deliveries,
          stores.stitches,
          stores.search,
          stores.lifecycle,
          stores.rooms
        ),
        moves,
        d.budget,
        db,
        clock,
        TriageSpeech(
          d.speaking,
          d.budget,
          turn => inbox.startTurn(turn).left.map(_.toString)
        ),
        Tuning.Default,
        placing,
        d.knowledge,
        d.triage
      )
    )

  /** The body of every opening's placement, its asks made by `moves`. */
  def stitch(moves: MovesEnv^): WorkflowId ->{this, moves} Durable^ ?->{this, moves} String =
    Stitch.body(StitchEnv(reads, moves, d.budget, db, clock, Tuning.Default))

  /** The body of every shadow, its asks made by `moves` and each variant's question by its
    * entry in `variants`.
    */
  def shadow[V^](
      moves: MovesEnv^,
      variants: Map[ShadowName, ShadowAsking^{V}]
  ): WorkflowId ->{this, moves, V} Durable^ ?->{this, moves, V} String =
    Shadow.body(
      ShadowEnv(
        reads,
        stores.rooms,
        stores.shadows,
        variants,
        d.knowledge,
        db,
        clock,
        Tuning.Default,
        moves,
        d.budget
      )
    )

  /** The body of every run of `d`'s jobs, its asks made by `moves`. */
  def run(moves: MovesEnv^): WorkflowId ->{this, moves} Durable^ ?->{this, moves} String =
    Run.body(
      RunEnv(
        RunRecords(stores.entries, stores.conversations, stores.schedules, stores.deliveries),
        db,
        jot,
        clock,
        moves,
        d.budget,
        stores.keeper
      ),
      d.allJobs
    )

  /** What stitching reads, the placement's and a mention's. */
  private def reads: StitchReads =
    StitchReads(
      stores.entries,
      stores.conversations,
      stores.lifecycle,
      stores.stitches,
      stores.search,
      stores.principals
    )

  /** The one wait for an opening's placement, the turn's and triage's. */
  private def placing: Placing.Env^{this} =
    Placing.Env(stores.entries, stores.conversations, db, placements)
}
