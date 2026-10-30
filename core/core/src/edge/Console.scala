package grit.core.edge

import grit.core.document.DocumentSearch
import grit.core.files.PieceSearch
import grit.core.id.{ConversationId, TurnRef}
import grit.core.identity.Account
import grit.core.inbox.Inbox
import grit.core.spend.{Budget, Spending}
import grit.core.store.{
  Db,
  EntryStore,
  Jot,
  LifecycleStore,
  ModelProfileStore,
  Origin,
  Principals,
  PromptStore,
  StoreError,
  UsageLedger,
  VoiceStore
}

/** What a person's screen onto a database's engine reads and writes (ADR 0015): the store,
  * its inbox, short transactions, and each turn's status, recorded steps and reply stream,
  * whether this process runs the engine or attached to one another process runs. Not a
  * terminal: a screen is drawn over it, or none is. Everything crosses Postgres (ADR 0002),
  * so it works while no engine runs; turns then wait. Closing it closes the engine this
  * process runs.
  */
trait Console extends caps.SharedCapability, AutoCloseable {

  val entries: EntryStore

  val ledger: UsageLedger

  /** What the ledger's calls cost, read back. */
  val spending: Spending

  /** How its inbox meters new messages: the cap, and where days begin. */
  val budget: Budget

  /** Which profile each turn's model calls were made under. */
  val profiles: ModelProfileStore

  /** The system prompt each turn was sent. */
  val prompts: PromptStore

  /** The lifecycle's settings in force. */
  val lifecycle: LifecycleStore

  /** The voice grit talks to the person in. */
  val voices: VoiceStore

  /** The accounts an edge named, and whose names a window shows. */
  val principals: Principals

  /** The plugins' documents, as windows hold them. */
  val documents: DocumentSearch

  /** The plugins' pieces of files, as windows hold them. */
  val pieces: PieceSearch

  /** Short read transactions. */
  val db: Db

  /** Short write transactions. */
  val jot: Jot

  val inbox: Inbox

  /** The conversation `origin` names, created by `by` if it is new, at its room's label, as an
    * edge's first ingest would.
    */
  def conversation(origin: Origin, by: Account): Either[StoreError, ConversationId]

  /** The conversation `origin` names, if one was ever created; never creates one. */
  def find(origin: Origin): Either[StoreError, Option[ConversationId]]

  /** Where `turn`'s workflow is, without waiting for it. */
  def status(turn: TurnRef): TurnStatus

  /** The steps `turn`'s workflow has recorded so far, in the order it ran them; none when
    * they cannot be read, which only dims what a watcher is shown. A step is recorded when
    * it completes, so a running step is not among them.
    */
  def steps(turn: TurnRef): Vector[RecordedStep]

  /** The pieces `turn`'s steps wrote to its stream `key`, in order, as they are written:
    * each `next` waits for one, woken by DBOS's notification with polling behind it, and
    * the pieces end when the turn's workflow does. None when DBOS has no workflow for
    * `turn`: not yet enqueued, or collected. An edge's read (ADR 0006); what the pieces mean
    * is the writer's.
    */
  def stream(turn: TurnRef, key: String): Iterator[String]

  /** The engine holding the database's lock; `None` when none holds it: turns wait until one
    * does.
    */
  def holder(): Option[Holder]
}

object Console {

  /** Opens a [[Console]] onto a database's engine, run where [[runner]] says. */
  trait Opener extends caps.SharedCapability {

    /** Who ran the database's engine when this opener was made. */
    def runner: Runner

    /** The console, opened; called once, off a screen's thread. `Left`, for a person, when the
      * deployment drops a compartment the database ran under. Throws, having closed what it
      * opened, when the database cannot be reached, when attaching finds no engine has yet
      * migrated DBOS's schema there, or when the deployment's workflows fail to launch.
      */
    def open(): Either[String, Console^]
  }

  /** The edge a process opening a console serves itself (ADR 0017), registered through the
    * desks of the engine it opens.
    */
  trait Edge extends caps.SharedCapability {

    /** Registers this edge through `desks` and starts serving it, returning once it serves. */
    def serve(desks: Desks^): Unit
  }

  /** Who runs a database's engine, as an [[Opener]] found it. */
  enum Runner {

    /** This process: the opener starts the engine. */
    case Here

    /** Another process, `holder` once it has written its row: the opener attaches. */
    case Elsewhere(holder: Option[Holder])
  }
}
