package grit.dbos.engine

import java.time.Instant

import grit.core.durable.{Durable, StepRecord}
import grit.core.host.ProcessIdentity
import grit.core.id.WorkflowId
import grit.core.spend.Budget
import grit.core.store.StoreError
import grit.core.visibility.Visibility
import grit.dbos.internal.Reader
import grit.dbos.sql.DbConfig

/** An engine on a live test database, for suites that need one. */
object LiveEngine {

  /** The process every test engine and edge says it is: fixed, so a test can name it. */
  val Identity: ProcessIdentity = ProcessIdentity("test-machine", 4242)

  /** No cap, days in UTC: what a test engine takes new messages under unless it says. */
  val Uncapped: Budget = Budget(java.time.ZoneOffset.UTC, None)

  /** A placement that places nothing: the `stitch` body of a suite that does not test
    * placements.
    */
  val Unplaced: WorkflowId -> Durable^ ?-> String =
    id => (_: Durable^) ?=> s"unplaced: ${WorkflowId.value(id)}"

  /** The engine of `config`'s database under compatibility epoch `epoch`, running under
    * `visibility`; throws, naming the holder, when another engine holds its lock (a test that
    * opens two at once has a bug), or naming the compartment `visibility` drops.
    */
  def open(
      config: DbConfig,
      epoch: String,
      budget: Budget = Uncapped,
      visibility: Visibility = Visibility.Shipped
  ): Engine^ =
    opened(config, epoch, budget, visibility, 1)

  /** As [[open]], after a process that held its lock was halted: the lock is free only once
    * the server notices the halted process's connection gone, which can take a moment under
    * load. Tries every half second for up to 30 seconds; throws, naming the holder, after.
    */
  def reopen(config: DbConfig, epoch: String): Engine^ =
    opened(config, epoch, Uncapped, Visibility.Shipped, 60)

  /** The engine, trying `tries` times, half a second apart, while its lock is held. */
  private def opened(
      config: DbConfig,
      epoch: String,
      budget: Budget,
      visibility: Visibility,
      tries: Int
  ): Engine^ =
    Engine.open(config, epoch, Identity, budget, visibility) match {
      case Right(engine) => engine
      case Left(Unopened.Lock(_)) if tries > 1 =>
        Thread.sleep(500)
        opened(config, epoch, budget, visibility, tries - 1)
      case Left(refused) =>
        sys.error(s"the engine could not open: ${refused.message(Instant.now())}")
    }

  /** `id`'s recorded steps, with their outputs, as [[Reader.steps]] reads them from `config`'s
    * database: for a suite outside grit.dbos, which may not name the reader.
    */
  def steps(config: DbConfig, id: WorkflowId): Either[StoreError, Vector[StepRecord]] = {
    val reader = Reader.open(config, Visibility.Shipped)
    try reader.steps(id)
    finally reader.close()
  }
}
