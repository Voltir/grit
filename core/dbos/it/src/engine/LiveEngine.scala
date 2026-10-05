package grit.dbos.engine

import java.time.Instant

import grit.core.durable.Durable
import grit.core.host.ProcessIdentity
import grit.core.id.WorkflowId
import grit.core.spend.Budget
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

  /** The engine of `config`'s database under compatibility epoch `epoch`; throws, naming the
    * holder, when another engine holds its lock: a test that opens two at once has a bug.
    */
  def open(config: DbConfig, epoch: String, budget: Budget = Uncapped): Engine^ =
    opened(config, epoch, budget, 1)

  /** As [[open]], after a process that held its lock was halted: the lock is free only once
    * the server notices the halted process's connection gone, which can take a moment under
    * load. Tries every half second for up to 30 seconds; throws, naming the holder, after.
    */
  def reopen(config: DbConfig, epoch: String): Engine^ =
    opened(config, epoch, Uncapped, 60)

  /** The engine, trying `tries` times, half a second apart, while its lock is held. */
  private def opened(config: DbConfig, epoch: String, budget: Budget, tries: Int): Engine^ =
    Engine.open(config, epoch, Identity, budget) match {
      case Right(engine) => engine
      case Left(_) if tries > 1 =>
        Thread.sleep(500)
        opened(config, epoch, budget, tries - 1)
      case Left(refused) =>
        sys.error(s"the engine could not open: ${refused.message(Instant.now())}")
    }
}
