package grit.app.main

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.identity.{Account, TestAccounts}
import grit.core.store.StoreError
import grit.core.visibility.Subject
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, TestPostgres}
import grit.kit.environment.Secrets
import grit.kit.run.{Kit, KitFailure}

import utest.*

/** Against Postgres and DBOS: a serving process whose engine loses its lock, with no other
  * holder, opens a new engine and its edges again without a restart, and one another session
  * holds the lock for is waited out ([[Kit.serving]]).
  */
object LockRecoveryLiveTests extends TestSuite {

  private val Ann: Account = TestAccounts.account("workshop:local/ann")

  /** An edge that, each time it opens, names `Ann` through its stores and keeps what the write
    * answered in `opens`.
    */
  private final class Reopening(opens: ConcurrentLinkedQueue[Either[StoreError, Unit]])
      extends ServedEdge {
    def name: EdgeName = EdgeName("reopening")
    def needs: Vector[Variable] = Vector.empty
    def asks: ServedEdge.Asks = ServedEdge.Asks.Never
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      opens.add(stores.jot.write(Subject.Public)(stores.principals.name(Ann, "Ann")).map(_ => ()))
      Right(new ServedEdge.Open {
        def deliver(): Either[StoreError, Int] = Right(0)
        def close(): Unit = ()
      })
    }
  }

  /** [[Kit.serving]] of an edge keeping its opens in `opens`, over `config`'s database, on a
    * thread of its own until [[stop]].
    */
  private final class Served(
      config: DbConfig,
      opens: ConcurrentLinkedQueue[Either[StoreError, Unit]]
  ) {
    private val stopped = new CountDownLatch(1)
    private val ended = new AtomicReference[Option[Either[KitFailure, Unit]]](None)
    private val thread = {
      val d = ServedDeployment.of(new Reopening(opens))
      val env = ServedDeployment.env(config)
      val secrets = Secrets.of(env, d).fold(r => sys.error(r.message), identity)
      new Thread(() => ended.set(Some(Kit.serving(d, secrets, env, Clock.system(), stopped))))
    }
    thread.setDaemon(true)
    thread.start()

    /** Whether serving is still under way. */
    def alive: Boolean = thread.isAlive

    /** Stops serving and waits up to a minute for it to end; what it returned, if it did. */
    def stop(): Option[Either[KitFailure, Unit]] = {
      stopped.countDown()
      thread.join(60.seconds.toMillis)
      ended.get()
    }
  }

  /** Waits up to `within` for `ok`; whether it came. */
  private def eventually(within: FiniteDuration)(ok: => Boolean): Boolean = {
    val until = System.nanoTime() + within.toNanos
    var done = ok
    while (!done && System.nanoTime() < until) { Thread.sleep(100); done = ok }
    done
  }

  val tests = Tests {

    test(
      "a served engine whose lock connection is terminated opens again, with a second engine start and its edge reopened over the new engine's stores"
    ) {
      val config = TestPostgres.freshDatabase("lock_recovery_reopens")
      val opens = new ConcurrentLinkedQueue[Either[StoreError, Unit]]()
      val serving = new Served(config, opens)
      val seen =
        try {
          assert(eventually(60.seconds)(opens.size == 1))
          LiveEngine.terminateHolder(config)
          val reopened = eventually(30.seconds)(opens.size == 2)
          (reopened, LiveEngine.starts(config), opens.asScala.toVector)
        } finally { val _ = serving.stop() }
      (seen, serving.stop()) ==> ((true, 2, Vector(Right(()), Right(()))), Some(Right(())))
    }

    test("a lock another session takes after the loss is waited out, then taken") {
      val config = TestPostgres.freshDatabase("lock_recovery_waits")
      val opens = new ConcurrentLinkedQueue[Either[StoreError, Unit]]()
      val serving = new Served(config, opens)
      val seen =
        try {
          assert(eventually(60.seconds)(opens.size == 1))
          LiveEngine.terminateHolder(config)
          // Taken as soon as the terminated session has gone: well before the engine's next
          // heartbeat finds its lock lost.
          val seized = LiveEngine.seize(config)
          val during =
            try {
              Thread.sleep(4000)
              (LiveEngine.starts(config), opens.size, serving.alive)
            } finally seized.close()
          (during, eventually(45.seconds)(opens.size == 2), LiveEngine.starts(config))
        } finally { val _ = serving.stop() }
      (seen, serving.stop()) ==> (((1, 1, true), true, 2), Some(Right(())))
    }
  }
}
