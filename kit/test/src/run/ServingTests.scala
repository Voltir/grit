package grit.kit.run

import scala.concurrent.duration.DurationInt

import grit.core.admin.InMemoryAdministration
import grit.core.clock.{Clock, SetClock}
import grit.core.document.InMemoryDocuments
import grit.core.edge.InMemoryJoins
import grit.core.edge.{
  Attesting,
  EdgeRefusal,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryOrigins,
  ServedEdge,
  Variable
}
import grit.core.files.NoIntake
import grit.core.id.EdgeName
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, StoreError, TestJot}

import utest.*

/** [[Serving]]: a deployment's edges opened, delivered to and closed, over fakes that record
  * what was asked of them.
  */
object ServingTests extends TestSuite {

  /** A clock for an edge opened here; none of them reads it. */
  private def stopped(): Clock^ = new SetClock(java.time.Instant.EPOCH)

  // Fresh for each use: a suite object cannot hold a capability as a field.
  private def stores: EdgeStores^ = {
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    EdgeStores(
      inbox,
      InMemoryAdministration.none(),
      InMemoryJoins.none(),
      inbox.principals,
      new InMemoryDeliveries,
      inbox.ledger,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      new InMemoryOrigins(inbox.conversations),
      (new InMemoryDocuments).reviews.of,
      TestJot(),
      new InMemoryEdges,
      new Attesting(InMemoryVoucher.none(), TestJot(), _ => ()),
      new NoIntake
    )
  }

  /** An edge named `called` that records, in `seen`, each open, deliver, look and close; it
    * refuses to open when `refuse` is set, its deliveries fail when `unreadable`, and its looks
    * when `unattestable`.
    */
  private final class Fake(
      called: String,
      seen: java.util.concurrent.ConcurrentLinkedQueue[String],
      refuse: Option[EdgeRefusal] = None,
      unreadable: Boolean = false,
      unattestable: Boolean = false
  ) extends ServedEdge {
    def name: EdgeName = EdgeName(called)
    def needs: Vector[Variable] = Vector.empty
    def asks: ServedEdge.Asks = ServedEdge.Asks.Unanswered
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
      refuse match {
        case Some(r) => Left(r)
        case None =>
          seen.add(s"open $called")
          Right(new ServedEdge.Open {
            def deliver(): Either[StoreError, Int] = {
              seen.add(s"deliver $called")
              if (unreadable) Left(StoreError.Invalid("gone")) else Right(0)
            }
            override def attest(): Either[StoreError, Int] = {
              seen.add(s"attest $called")
              if (unattestable) Left(StoreError.Invalid("lost")) else Right(1)
            }
            def close(): Unit = { val _ = seen.add(s"close $called") }
          })
      }
  }

  private def log() = new java.util.concurrent.ConcurrentLinkedQueue[String]()

  import scala.jdk.CollectionConverters.*

  val tests = Tests {
    test("an edge that refuses to open closes those opened before it, and is the failure") {
      val seen = log()
      val refused = EdgeRefusal.Refused("no")
      val edges = List(new Fake("a", seen), new Fake("b", seen), new Fake("c", seen, Some(refused)))
      val failure =
        Serving.open(edges, { val s = stores; _ => s }, Map.empty, stopped(), _ => ()) match {
          case Left(f) => Some(f)
          case Right(_) => None
        }
      (failure, seen.asScala.toVector) ==> (
        Some(KitFailure.Edge(EdgeName("c"), refused)),
        Vector("open a", "open b", "close b", "close a")
      )
    }

    test(
      "each round asks every edge in order, naming one whose replies could not be read, until stopped; closing closes each once, the last opened first"
    ) {
      val seen = log()
      val warned = log()
      val edges = List(new Fake("a", seen, unreadable = true), new Fake("b", seen))
      Serving.open(edges, { val s = stores; _ => s }, Map.empty, stopped(), _ => ()) match {
        case Left(f) => throw new java.lang.AssertionError(f.message)
        case Right(opened) =>
          val rounds = new java.util.concurrent.atomic.AtomicInteger(2)
          Serving.deliver(
            opened,
            () => rounds.get == 0,
            () => false,
            () => { val _ = rounds.decrementAndGet() },
            w => { val _ = warned.add(w) }
          )
          opened.close()
      }
      (seen.asScala.toVector, warned.asScala.toVector) ==> (
        Vector(
          "open a",
          "open b",
          "deliver a",
          "deliver b",
          "deliver a",
          "deliver b",
          "close b",
          "close a"
        ),
        Vector.fill(2)("a: replies not read: Invalid(gone)")
      )
    }

    test(
      "a look asks every edge once, in order, naming one whose store failed and still asking the next"
    ) {
      val seen = log()
      val warned = log()
      val edges = List(new Fake("a", seen, unattestable = true), new Fake("b", seen))
      Serving.open(edges, { val s = stores; _ => s }, Map.empty, stopped(), _ => ()) match {
        case Left(f) => throw new java.lang.AssertionError(f.message)
        case Right(opened) =>
          opened.attest(w => { val _ = warned.add(w) })
          opened.close()
      }
      (seen.asScala.toVector, warned.asScala.toVector) ==> (
        Vector("open a", "open b", "attest a", "attest b", "close b", "close a"),
        Vector("a: accounts not attested: Invalid(lost)")
      )
    }

    test("delivering asks for a look before each round its look says is due, and only then") {
      val seen = log()
      val edges = List(new Fake("a", seen))
      Serving.open(edges, { val s = stores; _ => s }, Map.empty, stopped(), _ => ()) match {
        case Left(f) => throw new java.lang.AssertionError(f.message)
        case Right(opened) =>
          val rounds = new java.util.concurrent.atomic.AtomicInteger(3)
          Serving.deliver(
            opened,
            () => rounds.get == 0,
            () => rounds.get != 2,
            () => { val _ = rounds.decrementAndGet() },
            _ => ()
          )
          opened.close()
      }
      seen.asScala.toVector ==> Vector(
        "open a",
        "attest a",
        "deliver a",
        "deliver a",
        "attest a",
        "deliver a",
        "close a"
      )
    }

    test("each edge is opened over the stores built for it") {
      val seen = log()
      val built = log()
      val edges = List(new Fake("a", seen), new Fake("b", seen))
      val opened = Serving.open(
        edges,
        { val s = stores; e => { val _ = built.add(EdgeName.value(e.name)); s } },
        Map.empty,
        stopped(),
        _ => ()
      )
      opened.foreach(_.close())
      built.asScala.toVector ==> Vector("a", "b")
    }
    test("a served repository's edge, once closed, stops reading its adverts again") {
      import grit.core.agent.{SubagentFixtures, Subagents}
      import grit.host.git.GitFixture
      GitFixture.inRepository { repo =>
        val coding = SubagentFixtures.coding.copy(repository = GitFixture.repository)
        val subagents = SubagentFixtures.ok(
          Subagents.of(
            Vector(
              SubagentFixtures.ok(
                SubagentFixtures.subagent(
                  SubagentFixtures.coder,
                  coding,
                  grit.core.visibility.Integrity.Member
                )
              )
            )
          )
        )
        val clone =
          grit.kit.deployment.RepositoryClone(GitFixture.repository, repo.source, repo.root)
        val hosted = grit.kit.deployment.Deployments
          .of(subagents = subagents, repositories = Vector(clone))
          .fold(r => sys.error(r.message), _.hosted)
        val name = s"grit-advert ${GitFixture.repository.place.written}"
        def advertising: Boolean =
          scala.jdk.CollectionConverters
            .SetHasAsScala(Thread.getAllStackTraces.keySet)
            .asScala
            .exists(t => t.getName == name && t.isAlive)
        val edge = hosted.toList match {
          case h :: Nil => LocalEdge.served(h, every = 10.millis)
          case other => sys.error(s"hosted: $other")
        }
        val before = edge.open(stores, Map.empty, Clock.system(), _ => ()) match {
          case Left(r) => sys.error(r.message)
          case Right(opened) =>
            val was = advertising
            opened.close()
            was
        }
        val deadline = System.nanoTime() + 2.seconds.toNanos
        while (advertising && System.nanoTime() < deadline) Thread.sleep(10)
        (before, advertising) ==> (true, false)
      }
    }
  }
}
