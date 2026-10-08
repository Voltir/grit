package grit.kit.run

import grit.core.clock.{Clock, SetClock}
import grit.core.edge.{
  Attesting,
  EdgeRefusal,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  ServedEdge,
  Variable
}
import grit.core.id.EdgeName
import grit.core.inbox.InMemoryInbox
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

/** [[Serving]]: a deployment's edges opened, delivered to and closed, over fakes that record
  * what was asked of them.
  */
object ServingTests extends TestSuite {

  /** A clock for an edge opened here; none of them reads it. */
  private def stopped(): Clock^ = new SetClock(java.time.Instant.EPOCH)

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
  private val stores =
    EdgeStores(
      inbox,
      inbox.principals,
      new InMemoryDeliveries,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      FakeJot,
      new InMemoryEdges,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ())
    )

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
    def answersAsks: Boolean = false
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
      val failure = Serving.open(edges, _ => stores, Map.empty, stopped(), _ => ()) match {
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
      Serving.open(edges, _ => stores, Map.empty, stopped(), _ => ()) match {
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
      Serving.open(edges, _ => stores, Map.empty, stopped(), _ => ()) match {
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
      Serving.open(edges, _ => stores, Map.empty, stopped(), _ => ()) match {
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
        e => { val _ = built.add(EdgeName.value(e.name)); stores },
        Map.empty,
        stopped(),
        _ => ()
      )
      opened.foreach(_.close())
      built.asScala.toVector ==> Vector("a", "b")
    }
  }
}
