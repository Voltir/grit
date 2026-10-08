package grit.kit.run

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
import grit.core.store.{Jot, NoVoucher, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*

/** [[Serving]]: a deployment's edges opened, delivered to and closed, over fakes that record
  * what was asked of them.
  */
object ServingTests extends TestSuite {

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
      new Attesting(NoVoucher, FakeJot, _ => ())
    )

  /** An edge named `called` that records, in `seen`, each open, deliver and close; it refuses
    * to open when `refuse` is set, and its deliveries fail when `unreadable`.
    */
  private final class Fake(
      called: String,
      seen: java.util.concurrent.ConcurrentLinkedQueue[String],
      refuse: Option[EdgeRefusal] = None,
      unreadable: Boolean = false
  ) extends ServedEdge {
    def name: EdgeName = EdgeName(called)
    def needs: Vector[Variable] = Vector.empty
    def answersAsks: Boolean = false
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}] =
      refuse match {
        case Some(r) => Left(r)
        case None =>
          seen.add(s"open $called")
          Right(new ServedEdge.Open {
            def deliver(): Either[StoreError, Int] = {
              seen.add(s"deliver $called")
              if (unreadable) Left(StoreError.Invalid("gone")) else Right(0)
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
      val failure = Serving.open(edges, _ => stores, Map.empty, _ => ()) match {
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
      Serving.open(edges, _ => stores, Map.empty, _ => ()) match {
        case Left(f) => throw new java.lang.AssertionError(f.message)
        case Right(opened) =>
          val rounds = new java.util.concurrent.atomic.AtomicInteger(2)
          Serving.deliver(
            opened,
            () => rounds.get == 0,
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

    test("each edge is opened over the stores built for it") {
      val seen = log()
      val built = log()
      val edges = List(new Fake("a", seen), new Fake("b", seen))
      val opened = Serving.open(
        edges,
        e => { val _ = built.add(EdgeName.value(e.name)); stores },
        Map.empty,
        _ => ()
      )
      opened.foreach(_.close())
      built.asScala.toVector ==> Vector("a", "b")
    }
  }
}
