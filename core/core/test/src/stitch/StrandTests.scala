package grit.core.stitch

import java.time.Duration

import grit.core.id.{ConversationId, EntryId, EntrySeq}
import grit.core.place.{Namespace, Place}
import grit.core.visibility.Label

import utest.*

/** [[Strand.of]]: a star, never a chain; [[Strand.Cut]]: a channel's top level cut into runs. */
object StrandTests extends TestSuite {

  private def c(n: String) = ConversationId(n)

  /** A channel's top-level messages, the `i`th said `minutes(i)` past a fixed hour. */
  private def top(minutes: Long*): Vector[Said] =
    minutes.toVector.zipWithIndex.map((m, i) =>
      StitchFixtures.heard("top", s"m$i", 100000 - m * 60, seq = i.toLong)
    )

  /** `minutes` past that hour. */
  private def past(minutes: Long) = StitchFixtures.Now.minusSeconds(100000 - minutes * 60)

  private def cut(gap: Long, most: Int, settle: Long): Strand.Cut =
    Strand.Cut
      .of(Duration.ofMinutes(gap), most, Duration.ofMinutes(settle))
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** Each run offered `until` minutes past the hour, by its messages' indices. */
  private def runs(c: Strand.Cut, said: Vector[Said], until: Long): Vector[Vector[Long]] =
    c.pieces(said, past(until)).map(_.map(x => EntrySeq.value(x.entry.seq)))

  private def digested(span: Strand.Span) = Strand.Digested(
    Place.under(Namespace.Slack, Vector("T", "C1")),
    Label.Public,
    c("room"),
    Vector(c("room")),
    2,
    40,
    past(0),
    past(4),
    StrandDigest.of(Vector(EntryId("a"), EntryId("b"))),
    past(60),
    span
  )

  val tests = Tests {
    test("a strand is its root and the root's direct followers, never a follower's follower") {
      // B and C follow A; D follows C, which placement never makes, but a race can.
      val links = Vector(Link(c("B"), c("A")), Link(c("C"), c("A")), Link(c("D"), c("C")))
      Strand.of(c("B"), links) ==> Set(c("A"), c("B"), c("C"))
      Strand.of(c("A"), links) ==> Set(c("A"), c("B"), c("C"))
      Strand.of(c("D"), links) ==> Set(c("C"), c("D"))
      Strand.of(c("E"), links) ==> Set(c("E"))
    }

    test("less than the gap apart join one run; one at the gap starts the next") {
      // 0, 4 and 8 chain; 13 is 5 after 8, at the gap; 17 joins it.
      runs(cut(5, 16, 30), top(0, 4, 8, 13, 17), 200) ==>
        Vector(Vector(0L, 1L, 2L), Vector(3L, 4L))
    }

    test("gap zero makes each its own") {
      runs(cut(0, 16, 30), top(0, 0, 1), 200) ==> Vector(Vector(0L), Vector(1L), Vector(2L))
    }

    test("a run is cut at most") {
      runs(cut(5, 2, 30), top(0, 1, 2, 3, 4), 200) ==>
        Vector(Vector(0L, 1L), Vector(2L, 3L), Vector(4L))
    }

    test("a run waits for a message to follow it or settle") {
      val said = top(0, 1, 20)
      // At 45: 0-1 is followed; 20 is not, and 25 minutes have passed, not 30.
      runs(cut(5, 16, 30), said, 45) ==> Vector(Vector(0L, 1L))
      // At 50, 30 minutes have passed since 20.
      runs(cut(5, 16, 30), said, 50) ==> Vector(Vector(0L, 1L), Vector(2L))
    }

    test("Cut.of refuses most 0 or 65, a negative gap, settle under gap") {
      val m = Duration.ofMinutes(5)
      assert(
        Strand.Cut.of(m, 0, m).isLeft,
        Strand.Cut.of(m, 65, m).isLeft,
        Strand.Cut.of(m, 16, Duration.ofMinutes(4)).isLeft,
        Strand.Cut.of(Duration.ofMinutes(-1), 16, m).isLeft,
        Strand.Cut.of(m, 64, m).isRight
      )
      Strand.Cut.read(Strand.Cut.write(Strand.Cut.Default)) ==> Right(Strand.Cut.Default)
    }

    test("a whole strand's record writes no span, and reads back as before") {
      val whole = digested(Strand.Span.Whole)
      val run = digested(Strand.Span.Run(EntryId("a"), EntryId("b")))
      assert(!Strand.Digested.write(whole).obj.contains("span"))
      Vector(whole, run).map(s => Strand.Digested.read(Strand.Digested.write(s))) ==>
        Vector(Right(whole), Right(run))
    }

    test("two runs of one channel have different ids") {
      val one = digested(Strand.Span.Run(EntryId("a"), EntryId("b")))
      val two = digested(Strand.Span.Run(EntryId("c"), EntryId("d")))
      assert(one.root == two.root, one.id != two.id, digested(Strand.Span.Whole).id.first.isEmpty)
    }
  }
}
