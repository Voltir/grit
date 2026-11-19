package grit.core.stitch

import grit.core.id.{EntryId, Href}
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Integrity, Label, Recorded, TestLabels}
import grit.dbos.sql.TestTx

import utest.*

/** [[StrandShelf]]: a room's strands by ids, counts and a digest. */
object StrandShelfTests extends TestSuite {

  import StrandWorld.{at, right}

  /** Runs of messages less than 6 hours apart, at most 4, settled in 6 hours. */
  private val sixHours: Strand.Cut = Strand.Cut
    .of(java.time.Duration.ofHours(6), 4, java.time.Duration.ofHours(6))
    .fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("the same messages give the same digest; one more message another") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 60), a)
      given Tx = TestTx.fake
      val before = w.of(a, at(600)).map(_.digest)
      w.of(a, at(600)).map(_.digest) ==> before
      w.say(b, "yes", 120)
      val after = w.of(a, at(600)).map(_.digest)
      assert(before.size == 1, after.size == 1, before != after)
      StrandDigest.of(Vector(EntryId("x"), EntryId("y"))) ==>
        StrandDigest.of(Vector(EntryId("y"), EntryId("x")))
    }

    test("a digest reads back from its text; text of another shape does not") {
      val d = StrandDigest.of(Vector(EntryId("x")))
      StrandDigest.read(StrandDigest.value(d)) ==> Right(d)
      assert(StrandDigest.read("0123456789ABCDEF").isLeft, StrandDigest.read("0123").isLeft)
    }

    test("a strand is its root then its followers by first message, said and chars counted") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      val c = w.thread("C1", "3.0")
      w.say(a, "abc", 0)
      w.follow(w.say(c, "x" * 400, 30), a)
      w.follow(w.say(b, "hello", 60), a)
      given Tx = TestTx.fake
      val seen = w.of(a, at(600))
      seen.map(s => (s.root, s.members, s.said, s.chars, s.since, s.touched, s.asOf)) ==>
        Vector((a.id, Vector(a.id, c.id, b.id), 3, 3 + 300 + 5, at(0), at(60), at(600)))
      seen.map(_.room) ==> Vector(Origin.Slack("T", "C1", "1.0").room)
    }

    test(
      "links lists the links of its members' messages said before asOf, each once, oldest first"
    ) {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      val other = w.thread("C1", "3.0")
      w.say(a, "the plan (https://h.io/plan)", 0)
      w.follow(w.say(b, "see https://h.io/b and https://h.io/plan", 30), a)
      w.say(b, "later https://h.io/late", 900)
      w.say(other, "not ours https://h.io/other", 10)
      given Tx = TestTx.fake
      val seen = w.of(a, at(600))
      seen.map(s => right(w.shelf.links(s)).map(Href.value)) ==>
        Vector(Vector("https://h.io/plan", "https://h.io/b"))
    }

    test("a member with nothing said before asOf is no member; a root with nothing is left out") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 60), a)
      given Tx = TestTx.fake
      w.of(a, at(30)).map(_.members) ==> Vector(Vector(a.id))
      w.of(a, at(0)) ==> Vector.empty
    }

    test("touched: the strands spoken in over a range, as they stood at its end, oldest first") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      val s = w.thread("C1", "3.0")
      val d = w.thread("C1", "4.0")
      val q = w.thread("C1", "5.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 10), a)
      w.say(s, "lunch at noon", 20)
      w.follow(w.say(d, "noon works", 30), s)
      w.say(q, "a lone question", 500)
      w.say(a, "fixed now", 900)
      w.say(d, "moved to one", 800)
      given Tx = TestTx.fake
      right(w.shelf.touched(at(700), at(1000))).map(x => (x.root, x.said, x.touched)) ==>
        Vector((s.id, 3, at(800)), (a.id, 3, at(900)))
      right(w.shelf.touched(at(700), at(850))).map(x => (x.root, x.said)) ==>
        Vector((s.id, 3))
    }

    test("a strand's room label is the one in force; atFloor keeps the floor's strands alone") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      val s = w.thread("C2", "1.0")
      val d = w.thread("C2", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 10), a)
      w.say(s, "the trial results", 20)
      w.follow(w.say(d, "results are in", 30), s)
      val trialRoom = Origin.Slack("T", "C2", "1.0").room
      given Tx = TestTx.fake(
        Clearance.of(TestLabels.Trial),
        TestLabels.Trialled,
        Recorded(
          Map(trialRoom -> Recorded.Kept(None, Some(TestLabels.Trial), false, None)),
          Map.empty
        )
      )
      right(w.shelf.touched(at(0), at(100))).map(x => (x.root, x.roomLabel)) ==>
        Vector((a.id, Label.Public), (s.id, TestLabels.Trial))
      right(w.shelf.atFloor(at(0), at(100))).map(_.root) ==> Vector(s.id)
    }

    test(
      "an excerpt is the root's opening then the rest, believed at its least message; none once one is said since its scan"
    ) {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 60, Integrity.Outside), a)
      given Tx = TestTx.fake
      val seen = w.of(a, at(600))
      seen.map(s => right(w.excerpts.excerpt(s, 1000)).map(x => (x.excerpt.text, x.integrity))) ==>
        Vector(Some(("Someone: the deploy is stuck\nSomeone: still stuck?", Integrity.Outside)))
      w.say(b, "yes", 120)
      seen.map(s => right(w.excerpts.excerpt(s, 1000))) ==> Vector(None)
    }

    test("a thread is shown its asking message as its opening") {
      val w = new StrandWorld
      val top = w.room("C1")
      w.say(top, "who can review the deploy script?", 0, source = Some("1.0"))
      val t = w.under("C1", "1.0")
      w.say(t, "I can, after lunch", 60)
      given Tx = TestTx.fake
      val units = right(w.shelf.units(at(0), at(600), Strand.Cut.Default))
      units.map(u => (u.root, u.span)) ==> Vector((t.id, Strand.Span.Whole))
      units.flatMap(u => right(w.excerpts.excerpt(u, 1000)).map(_.excerpt.text)) ==>
        Vector("Someone: who can review the deploy script?\nSomeone: I can, after lunch")
    }

    test("a thread under grit's post, or under an unread label, has no opening") {
      val w = new StrandWorld
      val open = w.room("C1")
      val shut = w.room("C2", TestLabels.Trial)
      w.say(open, "the nightly report", 0, source = Some("1.0"), posted = true)
      w.say(shut, "the trial's numbers", 0, source = Some("1.0"))
      val a = w.under("C1", "1.0")
      val b = w.under("C2", "1.0")
      w.say(a, "thanks", 60)
      w.say(b, "they look good", 60)
      given Tx = TestTx.fake(Clearance.of(Label.Public), TestLabels.Trialled)
      right(w.shelf.units(at(0), at(600), Strand.Cut.Default))
        .filter(_.span == Strand.Span.Whole)
        .flatMap(u => right(w.excerpts.excerpt(u, 1000)).map(_.excerpt.text)) ==>
        Vector("Someone: thanks", "Someone: they look good")
    }

    test("a top level is offered as runs at its room's label, and none at another label") {
      val w = new StrandWorld
      val c1 = w.room("C1")
      val c2 = w.room("C2")
      w.say(c1, "standup in five", 0)
      w.say(c1, "joining", 60)
      w.say(c1, "lunch?", 1200)
      w.say(c2, "the trial results", 0)
      given Tx = TestTx.fake(
        Clearance.of(Label.Public),
        recorded = Recorded(
          Map(
            Origin.Room("T", "C2").room -> Recorded.Kept(None, Some(TestLabels.Trial), false, None)
          ),
          Map.empty
        )
      )
      right(w.shelf.units(at(0), at(4000), Strand.Cut.Default)).map(u =>
        (u.root, u.said, u.since)
      ) ==>
        Vector((c1.id, 2, at(0)), (c1.id, 1, at(1200)))
    }

    test("a run cut on two nights' windows is the same run") {
      val w = new StrandWorld
      val top = w.room("C1")
      // A chain every 4 minutes from 0 to 40, cut at most 4: runs 0-12, 16-28, 32-40.
      (0 to 10).foreach(i => w.say(top, s"step $i", i * 240L))
      given Tx = TestTx.fake
      val four = Strand.Cut
        .of(java.time.Duration.ofMinutes(5), 4, java.time.Duration.ofMinutes(30))
        .fold(e => throw new java.lang.AssertionError(e), identity)
      def ids(from: Long) =
        right(w.shelf.units(at(from), at(9000), four)).map(u => (u.id, u.digest, u.said))
      // Read from minute 20, the run 16-28 is cut from 16, not from the window; from 33:20,
      // the run 32-40 from 32.
      assert(ids(0).size == 3)
      ids(1200) ==> ids(0).drop(1)
      ids(2000) ==> ids(0).drop(2)
    }

    test("a chain begun more than a day before the window is cut from its start") {
      val w = new StrandWorld
      val top = w.room("C1")
      // A chain every 5 hours, 6 apart joining, from 0 to 60 hours, cut at most 4.
      (0 to 12).foreach(i => w.say(top, s"step $i", i * 18000L))
      given Tx = TestTx.fake
      def units(from: Long) = right(w.shelf.units(at(from), at(900000), sixHours))
      // From 50 hours the chain's start is two days back: its runs are 8-11 and 12, not 6-9.
      units(180000).map(_.since) ==> Vector(at(8 * 18000L), at(12 * 18000L))
      units(180000) ==> units(0).filterNot(_.touched.isBefore(at(180000)))
    }

    test("a chain is read back at most ReadBack days; the oldest message read then starts it") {
      val w = new StrandWorld
      val top = w.room("C1")
      // A chain every 5 hours from 0 to 225 hours; from 216 hours, 7 days back is 48 hours,
      // whose first message is the 10th: runs count from it, not from 0.
      (0 to 45).foreach(i => w.say(top, s"step $i", i * 18000L))
      given Tx = TestTx.fake
      (
        StrandShelf.ReadBack,
        right(w.shelf.units(at(777600), at(900000), sixHours)).map(_.since)
      ) ==> (7, Vector(at(42 * 18000L)))
    }

    test("a run's excerpt and links read its range, not the whole root") {
      val w = new StrandWorld
      val top = w.room("C1")
      w.say(top, "see https://example.com/a", 0)
      w.say(top, "see https://example.com/b", 1200)
      w.say(top, "see https://example.com/c", 2400)
      given Tx = TestTx.fake
      val middle =
        right(w.shelf.units(at(1000), at(4000), Strand.Cut.Default)).filter(_.since == at(1200))
      middle.flatMap(u => right(w.shelf.links(u))).map(_.toString) ==>
        Vector(Href.in("see https://example.com/b").map(_.toString)).flatten
      middle.flatMap(u => right(w.excerpts.excerpt(u, 1000)).map(_.excerpt.text)) ==>
        Vector("Someone: see https://example.com/b")
    }

    test("an excerpt reads changed once a message of the run is removed") {
      val w = new StrandWorld
      val top = w.room("C1")
      w.seal(w.say(top, "who has the keys?", 0), 30)
      val middle = w.say(top, "not me", 60)
      w.seal(middle, 90)
      w.say(top, "me", 120)
      given Tx = TestTx.fake
      val run = right(w.shelf.units(at(0), at(4000), Strand.Cut.Default))
      run.map(u => (u.said, right(w.excerpts.excerpt(u, 1000)).isDefined)) ==> Vector((3, true))
      w.purgeSealed(middle, 3000)
      run.map(u => right(w.excerpts.excerpt(u, 1000))) ==> Vector(None)
    }

    test("a strand reads back as a step records it, which holds none of its messages' text") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 60), a)
      val seen = w.of(a, at(600))(using TestTx.fake)
      val written = seen.map(Strand.Digested.write)
      written.map(Strand.Digested.read) ==> seen.map(Right(_))
      assert(written.forall(v => !ujson.write(v).contains("stuck")))
    }
  }
}
