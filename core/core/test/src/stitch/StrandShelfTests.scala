package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, EntryId}
import grit.core.identity.TestAccounts
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.{Clearance, Integrity, Label, Recorded, TestLabels}
import grit.dbos.sql.TestTx

import utest.*

/** [[StrandShelf]]: a room's strands by ids, counts and a digest. */
object StrandShelfTests extends TestSuite {

  import StrandWorld.{at, right}

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

    test("a strand reads back as a step records it, which holds none of its messages' text") {
      val w = new StrandWorld
      val a = w.thread("C1", "1.0")
      val b = w.thread("C1", "2.0")
      w.say(a, "the deploy is stuck", 0)
      w.follow(w.say(b, "still stuck?", 60), a)
      val seen = w.of(a, at(600))(using TestTx.fake)
      val written = seen.map(Strand.Seen.write)
      written.map(Strand.Seen.read) ==> seen.map(Right(_))
      assert(written.forall(v => !ujson.write(v).contains("stuck")))
    }
  }
}
