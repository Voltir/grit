package grit.core.stitch

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.period.Probability
import grit.core.place.Scope
import grit.core.store.{EntrySearch, Payload, Speakers}

import utest.*
import StitchFixtures.*

/** [[Stitching]]: which exchanges a first message is offered, where the classifier places it,
  * and how a reader's excerpt is cut.
  */
object StitchingTests extends TestSuite {

  private val Day = 86_400L
  private val first = heard("new", "Did the Powered by Engine line get added?", 0)

  private def hit(s: Said, score: Double) =
    EntrySearch.Hit(s.entry.id, TurnRef(s.conversation, TurnSeq.First), score)

  private def offered(es: Vector[Exchange]): Vector[(ConversationId, Offered)] =
    es.map(e => e.root -> e.offered)

  private def p(x: Double) = Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  val tests = Tests {
    test("a lexical match a week old is offered past two more recent exchanges") {
      val engine = heard("engine", "where did we land on the Engine contract term?", 6 * Day)
      val s1 = heard("s1", "sandwiches?", 120)
      val s2 = heard("s2", "lunch at noon", 300)
      val s3 = heard("s3", "standup moved", 600)
      val stale = heard("stale", "Engine contract draft", 8 * Day)
      val said = Vector(stale, engine, s3, s2, s1)
      val es = Stitching.offer(
        first,
        room(),
        said,
        Vector.empty,
        Vector(hit(stale, 9.0), hit(engine, 3.0)),
        Vector.empty,
        Scope.Room,
        Tuning.Default
      )
      offered(es) ==> Vector(
        s1.conversation -> Offered.Recent(1),
        s2.conversation -> Offered.Recent(2),
        s3.conversation -> Offered.Recent(3),
        engine.conversation -> Offered.Lexical(3.0)
      )
    }

    test("a conversation in another channel is never offered, even under everywhere") {
      val here = heard("here", "the Engine contract", 60)
      val there = heard("there", "the Engine contract", 30, channel = "C2")
      val es = Stitching.offer(
        first,
        room(),
        Vector(here, there),
        Vector.empty,
        Vector(hit(there, 5.0), hit(here, 1.0)),
        Vector.empty,
        Scope.Everywhere,
        Tuning.Default
      )
      offered(es) ==> Vector(here.conversation -> Offered.Recent(1))
    }

    test("Off offers nothing") {
      val here = heard("here", "the Engine contract", 60)
      val es = Stitching.offer(
        first,
        room(),
        Vector(here),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Scope.Off,
        Tuning.Default
      )
      es ==> Vector.empty
    }

    test("a follower's messages are offered as its root's exchange, opened by the root") {
      val root = heard("engine", "where did we land on the Engine contract term?", 90)
      val follower = heard("real", "Is this a real question", 60)
      val es = Stitching.offer(
        first,
        room(),
        Vector(follower),
        Vector(root),
        Vector.empty,
        Vector(Link(follower.conversation, root.conversation)),
        Scope.Room,
        Tuning.Default
      )
      es.map(e => (e.root, e.opening, e.latest)) ==>
        Vector((root.conversation, root, Vector(follower)))
    }

    test("under followsAt it begins something new; at it, it follows the chosen root") {
      val engine = heard("engine", "the Engine contract term?", 30)
      val lunch = heard("lunch", "lunch?", 60)
      val es = Stitching.offer(
        heard("real", "Is this a real question", 0),
        room(),
        Vector(lunch, engine),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Scope.Room,
        Tuning.Default
      )
      def placed(pEngine: Double, key: String = "exchange 1") = Stitching.place(
        new Scripted(
          Some(
            chose(
              key,
              "exchange 1" -> pEngine,
              "exchange 2" -> 0.1,
              Stitching.NewKey -> (0.9 - pEngine)
            )
          )
        ),
        Offer(heard("real", "Is this a real question", 0), "David", es, Speakers.none),
        Tuning.Default
      )
      placed(0.55) match {
        case Placed.Begins(pb, _, _, _) => pb ==> p(0.55)
        case other => throw new java.lang.AssertionError(s"not begins: $other")
      }
      // Something new chosen: how likely the likeliest exchange was, not the choice.
      placed(0.2, Stitching.NewKey) match {
        case Placed.Begins(pb, _, _, _) => pb ==> p(0.2)
        case other => throw new java.lang.AssertionError(s"not begins: $other")
      }
      placed(0.6) match {
        case Placed.Follows(r, pf, seen, _, _) =>
          (r, pf) ==> (engine.conversation, p(0.6))
          seen.offered.map(o => (o.root, o.p)) ==> Vector(
            engine.conversation -> Some(p(0.6)),
            lunch.conversation -> Some(p(0.1))
          )
        case other => throw new java.lang.AssertionError(s"not follows: $other")
      }
    }

    test("a classifier that fails leaves the message unread, with what it was shown") {
      val engine = heard("engine", "the Engine contract term?", 30)
      val es = Stitching.offer(
        first,
        room(),
        Vector(engine),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Scope.Room,
        Tuning.Default
      )
      val jev = new Scripted(None)
      Stitching.place(jev, Offer(first, "Nick", es, Speakers.none), Tuning.Default) match {
        case Placed.Unread(why, seen) =>
          why ==> "unavailable: down"
          seen.state ==> jev.states.head
          seen.offered ==> Vector(Seen.Offer(engine.conversation, Offered.Recent(1), None))
        case other => throw new java.lang.AssertionError(s"not unread: $other")
      }
    }

    test("an excerpt keeps the opening, then the messages nearest the end, a gap between") {
      val opening = heard("engine", "where did we land on the Engine contract term?", 60)
      val strand = Vector(
        heard("a", "first reply here", 50),
        heard("b", "second reply", 40),
        heard("c", "lol", 30)
      )
      val names = Speakers(strand.map(_.entry.id -> "David").toMap + (opening.entry.id -> "Nick"))
      val openLine = "Nick: where did we land on the Engine contract term?"
      val chars = openLine.length + 1 + 2 + "David: second reply".length + 1 + "David: lol".length
      Stitching.excerpt(Some(opening), strand, names, chars) ==>
        s"$openLine\n…\nDavid: second reply\nDavid: lol"
      Stitching.excerpt(Some(opening), strand, names, 1_000) ==>
        s"$openLine\nDavid: first reply here\nDavid: second reply\nDavid: lol"
    }

    test("an excerpt whose opening is grit's post shows it under Assistant") {
      val heardOpening = heard("post", "unused", 60)
      val opening = heardOpening.copy(entry =
        heardOpening.entry.copy(payload = Payload.Posted("The engine's open issues."))
      )
      val reply = heard("post", "why this?", 30, seq = 1)
      Stitching.excerpt(
        Some(opening),
        Vector(reply),
        Speakers(Map(reply.entry.id -> "Nick")),
        1_000
      ) ==> "Assistant: The engine's open issues.\nNick: why this?"
    }
  }
}
