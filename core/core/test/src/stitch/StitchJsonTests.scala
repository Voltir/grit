package grit.core.stitch

import grit.core.store.Speakers

import utest.*
import StitchFixtures.*

/** [[StitchJson]]: every placement reads back as it was written, under stored names. */
object StitchJsonTests extends TestSuite {

  private val engine = heard("engine", "the Engine contract term?", 30)
  private val lunch = heard("lunch", "lunch?", 3_000)

  private def placedWith(answer: Option[grit.core.classify.Answer]): Placed = {
    val es = Stitching.offer(
      heard("real", "real?", 0),
      room(),
      Vector(lunch, engine),
      Vector.empty,
      Vector(
        grit.core.store.EntrySearch
          .Hit(
            lunch.entry.id,
            grit.core.id.TurnRef(lunch.conversation, grit.core.id.TurnSeq.First),
            2.5
          )
      ),
      Vector.empty,
      grit.core.place.Scope.Room,
      Tuning.Default.copy(recent = 1, lexical = 1)
    )
    Stitching
      .place(
        new Scripted(answer),
        heard("real", "real?", 0),
        "David",
        es,
        Speakers.none,
        Tuning.Default.copy(recent = 1, lexical = 3)
      )
      .getOrElse(throw new java.lang.AssertionError("not asked"))
  }

  val tests = Tests {
    test("follows, begins and unread read back as written, what was seen included") {
      val follows = placedWith(Some(chose("exchange 1", "exchange 1" -> 0.8, "exchange 2" -> 0.2)))
      val begins =
        placedWith(Some(chose(Stitching.NewKey, Stitching.NewKey -> 0.9, "exchange 1" -> 0.1)))
      val unread = placedWith(None)
      Vector(follows, begins, unread).map(p => StitchJson.read(StitchJson.write(p))) ==>
        Vector(Right(follows), Right(begins), Right(unread))
    }

    test("the stored kinds are follows, begins and unread; an offer's why is recent or lexical") {
      // Stored names: rows and journals written under them must keep reading.
      val follows = placedWith(Some(chose("exchange 1", "exchange 1" -> 0.8, "exchange 2" -> 0.2)))
      val json = StitchJson.write(follows)
      json("kind").str ==> "follows"
      json("seen")("offered").arr.map(o => o("why")("kind").str).toVector ==>
        Vector("recent", "lexical")
      json("seen")("tuning")("horizon_seconds").num ==> 604_800.0
      StitchJson.kindOf(placedWith(None)) ==> "unread"
      StitchJson.read(ujson.Obj("kind" -> "maybe", "seen" -> json("seen"))) ==>
        Left("placed: no kind maybe")
    }
  }
}
