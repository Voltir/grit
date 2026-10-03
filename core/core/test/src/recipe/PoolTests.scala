package grit.core.recipe

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.EntryId

import utest.*

/** [[Pool]]: which candidates a pool keeps, in what order, within its budget, and how it shows
  * them.
  */
object PoolTests extends TestSuite {

  private val t = Instant.parse("2026-10-02T12:00:00Z")

  /** `text` said by `speaker` `minutes` before t, as entry `id`. */
  private def said(id: String, speaker: String, minutes: Long, text: String): Spoken =
    Spoken(EntryId(id), speaker, t.minusSeconds(minutes * 60), text)

  private val none = Candidates.none

  /** What `pool` shows of `candidates`, its sections in order. */
  private def shown(pool: Pool, candidates: Candidates) =
    Pool.show(pool, candidates, t).toVector.map((s, text) => (s.key, text))

  val tests = Tests {
    test("a message source keeps the latest `most` said in [t − within, t), shown oldest first") {
      val candidates = none.copy(said =
        Vector(
          Spoken(EntryId("after"), "Ana", t.plusMillis(1), "after t"),
          Spoken(EntryId("at"), "Ana", t, "at t"),
          said("m1", "Ben", 1, "one minute before"),
          said("m3", "Ana", 3, "three minutes before"),
          said("m5", "Cy", 5, "five minutes before"),
          Spoken(EntryId("edge"), "Ben", t.minusSeconds(600), "at the window's start"),
          Spoken(EntryId("out"), "Ben", t.minusSeconds(600).minusMillis(1), "before it")
        )
      )
      shown(Pool(Vector(Source.Channel(10.minutes, 3)), 1_000), candidates) ==> Vector(
        "nearby_in_channel" -> Vector(
          "Cy, 5 minutes before: five minutes before",
          "Ana, 3 minutes before: three minutes before",
          "Ben, 1 minute before: one minute before"
        ).mkString("\n")
      )
      shown(Pool(Vector(Source.Channel(10.minutes, 9)), 1_000), candidates) ==> Vector(
        "nearby_in_channel" -> Vector(
          "Ben, 10 minutes before: at the window's start",
          "Cy, 5 minutes before: five minutes before",
          "Ana, 3 minutes before: three minutes before",
          "Ben, 1 minute before: one minute before"
        ).mkString("\n")
      )
    }

    test("a line shows at most LineChars of a message's words") {
      val long = "y" * 300
      shown(
        Pool(Vector(Source.Author(1.hour, 1)), 1_000),
        none.copy(byAuthor = Vector(said("a", "Ana", 2, long)))
      ) ==> Vector("nearby_in_channel" -> s"Ana, 2 minutes before: ${"y" * Pool.LineChars}")
    }

    test("a unit that does not fit is skipped, and a later smaller one still kept") {
      // Latest first: a, then b (longer than the budget leaves), then c.
      val candidates = none.copy(said =
        Vector(
          said("a", "Ana", 1, "short one"),
          said("b", "Ben", 2, "x" * 100),
          said("c", "Cy", 3, "short two")
        )
      )
      val a = "Ana, 1 minute before: short one"
      val c = "Cy, 3 minutes before: short two"
      val channel = Vector(Source.Channel(1.hour, 5))
      // Each kept line costs its length and one newline.
      shown(Pool(channel, a.length + 1 + c.length + 1), candidates) ==>
        Vector("nearby_in_channel" -> s"$c\n$a")
      shown(Pool(channel, a.length + 1 + c.length), candidates) ==>
        Vector("nearby_in_channel" -> a)
    }

    test("an exchange shows its opening and record, kept whole or not at all") {
      val exchange =
        Offered(said("o", "Ben", 120, "the contract term?"), Some("term: 3 years"), false)
      val opened = "Exchange opened 2 hours before by Ben: the contract term?"
      val whole = s"$opened\n  Record: term: 3 years"
      val pool = (budget: Int) => Pool(Vector(Source.Exchanges), budget)
      shown(pool(whole.length + 1), none.copy(offered = Vector(exchange))) ==>
        Vector("exchanges_in_channel" -> whole)
      shown(pool(whole.length), none.copy(offered = Vector(exchange))) ==> Vector.empty
      shown(pool(1_000), none.copy(offered = Vector(exchange.copy(record = None)))) ==>
        Vector("exchanges_in_channel" -> opened)
    }

    test("the exchange the thread follows shows its record alone, and nothing without one") {
      val followed =
        Offered(said("o", "Ben", 120, "the contract term?"), Some("term: 3 years"), true)
      val other = Offered(said("p", "Cy", 30, "lunch?"), None, false)
      val pool = Pool(Vector(Source.Exchanges), 1_000)
      shown(pool, none.copy(offered = Vector(followed, other))) ==> Vector(
        "exchanges_in_channel" ->
          "Record of the exchange this thread continues: term: 3 years\nExchange opened 30 minutes before by Cy: lunch?"
      )
      shown(pool, none.copy(offered = Vector(followed.copy(record = None), other))) ==>
        Vector("exchanges_in_channel" -> "Exchange opened 30 minutes before by Cy: lunch?")
    }

    test("an entry two sources find is shown once, under the first; sections in its order") {
      val opening = said("o", "Ben", 20, "the contract term?")
      val candidates = none.copy(
        said = Vector(opening, said("m", "Ana", 5, "standup at 10")),
        offered = Vector(Offered(opening, None, false))
      )
      val channel = Source.Channel(1.hour, 5)
      shown(Pool(Vector(Source.Exchanges, channel), 1_000), candidates) ==> Vector(
        "exchanges_in_channel" -> "Exchange opened 20 minutes before by Ben: the contract term?",
        "nearby_in_channel" -> "Ana, 5 minutes before: standup at 10"
      )
      shown(Pool(Vector(channel, Source.Exchanges), 1_000), candidates) ==> Vector(
        "nearby_in_channel" ->
          "Ben, 20 minutes before: the contract term?\nAna, 5 minutes before: standup at 10"
      )
    }

    test("an exchange whose opening an earlier source kept shows its record line alone") {
      val opening = said("o", "Ben", 20, "the contract term?")
      val channel = Source.Channel(1.hour, 5)
      val pool = Pool(Vector(channel, Source.Exchanges), 1_000)
      val nearby = "nearby_in_channel" -> "Ben, 20 minutes before: the contract term?"
      shown(
        pool,
        none.copy(
          said = Vector(opening),
          offered = Vector(Offered(opening, Some("term: 3 years"), false))
        )
      ) ==> Vector(
        nearby,
        "exchanges_in_channel" -> "Record of the exchange Ben opened 20 minutes before: term: 3 years"
      )
      shown(
        pool,
        none.copy(said = Vector(opening), offered = Vector(Offered(opening, None, false)))
      ) ==>
        Vector(nearby)
    }
  }
}
