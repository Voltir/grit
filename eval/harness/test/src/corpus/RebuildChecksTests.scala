package grit.eval.harness.corpus

import java.time.Instant

import grit.core.id.ConversationId
import grit.core.stitch.Offered
import grit.dbos.engine.Build

import utest.*

/** What capture checks of a rebuilt input against the live one, and which build a triage ran
  * on. Every state here is synthetic.
  */
object RebuildChecksTests extends TestSuite {

  private val (a, b, c) = (ConversationId("a"), ConversationId("b"), ConversationId("c"))

  private def said(from: String, text: String): ujson.Value =
    ujson.Obj("from" -> from, "text" -> text, "ago" -> "3 minutes")

  private def exchange(
      key: String,
      opening: String,
      latest: Vector[String],
      record: Option[String]
  ) =
    ujson.Obj(
      "key" -> key,
      "opening" -> said("Ann", opening),
      "latest" -> ujson.Arr.from(latest.map(said("Bo", _))),
      "record" -> record.fold[ujson.Value](ujson.Null)(ujson.Str(_))
    )

  private def state(message: String, exchanges: ujson.Value*): ujson.Value = ujson.Obj(
    "new_message" -> message,
    "author" -> "Cy",
    "exchanges" -> ujson.Arr.from(exchanges)
  )

  private val one = exchange("exchange 1", "lunch?", Vector("sure"), None)
  private val two = exchange("exchange 2", "deploy at 3", Vector("ok", "done"), Some("deployed"))

  val tests = Tests {
    test("the same state offering the same roots matches") {
      SeenCheck.compare(
        state("hi", one, two),
        Vector(a, b),
        state("hi", one, two),
        Vector(a, b)
      ) ==>
        SeenCheck.Match
    }

    test("the seen check names each field that differs, and only those") {
      val later =
        exchange("exchange 2", "deploy at 3", Vector("ok", "rolled back"), Some("deployed"))
      SeenCheck.compare(
        state("hi", one, two),
        Vector(a, b),
        state("hey", one, later),
        Vector(a, b)
      ) ==>
        SeenCheck.of(Set(SeenCheck.Field.NewMessage, SeenCheck.Field.Latest))
    }

    test(
      "an exchange fewer differs in exchanges and roots, and the shared ones are still compared"
    ) {
      val opened = exchange("exchange 1", "lunch at noon?", Vector("sure"), None)
      SeenCheck.compare(state("hi", one, two), Vector(a, b), state("hi", opened), Vector(a)) ==>
        SeenCheck.of(Set(SeenCheck.Field.Exchanges, SeenCheck.Field.Roots, SeenCheck.Field.Opening))
    }

    test("the same exchanges at roots in another order differ in roots alone") {
      SeenCheck.compare(
        state("hi", one, two),
        Vector(a, b),
        state("hi", one, two),
        Vector(b, a)
      ) ==>
        SeenCheck.of(Set(SeenCheck.Field.Roots))
    }

    test("a record line kept on one side only differs in record") {
      val closed = exchange("exchange 1", "lunch?", Vector("sure"), Some("lunch at noon"))
      SeenCheck.compare(state("hi", one), Vector(a), state("hi", closed), Vector(a)) ==>
        SeenCheck.of(Set(SeenCheck.Field.Record))
    }

    test(
      "drift counts the exchanges offered lexically on both sides further apart than the tolerance"
    ) {
      val live =
        Vector(a -> Offered.Lexical(2.0), b -> Offered.Lexical(1.0), c -> Offered.Recent(1))
      val rebuilt = Vector(
        a -> Offered.Lexical(2.0 + Stitched.Tolerance / 2),
        b -> Offered.Lexical(1.5),
        c -> Offered.Lexical(9.0)
      )
      Stitched.drift(live, rebuilt) ==> 1
    }

    test("a triage ran on the latest build started at or before it was created") {
      val t = Instant.parse("2026-10-02T12:00:00Z")
      val (first, second) = (Build.Known("1" * 40, false), Build.Known("2" * 40, true))
      val starts = Vector(
        Build.Started(t, "m", 1, "e", first),
        Build.Started(t.plusSeconds(60), "m", 2, "e", second)
      )
      Triaged.buildAt(starts, t.plusSeconds(30)) ==> first
      Triaged.buildAt(starts, t.plusSeconds(60)) ==> second
      Triaged.buildAt(starts, t.minusSeconds(1)) ==> Build.Unknown
    }
  }
}
