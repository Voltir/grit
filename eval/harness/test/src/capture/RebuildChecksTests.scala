package grit.eval.harness.capture

import java.time.Instant

import grit.core.id.ConversationId
import grit.core.stitch.{Offered, Seen, Tuning}
import grit.dbos.engine.Build

import utest.*

/** What capture checks of a rebuilt input against the live one, and which build a triage ran
  * on. Every state and id here is synthetic.
  */
object RebuildChecksTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)
  private val (a, b, c, d) = (id("C/1.1"), id("C/1.2"), id("C/1.3"), id("C/1.4"))

  private def said(from: String, text: String): ujson.Value =
    ujson.Obj("from" -> from, "text" -> text, "ago" -> "3 minutes")

  /** An exchange's state, its words named after its root, so a root's exchange looks the same
    * wherever it is offered.
    */
  private def exchange(
      root: CaseId,
      latest: Option[String] = Some("ok"),
      record: Option[String] = None
  ) =
    ujson.Obj(
      "opening" -> said("Ann", s"opening of ${root.written}"),
      "latest" -> ujson.Arr.from(latest.map(said("Bo", _))),
      "record" -> record.fold[ujson.Value](ujson.Null)(ujson.Str(_))
    )

  /** `e` in the later form, with no `latest` when it has none and no `record` when it has none. */
  private def bare(e: ujson.Obj): ujson.Obj =
    ujson.Obj.from(e.obj.filterNot((_, v) => v == ujson.Null || v == ujson.Arr()))

  private def state(message: String, exchanges: ujson.Value*): ujson.Value = ujson.Obj(
    "new_message" -> message,
    "author" -> "Cy",
    "exchanges" -> ujson.Arr.from(exchanges.zipWithIndex.map { (e, i) =>
      ujson.Obj.from(("key" -> ujson.Str(s"exchange ${i + 1}")) +: e.obj.toSeq)
    })
  )

  private def slots(s: (CaseId, Offered)*): Vector[Slot] = s.toVector.map(Slot(_, _))

  /** [[SeenCheck.compare]] of `rebuilt` against `live`, each shown with its slots. */
  private def check(
      live: ujson.Value,
      liveSlots: Vector[Slot],
      rebuilt: ujson.Value,
      rebuiltSlots: Vector[Slot],
      recent: Int
  ): SeenCheck = {
    def seen(state: ujson.Value, s: Vector[Slot]) = Seen(
      state,
      s.map(x => Seen.Offer(ConversationId(x.root.written), x.why, None)),
      Tuning.Default
    )
    SeenCheck.compare(
      seen(live, liveSlots),
      liveSlots,
      seen(rebuilt, rebuiltSlots),
      rebuiltSlots,
      recent
    )
  }

  /** Two recent slots, then one lexical, as the shipped tuning offers them. */
  private val live: Vector[Slot] =
    slots(a -> Offered.Recent(1), b -> Offered.Recent(2), c -> Offered.Lexical(3.0))
  private val liveState = state("hi", exchange(a), exchange(b), exchange(c))

  val tests = Tests {
    test("the same state offering the same roots matches, whatever the lexical scores") {
      val rescored =
        slots(a -> Offered.Recent(1), b -> Offered.Recent(2), c -> Offered.Lexical(2.0))
      check(liveState, live, liveState, rescored, 2) ==> SeenCheck.Match
    }

    test("a recent slot holding another root differs in recent, naming its rank") {
      val rebuilt = slots(d -> Offered.Recent(1), b -> Offered.Recent(2), c -> Offered.Lexical(3.0))
      check(
        liveState,
        live,
        state("hi", exchange(d), exchange(b), exchange(c)),
        rebuilt,
        2
      ) ==> new SeenCheck.RecentDiffers(Vector(1))
    }

    test("a lexical slot holding another root differs in lexical slots alone") {
      val rebuilt = slots(a -> Offered.Recent(1), b -> Offered.Recent(2), d -> Offered.Lexical(3.0))
      check(
        liveState,
        live,
        state("hi", exchange(a), exchange(b), exchange(d)),
        rebuilt,
        2
      ) ==> new SeenCheck.LexicalOnly(Set(SeenCheck.Field.Roots, SeenCheck.Field.Opening))
    }

    test("a slot ranked after the recent ones filled a lexical slot, so it may differ") {
      val filled = slots(a -> Offered.Recent(1), b -> Offered.Recent(2), c -> Offered.Recent(3))
      val other = slots(a -> Offered.Recent(1), b -> Offered.Recent(2), d -> Offered.Recent(3))
      check(
        liveState,
        filled,
        state("hi", exchange(a), exchange(b), exchange(d)),
        other,
        2
      ) ==> new SeenCheck.LexicalOnly(Set(SeenCheck.Field.Roots, SeenCheck.Field.Opening))
    }

    test("a root both sides offer, shown with other latest messages, differs as that root") {
      check(
        liveState,
        live,
        state("hi", exchange(a), exchange(b, latest = Some("rolled back")), exchange(c)),
        live,
        2
      ) ==> new SeenCheck.SameRootDiffers(Vector(b), Set(SeenCheck.Field.Latest))
    }

    test(
      "an exchange shown without latest messages or a record matches one shown with them empty, in either era's form"
    ) {
      val earlier = state("hi", exchange(a, latest = None), exchange(b), exchange(c))
      val later = state("hi", bare(exchange(a, latest = None)), bare(exchange(b)), exchange(c))
      check(earlier, live, later, live, 2) ==> SeenCheck.Match
      check(later, live, later, live, 2) ==> SeenCheck.Match
    }

    test("a root both sides offer at different places is compared as that root") {
      val was = slots(a -> Offered.Recent(1), c -> Offered.Lexical(3.0), b -> Offered.Lexical(2.0))
      val now = slots(a -> Offered.Recent(1), b -> Offered.Lexical(2.0), d -> Offered.Lexical(3.0))
      check(
        state("hi", exchange(a), exchange(c), exchange(b)),
        was,
        state("hi", exchange(a), exchange(b, record = Some("closed")), exchange(d)),
        now,
        1
      ) ==> new SeenCheck.SameRootDiffers(Vector(b), Set(SeenCheck.Field.Record))
    }

    test("the message differing comes first of every kind") {
      val rebuilt = slots(d -> Offered.Recent(1), b -> Offered.Recent(2), c -> Offered.Lexical(3.0))
      check(
        liveState,
        live,
        state("hey", exchange(d), exchange(b), exchange(c)),
        rebuilt,
        2
      ) ==> new SeenCheck.MessageDiffers(Set(SeenCheck.Field.NewMessage))
    }

    test(
      "drift counts the exchanges offered lexically on both sides further apart than the tolerance"
    ) {
      val was = slots(a -> Offered.Lexical(2.0), b -> Offered.Lexical(1.0), c -> Offered.Recent(1))
      val now = slots(
        a -> Offered.Lexical(2.0 + Stitched.Tolerance / 2),
        b -> Offered.Lexical(1.5),
        c -> Offered.Lexical(9.0)
      )
      Stitched.drift(was, now) ==> 1
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
