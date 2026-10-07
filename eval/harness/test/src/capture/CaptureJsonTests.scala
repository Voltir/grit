package grit.eval.harness.capture

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, QuestionName, WorkflowId}
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.stitch.{Offered, Tuning}
import grit.core.triage.Kind
import grit.dbos.engine.{Build, Reader}

import utest.*

/** A capture's files read back as written. Every id and digest here is synthetic. */
object CaptureJsonTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)
  private def p(d: Double): Probability = Probability.clamped(d)
  private val d1 = Digest.text("one")
  private val d2 = Digest.text("two")

  private val odd = Tuning(36.hours, 3, 1, p(0.55), Tokens(900), 640)

  /** Every optional part present, every choice its richer case. */
  private val full = Case(
    id("C1/1727000000.000200"),
    EntryId("in:conv-a:1727000000.000200"),
    ConversationId("conv-a"),
    Instant.parse("2026-09-30T10:15:30.123456Z"),
    Triaged(
      WorkflowId("triage:conv-a:1:0"),
      Some(Reader.Recorded("SUCCESS", "turn-v7", Instant.parse("2026-09-30T10:15:29Z"))),
      Build.Known("0123456789abcdef0123456789abcdef01234567", true)
    ),
    Live.Weighed(
      Kind.Decision,
      p(0.71),
      p(0.2),
      p(0.93),
      p(0.05),
      "jev-1.13.0",
      Some(BigDecimal("0.0000412"))
    ),
    Some(Asked(Built(d1, d2), 140, 1999)),
    Some(d2),
    Clusters(id("C1/1727000000.000200"), id("C1/1726990000.000100")),
    Some(
      Stitched(
        Placement.Follows(id("C1/1726990000.000100"), p(0.81)),
        Vector(
          Offering(id("C1/1726990000.000100"), Offered.Recent(1), Some(p(0.81))),
          Offering(id("C1/1726900000.000900"), Offered.Lexical(3.25), None)
        ),
        Some(Built(d2, d1)),
        Vector(
          Slot(id("C1/1726990000.000100"), Offered.Recent(1)),
          Slot(id("C1/1726900000.000900"), Offered.Lexical(3.5))
        ),
        new SeenCheck.SameRootDiffers(
          Vector(id("C1/1726990000.000100")),
          Set(SeenCheck.Field.Latest, SeenCheck.Field.Record)
        ),
        1
      )
    ),
    Some(odd)
  )

  /** Every optional part absent, every choice its plainer case. */
  private val bare = Case(
    id("C2/1727000100.000300"),
    EntryId("in:conv-b:1727000100.000300"),
    ConversationId("conv-b"),
    Instant.parse("2026-09-30T11:00:00Z"),
    Triaged(WorkflowId("triage:conv-b:1:2"), None, Build.Unknown),
    Live.Unanswered(Failure.Unreadable),
    None,
    None,
    Clusters(id("C2/1727000000.000000"), id("C2/1727000000.000000")),
    None,
    None
  )

  private val manifest = Manifest(
    "grit_bort",
    "grit_eval_20261002",
    Dump(d1, Instant.parse("2026-10-02T20:00:00Z")),
    Settings(2.hours, 30.days, 90.days, 4096, 1.hour, p(0.8), 3, "room slack:T1", 2.0),
    Tuning.Default,
    2,
    Constants.Shipped,
    Build.Known("89abcdef0123456789abcdef0123456789abcdef", false),
    75,
    45
  )

  private def without(v: ujson.Value, path: String*): ujson.Value = {
    val copy = ujson.read(v.render())
    path.init.foldLeft(copy)((at, k) => at(k)).obj.remove(path.last)
    copy
  }

  val tests = Tests {
    test("a case is read back as it was written, every optional part present or absent") {
      CaptureJson.readCase(ujson.read(CaptureJson.writeCase(full).render())) ==> Right(full)
      CaptureJson.readCase(ujson.read(CaptureJson.writeCase(bare).render())) ==> Right(bare)
    }

    test("a manifest is read back as it was written") {
      CaptureJson.readManifest(ujson.read(CaptureJson.writeManifest(manifest).render())) ==>
        Right(manifest)
    }

    test("a case missing a field is refused, naming the field and where it is missing") {
      CaptureJson.readCase(without(CaptureJson.writeCase(full), "clusters")) ==>
        Left("case: no clusters")
      CaptureJson.readCase(without(CaptureJson.writeCase(full), "stitch", "drift")) ==>
        Left("stitch: no drift")
    }

    test("a manifest missing a field is refused, naming it") {
      CaptureJson.readManifest(without(CaptureJson.writeManifest(manifest), "dump", "sha256")) ==>
        Left("dump: no sha256")
    }

    test(
      "a case's tags read in every form a build wrote: v1's, a question set's under its names, and none"
    ) {
      def name(n: String) = QuestionName.read(n).fold(sys.error, identity)
      val named = Live.Named(
        VectorMap(
          name("gap") -> Answer.Choice(
            "asks",
            Vector(Answer.Weight("asks", 0.75), Answer.Weight("nothing", 0.25)),
            Answer.confidence(Vector(0.75, 0.25))
          ),
          name("source:github") -> Answer.YesNo(0.5)
        ),
        "jev-2",
        None
      )
      def tags(line: String) =
        CaptureJson
          .readCase(
            ujson.read(
              CaptureJson
                .writeCase(bare)
                .render()
                .replace(
                  "{\"unanswered\":\"unreadable\"}",
                  line
                )
            )
          )
          .map(_.tags)
      (
        CaptureJson
          .writeCase(bare.copy(tags = named))
          .render()
          .contains(
            """"tags":{"answers":[{"name":"gap","choice":"asks","weights":[{"key":"asks","p":0.75},{"key":"nothing","p":0.25}]},{"name":"source:github","yes":0.5}],"model":"jev-2","cost_usd":null}"""
          ),
        tags(
          """{"answers":[{"name":"gap","choice":"asks","weights":[{"key":"asks","p":0.75},{"key":"nothing","p":0.25}]},{"name":"source:github","yes":0.5}],"model":"jev-2","cost_usd":null}"""
        ),
        tags(
          """{"kind":"decision","kind_p":0.71,"waiting":0.2,"durable":0.93,"helps":0.05,"model":"jev-1.13.0","cost_usd":"0.0000412"}"""
        ),
        tags("""{"unanswered":"unreadable"}""")
      ) ==> (
        true,
        Right(named),
        Right(
          Live.Weighed(
            Kind.Decision,
            p(0.71),
            p(0.2),
            p(0.93),
            p(0.05),
            "jev-1.13.0",
            Some(BigDecimal("0.0000412"))
          )
        ),
        Right(Live.Unanswered(Failure.Unreadable))
      )
    }

    // The line form is what later runs read from cases.jsonl: its keys are pinned.
    test("a case's line keeps its keys in one order") {
      CaptureJson.writeCase(bare).render() ==>
        """{"id":"C2/1727000100.000300","entry":"in:conv-b:1727000100.000300","conversation":"conv-b","tagged":"2026-09-30T11:00:00Z","triage":{"workflow":"triage:conv-b:1:2","recorded":null,"build":"unknown"},"tags":{"unanswered":"unreadable"},"asked":null,"author":null,"clusters":{"conversation":"C2/1727000000.000000","exchange":"C2/1727000000.000000"},"stitch":null,"tuning":null}"""
    }
  }
}
