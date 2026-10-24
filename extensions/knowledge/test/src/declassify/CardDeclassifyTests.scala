package grit.knowledge.declassify

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Asked, Dreaming, MoveError, ScriptedDreaming}
import grit.core.classify.Answer
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Pointer,
  Written
}
import grit.core.flow.TestTainted
import grit.core.id.{CorpusName, DocumentVersionId, PluginName}
import grit.core.job.JobRun
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.need.{Needs, PluginReads}
import grit.core.place.Place
import grit.core.plugin.{Exports, InMemoryPlugins}
import grit.core.stitch.StrandWorld
import grit.core.tool.ToolName
import grit.core.visibility.{
  Clearance,
  DeclassificationPolicy,
  Integrity,
  Label,
  Level,
  Recorded,
  TestLabels
}
import grit.dbos.sql.TestTx
import grit.knowledge.card.{Card, CardShelf, CardSummary, UnitVersion}

import utest.*

/** The declassify job's runs over cards in memory, their moves made by [[ScriptedDreaming]] over
  * one document store, so a later run reads what an earlier one kept and the operator's rows
  * written between them.
  */
object CardDeclassifyTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Knowledge = right(PluginName.of("knowledge"))
  private val Plugin = right(PluginName.of("declassified"))
  private val CardTerms =
    right(DocumentTerms.of(right(DocLabel.of("knowledge card")), DocWeight.Unscaled, 30.days, 100))
  private val Terms =
    right(
      DocumentTerms.of(right(DocLabel.of("declassified card")), DocWeight.Unscaled, 30.days, 100)
    )
  private val Corpus = right(CorpusName.of("shared"))
  private val Drive = right(ToolName.of("drive_read"))

  private val Team = TestLabels.compartment("team")
  private val Over = DeclassificationPolicy.Over.Compartment(Team)

  /** What the run reads at, and its cards are kept at. */
  private val Floor = Label.at(Level.Internal, Team)

  private val Now = Instant.parse("2026-10-23T12:00:00Z")

  /** The knowledge plugin's export, over the real [[CardShelf]]. */
  private object Cards extends Exports[CardShelf] {
    val name: PluginName = Knowledge
    val version: Int = 1
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  private def pointer(id: String): Pointer = right(Pointer.of(Drive, "file" -> ujson.Str(id)))

  /** A world of cards and the declassify plugin's documents, run run by run. */
  private final class Runs {
    val documents = new InMemoryDocuments
    private val counts = new Counts

    /** How many rewrites and checks were asked, across runs. */
    def asked: (Int, Int) = (counts.rewrites, counts.checks)

    /** The card of the Drive file `id`, holding `says` at [[Floor]]: its version. */
    def card(id: String, says: String = "The offsite is at the lake house."): DocumentVersionId = {
      val c = Card.file(
        Corpus,
        id,
        right(UnitVersion.of("2026-01-01T00:00:00Z")),
        right(CardSummary.of(says, Vector.empty)),
        pointer(id),
        Some(id)
      )
      documents
        .keeper(Knowledge, CardTerms)
        .write(
          c.docKey,
          Floor,
          Integrity.Member,
          Place.Everywhere,
          c.text,
          c.data,
          Some(c.pointer),
          Now
        )(using
          TestTx.fake(Clearance.of(Floor))
        ) match {
        case Right(Written.Versioned(v, _, _)) => v
        case other => throw new java.lang.AssertionError(other.toString)
      }
    }

    /** The card of `id` withdrawn. */
    def gone(id: String): Unit = {
      val c = Card.file(
        Corpus,
        id,
        right(UnitVersion.of("2026-01-01T00:00:00Z")),
        right(CardSummary.of("x", Vector.empty)),
        pointer(id),
        Some(id)
      )
      val _ = documents
        .keeper(Knowledge, CardTerms)
        .withdraw(c.docKey, Floor, Now)(using TestTx.fake(Clearance.of(Floor)))
    }

    /** The `version`th policy over `team`, a revocation when `text` is `None`. */
    def policy(version: Int, text: Option[String] = Some("Remove where the offsite is.")): Unit =
      documents.policy(Over, version, text, Floor)

    /** The `version`th application to the card of `id`, a removal when `applied` is false. */
    def apply(id: String, version: Int, applied: Boolean = true): Unit =
      documents.application(pointer(id), version, Option.when(applied)(Over))

    /** One run with `perRun`, its rewrites answered "The team meets offsite." and each check's
      * three questions `leak`: its reply.
      */
    def run(perRun: Int = 5, leak: Double = 0.0): String = {
      val job = new CardDeclassify(5, Cards)
      val world = new StrandWorld
      val needs = Needs.over(
        Plugin,
        Vector(
          Knowledge -> PluginReads(
            new InMemoryPlugins().docs(Knowledge),
            documents.shelf(Knowledge)
          )
        )
      )
      val reply: Message.Assistant = Message.Assistant(
        Vector(AssistantBlock.Text("The team meets offsite.")),
        StopReason.EndTurn,
        Usage.Zero,
        "m"
      )
      val moves = new ScriptedDreaming(
        job.limits,
        Floor,
        Plugin,
        Terms,
        Dreaming.Reads(world.shelf, _ => needs, world.excerpts, documents),
        (_, _) => Left(MoveError.Model("no JSON reply scripted")),
        // Positional: a named argument lifts the capturing script into a val it cannot flow into.
        documents,
        Now,
        Integrity.Authority,
        Recorded.Empty,
        _ => {
          counts.rewrite()
          Right(Asked(TestTainted.at(reply, Integrity.Member), Floor))
        },
        _ => {
          counts.check()
          Right(Asked(TestTainted.at(Vector.fill(3)(Answer.YesNo(leak)), Integrity.Member), Floor))
        }
      )
      job.run(TestTainted.trusted(JobRun(DeclassifyParams(perRun), Now, Now)), moves)
    }

    /** The plugin's current documents a run at [[Floor]] reads: each label and text. */
    def kept: Vector[(String, String)] =
      documents
        .keeper(Plugin, Terms)
        .newest(Int.MaxValue)(using TestTx.fake(Clearance.of(Floor)))
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
        .filterNot(_.key == Withheld.Key)
        .map(d => (Label.written(d.label), DocText.value(d.text)))
  }

  /** How many rewrites and checks a world's runs asked. */
  private final class Counts {
    // Counted by the scripts; one test's, read on its one thread.
    @caps.unsafe.untrackedCaptures
    var rewrites = 0
    @caps.unsafe.untrackedCaptures
    var checks = 0
    def rewrite(): Unit = rewrites += 1
    def check(): Unit = checks += 1
  }

  val tests = Tests {
    test("only an applied card is rewritten") {
      val runs = new Runs
      val _ = runs.card("applied")
      val _ = runs.card("other")
      runs.policy(1)
      runs.apply("applied", 1)
      (runs.run(), runs.kept, runs.asked) ==> (
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
        Vector(("internal", "The team meets offsite.")),
        (1, 1)
      )
    }

    test("a card is rewritten once per card version, policy version and application version") {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1)
      runs.apply("c1", 1)
      val first = runs.run()
      val again = runs.run()
      runs.policy(2)
      val policed = runs.run()
      runs.apply("c1", 2)
      val reapplied = runs.run()
      val _ = runs.card("c1", "The offsite moved to the city.")
      val changed = runs.run()
      val withheld = runs.run(leak = 0.9)
      (Vector(first, again, policed, reapplied, changed), runs.asked) ==> (
        Vector(
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
          "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 1 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 1 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 1 withdrawn, 0 left for the next run"
        ),
        (4, 4)
      )
      withheld ==>
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run"
    }

    test("a check that keeps a rewrite back is withheld, and not asked again at those versions") {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1)
      runs.apply("c1", 1)
      (runs.run(leak = 0.5), runs.run(leak = 0.5), runs.kept, runs.asked) ==> (
        "declassified cards: 0 written, 1 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
        Vector(),
        (1, 1)
      )
    }

    test(
      "a rewrite whose card is gone, or whose application or policy is no longer current, is withdrawn in the run's scan"
    ) {
      val runs = new Runs
      Vector("a", "b", "c").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      val made = runs.run()
      runs.gone("a")
      runs.apply("b", 2, applied = false)
      val lapsed = runs.run()
      // A new policy version whose check keeps the rewrite back: nothing replaces the old one.
      runs.policy(2)
      val superseded = runs.run(leak = 0.9)
      (made, lapsed, superseded, runs.kept) ==> (
        "declassified cards: 3 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 2 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 1 withheld, 0 unapplied, 1 withdrawn, 0 left for the next run",
        Vector()
      )
    }

    test("at most perRun rewrites a run, the rest left for the next") {
      val runs = new Runs
      Vector("a", "b", "c").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      (runs.run(perRun = 2), runs.run(perRun = 2), runs.kept.size) ==> (
        "declassified cards: 2 written, 0 withheld, 0 unapplied, 0 withdrawn, 1 left for the next run",
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 withdrawn, 0 left for the next run",
        3
      )
    }

    test("its parameters read back as written; a perRun under 1, or not whole, does not read") {
      val job = new CardDeclassify(5, Cards)
      (
        job.read(job.write(DeclassifyParams(3))),
        job.read(ujson.Obj("perRun" -> 0)),
        job.read(ujson.Obj("perRun" -> 1.5))
      ) ==> (
        Right(DeclassifyParams(3)),
        Left("a declassify's parameters hold perRun, a whole number at least 1: {\"perRun\":0}"),
        Left("a declassify's parameters hold perRun, a whole number at least 1: {\"perRun\":1.5}")
      )
    }
  }
}
