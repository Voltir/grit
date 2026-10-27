package grit.knowledge.declassify

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Asked, Dreaming, MoveError, ScriptedDreaming}
import grit.core.classify.Answer
import grit.core.document.{
  Declassified,
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Pointer,
  Written
}
import grit.core.files.{
  FormatName,
  InMemoryPieces,
  Kind,
  PieceKeeper,
  PiecedFile,
  Pieces,
  TestPieces
}
import grit.core.flow.TestTainted
import grit.core.id.{CorpusName, DocKey, DocumentVersionId, PluginName}
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
      DocumentTerms.of(right(DocLabel.of("document")), DocWeight.Unscaled, 30.days, 100)
    )
  private val Corpus = right(CorpusName.of("shared"))
  private val Drive = right(ToolName.of("drive_read"))

  private val Team = TestLabels.compartment("team")
  private val Over = DeclassificationPolicy.Over.Compartment(Team)

  /** What the run reads at, and its cards are kept at. */
  private val Floor = Label.at(Level.Internal, Team)

  private val Now = Instant.parse("2026-10-23T12:00:00Z")

  /** A card's file's first revision, as its [[UnitVersion]] writes it. */
  private val First = "2026-01-01T00:00:00Z"

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
    val pieces = new InMemoryPieces
    private val counts = new Counts

    /** How many rewrites and screens (a rewrite's or its source's) were asked, across runs. */
    def asked: (Int, Int) = (counts.rewrites, counts.checks)

    /** The card of the Drive file `id` at `revision`, holding `says` at [[Floor]], its file
      * [[pieced]] at that revision unless not `withPieces`: its version.
      */
    def card(
        id: String,
        says: String = "The offsite is at the lake house.",
        revision: String = First,
        withPieces: Boolean = true
    ): DocumentVersionId = {
      val c = Card.file(
        Corpus,
        id,
        right(UnitVersion.of(revision)),
        right(CardSummary.of(says, Vector.empty)),
        pointer(id),
        Some(id)
      )
      if (withPieces) pieced(id, revision)
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

    /** The Drive file `id` at `revision`, kept as the knowledge plugin's pieces at [[Floor]]. */
    def pieced(id: String, revision: String): Unit = {
      val kept = pieces
        .keeper(Knowledge, PieceKeeper.Bound)
        .keep(
          PiecedFile(
            id,
            id,
            revision,
            Kind.Pdf,
            FormatName("paged-text"),
            1,
            pointer(id),
            Pieces.Target
          ),
          Place.Everywhere,
          Integrity.Member,
          TestPieces.paragraphs(Vector("Offsite"), Vector("The offsite is at the lake house.")),
          0,
          Now
        )(using TestTx.fake(Clearance.of(Floor)))
      kept.fold(e => throw new java.lang.AssertionError(s"not pieced: $e"), _ => ())
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

    /** The `version`th policy over `team`, a revocation when `text` is `None`, with `checks` as
      * its row holds them, kept at `kept`, at `status`; and, when `benched`, a passing bench of
      * those checks.
      */
    def policy(
        version: Int,
        text: Option[String] = Some("Remove where the offsite is."),
        checks: Option[ujson.Value] = Some(InMemoryDocuments.Checks),
        benched: Boolean = true,
        kept: Label = Floor,
        status: InMemoryDocuments.Status = InMemoryDocuments.Status.Withdraw
    ): Unit =
      documents.policy(Over, version, text, kept, checks, benched, status)

    /** The `version`th policy published at `status`, as the operator's publish does. */
    def publish(version: Int, status: InMemoryDocuments.Status): Unit =
      documents.publish(Over, version, status)

    /** The policy version each current rewrite was made under, as its data names it. */
    def made: Vector[Int] =
      documents
        .keeper(Plugin, Terms)
        .newest(Int.MaxValue)(using TestTx.fake(Clearance.of(Floor)))
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
        .filterNot(_.key == Withheld.Key)
        .flatMap(d => Declassified.Made.read(d.data).map(_.policy))

    /** The `version`th application to the card of `id`, a removal when `applied` is false. */
    def apply(id: String, version: Int, applied: Boolean = true): Unit =
      documents.application(pointer(id), version, Option.when(applied)(Over))

    /** One run with `perRun`, its rewrites answered "The team meets offsite." and every question
      * of each screen, a rewrite's or a source's, `leak`; or, given `screens`, the `i`th screen
      * across runs answering `screens(i)` in order, cycling: its reply.
      */
    def run(
        perRun: Int = 5,
        leak: Double = 0.0,
        screens: Vector[Vector[Double]] = Vector()
    ): String = {
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
        Dreaming.Reads(world.shelf, _ => needs, world.excerpts, documents, pieces),
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
        request => {
          val at = counts.checks
          counts.check()
          val answers =
            if (screens.isEmpty) Vector.fill(request.questions.size)(Answer.YesNo(leak))
            else screens(at % screens.size).map(Answer.YesNo(_))
          Right(Asked(TestTainted.at(answers, Integrity.Member), Floor))
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

    /** The plugin's own document `key`, holding `text` and `data`, written at [[Floor]] as an
      * earlier build of it might have.
      */
    def own(key: String, text: String, data: ujson.Value): Unit = {
      val written = documents
        .keeper(Plugin, Terms)
        .write(
          right(DocKey.of(key)),
          Floor,
          Integrity.Authority,
          Place.Everywhere,
          right(DocText.of(text)),
          data,
          None,
          Now
        )(using TestTx.fake(Clearance.of(Floor)))
      written.fold(e => throw new java.lang.AssertionError(s"$e"), _ => ())
    }

    /** The entries the withholding at [[Floor]] holds as written, read or not. */
    def withholding: Vector[ujson.Value] =
      documents
        .keeper(Plugin, Terms)
        .current(Withheld.Key, Floor)(using TestTx.fake(Clearance.of(Floor)))
        .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
        .flatMap(_.data.objOpt.flatMap(_.get("withheld")).flatMap(_.arrOpt))
        .fold(Vector.empty)(_.toVector)
  }

  /** How many rewrites and screens a world's runs asked. */
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
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        Vector(("internal", "The team meets offsite.")),
        (1, 1)
      )
    }

    test(
      "an unpieced card is tallied unpieced, not lapsed, and rewritten once its file is pieced"
    ) {
      val runs = new Runs
      val _ = runs.card("c1", withPieces = false)
      runs.policy(1)
      runs.apply("c1", 1)
      val unpieced = runs.run()
      val asked = runs.asked
      runs.pieced("c1", First)
      (unpieced, asked, runs.run(), runs.kept) ==> (
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 1 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        (0, 0),
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        Vector(("internal", "The team meets offsite."))
      )
    }

    test(
      "a card is rewritten once per card version, revision, policy version and application version"
    ) {
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
      val _ = runs.card("c1", "The offsite moved to the city.", revision = "2026-02-01T00:00:00Z")
      val revised = runs.run()
      val withheld = runs.run(leak = 0.9)
      (Vector(first, again, policed, reapplied, changed, revised), runs.asked) ==> (
        Vector(
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
          "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run"
        ),
        (5, 5)
      )
      withheld ==>
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run"
    }

    test(
      "a card whose policy version in force has checks that do not read is tallied unchecked, and asked nothing"
    ) {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1, checks = Some(ujson.Obj("signals" -> ujson.Arr())))
      runs.apply("c1", 1)
      (runs.run(), runs.kept, runs.asked) ==> (
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 1 unchecked, 0 withdrawn, 0 left for the next run; policy v1 unchecked: checks: not of their shape",
        Vector(),
        (0, 0)
      )
    }

    test(
      "the version in force is benched before any card, and a card rewritten under it in the same run; while its checks stand, it is not benched again"
    ) {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1, benched = false)
      runs.apply("c1", 1)
      // The accept example screens clean, the refuse example is refused, then the rewrite's
      // screen (the floor and the one signal) is clean.
      val first = runs.run(screens = Vector(Vector(0.1), Vector(0.9), Vector(0.0, 0.0, 0.0)))
      val _ = runs.card("c2")
      runs.apply("c2", 1)
      val second = runs.run()
      (first, second, runs.kept.size, runs.asked) ==> (
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run; policy v1 benched",
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        2,
        (2, 4)
      )
    }

    test(
      "a failing bench is tallied by its first failing example and that example's deciding signal, and nothing is rewritten under it"
    ) {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1, benched = false)
      runs.apply("c1", 1)
      (runs.run(screens = Vector(Vector(0.63), Vector(0.9))), runs.kept, runs.asked) ==> (
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 1 unchecked, 0 withdrawn, 0 left for the next run; policy v1 unchecked: accept example 1 refused by signal 1 (0.63)",
        Vector(),
        (0, 2)
      )
    }

    test("a version whose checks the run cannot read is tallied not read, and asked nothing") {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1, benched = false, kept = Label.at(Level.Confidential, Team))
      runs.apply("c1", 1)
      (runs.run(), runs.asked) ==> (
        "declassified cards: 0 written, 0 withheld, 1 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run; policy v1 not read",
        (0, 0)
      )
    }

    test("a screen that keeps a rewrite back is withheld, and not asked again at those versions") {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.policy(1)
      runs.apply("c1", 1)
      // Its floor fires too, so its source's floor is asked: two screens, once.
      (runs.run(leak = 0.5), runs.run(leak = 0.5), runs.kept, runs.asked) ==> (
        "declassified cards: 0 written, 1 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        Vector(),
        (1, 2)
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
        "declassified cards: 3 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 2 withdrawn, 0 left for the next run",
        "declassified cards: 0 written, 1 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
        Vector()
      )
    }

    test(
      "withdraw: a draft changes nothing, and once it is published to withdraw, the earlier version's rewrite is withdrawn at the next scan and the card rewritten under it"
    ) {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.apply("c1", 1)
      runs.policy(1)
      val _ = runs.run()
      runs.policy(2, status = InMemoryDocuments.Status.Draft)
      val drafted = (runs.run(), runs.made)
      runs.publish(2, InMemoryDocuments.Status.Withdraw)
      (drafted, runs.run(), runs.made) ==> (
        (
          "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
          Vector(1)
        ),
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
        Vector(2)
      )
    }

    test(
      "keep: an earlier version's rewrite stands once a version is published to keep, through a run that rewrites nothing, until that version rewrites its card (replacing it) or withholds it (withdrawn at the next scan)"
    ) {
      val runs = new Runs
      Vector("a", "b").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      val _ = runs.run()
      runs.policy(
        2,
        checks = Some(ujson.Obj("signals" -> ujson.Arr())),
        status = InMemoryDocuments.Status.Keep
      )
      val unchecked = (runs.run(), runs.made)
      runs.policy(2, status = InMemoryDocuments.Status.Keep)
      val replaced = (runs.run(perRun = 1), runs.made)
      val withheld = (runs.run(leak = 0.9), runs.made)
      (unchecked, replaced, withheld, runs.run(), runs.made) ==> (
        (
          "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 2 unchecked, 0 withdrawn, 0 left for the next run; policy v2 unchecked: checks: not of their shape",
          Vector(1, 1)
        ),
        (
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 1 left for the next run",
          Vector(2, 1)
        ),
        (
          "declassified cards: 0 written, 1 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
          Vector(2, 1)
        ),
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
        Vector(2)
      )
    }

    test(
      "a keep after a withdraw keeps the rewrites from the withdraw's version on, and none before it"
    ) {
      val runs = new Runs
      Vector("a", "b").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      val _ = runs.run()
      runs.policy(2)
      runs.policy(
        3,
        checks = Some(ujson.Obj("signals" -> ujson.Arr())),
        status = InMemoryDocuments.Status.Keep
      )
      (runs.run(), runs.made) ==> (
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 2 unchecked, 2 withdrawn, 0 left for the next run; policy v3 unchecked: checks: not of their shape",
        Vector()
      )
    }

    test(
      "a revocation withdraws every rewrite, though the versions before it were published to keep"
    ) {
      val runs = new Runs
      val _ = runs.card("c1")
      runs.apply("c1", 1)
      runs.policy(1, status = InMemoryDocuments.Status.Keep)
      runs.policy(2, status = InMemoryDocuments.Status.Keep)
      val _ = runs.run()
      runs.policy(3, text = None)
      (runs.run(), runs.made) ==> (
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
        Vector()
      )
    }

    test(
      "as a deployment's rows stand after the ALTER, v1 and v2 published to withdraw and v2's rewrites kept: while v2 is in force with NULL checks nothing is rewritten and its rewrites stand; once v3, benched as a draft, is published to keep, they stand through runs whose declassify is unapplied or fails, until v3 rewrites a card (replaced) or withholds it (withdrawn)"
    ) {
      val runs = new Runs
      Vector("a", "b").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      runs.policy(2)
      val _ = runs.run()
      runs.policy(2, checks = None)
      val altered = (runs.run(), runs.made)
      runs.policy(3, status = InMemoryDocuments.Status.Draft)
      val drafted = (runs.run(), runs.made)
      runs.publish(3, InMemoryDocuments.Status.Keep)
      runs.documents.unbench(Over, 3)
      val failedBench = (runs.run(leak = 0.9), runs.made)
      runs.policy(3, status = InMemoryDocuments.Status.Keep)
      val failed = (runs.run(screens = Vector(Vector())), runs.made)
      val replaced = (runs.run(perRun = 1), runs.made)
      val withheld = (runs.run(leak = 0.9), runs.made)
      val stood =
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run; policy v2 unchecked: no checks"
      val unread = "the classifier's answers do not read: 0 answers to 3 questions"
      (altered, drafted, failedBench, failed, replaced, withheld, runs.run(), runs.made) ==> (
        (stood, Vector(2, 2)),
        (stood, Vector(2, 2)),
        (
          "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 2 unchecked, 0 withdrawn, 0 left for the next run; policy v3 unchecked: accept example 1 refused by signal 1 (0.90)",
          Vector(2, 2)
        ),
        (
          s"declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 2 left for the next run; not written: 2 ($unread), 1 ($unread)",
          Vector(2, 2)
        ),
        (
          "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 1 left for the next run",
          Vector(3, 2)
        ),
        (
          "declassified cards: 0 written, 1 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
          Vector(3, 2)
        ),
        "declassified cards: 0 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 1 withdrawn, 0 left for the next run",
        Vector(3)
      )
    }

    test(
      "a rewrite with no revision in its data, or any own document but the withholding that reads as no current rewrite, is withdrawn in the run's scan and its card rewritten; a withholding entry with no revision is forgotten"
    ) {
      val runs = new Runs
      val v = runs.card("c1")
      runs.policy(1)
      runs.apply("c1", 1)
      // As an earlier build kept them: a rewrite of the card's own text, and its withholding.
      val unrevised = ujson.Obj(
        "source" -> ujson.Num(DocumentVersionId.value(v).toDouble),
        "over" -> ujson.Str(Over.written),
        "policy" -> ujson.Num(1),
        "application" -> ujson.Num(1)
      )
      runs.own("knowledge:old", "The team meets.", unrevised)
      runs.own("stray", "Nothing a rewrite names.", ujson.Obj())
      runs.own(DocKey.value(Withheld.Key), "…", ujson.Obj("withheld" -> ujson.Arr(unrevised)))
      (runs.run(), runs.kept, runs.withholding) ==> (
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 2 withdrawn, 0 left for the next run",
        Vector(("internal", "The team meets offsite.")),
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
        "declassified cards: 2 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 1 left for the next run",
        "declassified cards: 1 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        3
      )
    }

    test(
      "a run of its most cards, each retried after its source was asked, is not cut short by its limits"
    ) {
      val runs = new Runs
      Vector("a", "b", "c", "d", "e").foreach { id =>
        val _ = runs.card(id)
        runs.apply(id, 1)
      }
      runs.policy(1)
      // Each card: `meta` fires, its source's floor does not excuse it, and the retry passes.
      val screens = Vector(Vector(0.5, 0.0, 0.0), Vector(0.0, 0.0), Vector(0.0, 0.0, 0.0))
      (runs.run(screens = screens), runs.asked) ==> (
        "declassified cards: 5 written, 0 withheld, 0 unapplied, 0 unpieced, 0 unchecked, 0 withdrawn, 0 left for the next run",
        (10, 15)
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
