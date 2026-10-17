package grit.knowledge.plugin

import java.time.{Instant, LocalTime, ZoneId}

import grit.core.document.{Document, Placement, Pointer}
import grit.core.id.{CorpusName, DocumentVersion, PluginName, ScheduleKey}
import grit.core.job.{Declared, SlotRule}
import grit.core.place.{Namespace, Place, Service}
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.{
  Cleared,
  Compartments,
  Integrity,
  Label,
  Labelled,
  Level,
  Party,
  RoomLabels,
  Visibility
}
import grit.knowledge.card.{Card, CardSummary, UnitVersion}
import grit.knowledge.drive.DriveCards
import grit.knowledge.github.GitHubCards
import grit.knowledge.sync.SyncSchedule

import utest.*

/** Which sources [[Knowledge.of]] accepts under a deployment's visibility, and the label each
  * one's sync schedules are declared at.
  */
object KnowledgeTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val name = right(PluginName.of("knowledge"))
  private val service = right(Service.of("github"))
  private val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))

  private val internal = Label.at(Level.Internal)
  private val confidential = Label.at(Level.Confidential)

  /** The plugin's card of `corpus`'s file `f1`, kept at `label`. */
  private def card(corpus: String, label: Label): Document = {
    val pointer = right(Pointer.of(right(ToolName.of("drive_read")), "file" -> ujson.Str("f1")))
    val card = Card.file(
      right(CorpusName.of(corpus)),
      "f1",
      right(UnitVersion.of("2026-01-01T00:00:00Z")),
      right(CardSummary.of("What the file holds.", Vector.empty)),
      pointer
    )
    Document(
      right(DocumentVersion.of(1).toRight("no version")),
      name,
      card.docKey,
      label,
      Integrity.Member,
      driveService.place,
      card.text,
      card.data,
      Some(pointer),
      Instant.EPOCH,
      Placement(0, Instant.EPOCH),
      None
    )
  }

  /** The source `corpus` over GitHub's service at `label`, its one schedule keyed `key`. */
  private def source(
      label: Option[Label],
      corpus: String = "repo",
      key: String = "nightly"
  ): CardSource =
    CardSource.GitHub(
      right(
        GitHubCards.of(
          Corpus(right(CorpusName.of(corpus)), "a repository", service.place, Some(service)),
          "github",
          "octocat",
          "Hello-World",
          "main",
          Vector(""),
          Set.empty,
          "files",
          10,
          label,
          Place.under(Namespace.Task, Vector.empty),
          Vector(right(SyncSchedule.of(right(ScheduleKey.of(key)), rule, 5)))
        )
      )
    )

  /** Every room labelled `otherwise`, GitHub's service cleared for `trusted`. */
  private def visibility(otherwise: Labelled, trusted: Label): Visibility =
    Visibility
      .of(
        Compartments.Shipped,
        RoomLabels.of(Vector.empty, otherwise).fold(p => sys.error(p.written), identity),
        Vector.empty,
        Vector.empty,
        Vector(Cleared(Party.Service(service), trusted))
      )
      .fold(r => sys.error(r.toString), identity)

  private val internalService = visibility(Labelled.Mapped(internal), internal)

  private val driveService = right(Service.of("drive"))

  /** The Drive source `shared` at `labels`, at most 10 cards, its one schedule `nightly`. */
  private def drive(labels: Vector[Label]): CardSource =
    CardSource.Drive(
      right(
        DriveCards.of(
          Corpus(
            right(CorpusName.of("shared")),
            "shared drives",
            driveService.place,
            Some(driveService)
          ),
          right(ToolName.of("drive_list")),
          right(ToolName.of("drive_read")),
          labels,
          10,
          Place.under(Namespace.Task, Vector.empty),
          Vector(right(SyncSchedule.of(right(ScheduleKey.of("nightly")), rule, 5)))
        )
      )
    )

  /** Every room labelled `otherwise`; Drive's and GitHub's services cleared for confidential. */
  private def driveUnder(otherwise: Label): Visibility =
    Visibility
      .of(
        Compartments.Shipped,
        RoomLabels
          .of(Vector.empty, Labelled.Mapped(otherwise))
          .fold(p => sys.error(p.written), identity),
        Vector.empty,
        Vector.empty,
        Vector(
          Cleared(Party.Service(driveService), confidential),
          Cleared(Party.Service(service), confidential)
        )
      )
      .fold(r => sys.error(r.toString), identity)

  /** Each schedule `of` declared, as its key and clearance. */
  private def declared(
      cards: Vector[CardSource],
      under: Visibility
  ): Either[String, Vector[(String, Label)]] =
    Knowledge
      .of(name, cards, under)
      .map(_.schedules.map((d: Declared[?]) => (ScheduleKey.value(d.key), d.clearance)))

  val tests = Tests {
    test("a source with no label is declared at its service place's label") {
      declared(Vector(source(None)), internalService) ==> Right(Vector(("repo-nightly", internal)))
    }

    test("a source labelled above its service is declared at its own label") {
      declared(
        Vector(source(Some(confidential))),
        visibility(Labelled.Mapped(internal), confidential)
      ) ==> Right(Vector(("repo-nightly", confidential)))
    }

    test("a source labelled below its service's place is refused, naming it") {
      declared(Vector(source(Some(Label.Public))), internalService) ==> Left(
        "the source repo is labelled [level: public], below its service github's place, " +
          "[level: internal]: its runs could not read the service"
      )
    }

    test("a source whose service is cleared for less than its label is refused, naming it") {
      declared(Vector(source(Some(confidential))), internalService) ==> Left(
        "the source repo is labelled [level: confidential], above what its service github is " +
          "cleared for, [level: internal]: its runs' calls could not send"
      )
    }

    test("a source over an unmapped service place is refused unless its label holds unmapped") {
      val unmapped = Label.Unmapped
      val under =
        visibility(Labelled.Unmapped(Label.Public), Label.at(Level.Internal).join(Label.Unmapped))
      (
        declared(Vector(source(Some(internal))), under),
        declared(Vector(source(None)), under)
      ) ==> (
        Left(
          s"the source repo is labelled [level: internal], below its service github's place, " +
            s"${Label.shown(unmapped)}: its runs could not read the service"
        ),
        Right(Vector(("repo-nightly", unmapped)))
      )
    }

    test("no source, two of one corpus, or two schedules of one key are refused") {
      (
        declared(Vector.empty, internalService),
        declared(Vector(source(None), source(None)), internalService),
        declared(
          Vector(source(None, "a_b", "c"), source(None, "a", "b-c")),
          internalService
        )
      ) ==> (
        Left("a knowledge plugin has at least one source"),
        Left("two sources are the corpus repo"),
        Left("the sources a_b and a both declare the sync schedule a-b-c")
      )
    }

    test("a Drive source declares one schedule per label, keyed by its label's written form") {
      val plugin = right(
        Knowledge.of(name, Vector(drive(Vector(confidential, internal))), driveUnder(internal))
      )
      (
        plugin.schedules.map((d: Declared[?]) => (ScheduleKey.value(d.key), d.clearance)),
        plugin.documents.map(_.terms.bound)
      ) ==> (
        Vector(
          ("shared-nightly-internal", internal),
          ("shared-nightly-confidential", confidential)
        ),
        Some(20)
      )
    }

    test("a Drive source with a label below its service's place is refused, naming the label") {
      Knowledge.of(
        name,
        Vector(drive(Vector(internal, confidential))),
        driveUnder(confidential)
      ) ==>
        Left(
          "the source shared is labelled [level: internal], below its service drive's place, " +
            "[level: confidential]: its runs could not read the service"
        )
    }

    test("a Drive source with a label its service is not cleared for is refused, naming it") {
      Knowledge.of(
        name,
        Vector(drive(Vector(internal, Label.at(Level.Restricted)))),
        driveUnder(internal)
      ) ==> Left(
        "the source shared is labelled [level: restricted], above what its service drive is " +
          "cleared for, [level: confidential]: its runs' calls could not send"
      )
    }

    test("a card at a label its source does not declare is undeclared, Drive's or GitHub's") {
      val plugin = right(
        Knowledge.of(
          name,
          Vector(drive(Vector(confidential, internal)), source(None)),
          driveUnder(internal)
        )
      )
      val restricted = Label.at(Level.Restricted)
      Vector(
        card("shared", internal),
        card("shared", restricted),
        card("repo", internal),
        card("repo", confidential)
      ).map(d => plugin.documents.exists(_.undeclared(d))) ==> Vector(false, true, false, true)
    }

    test("a card of a corpus no source declares is undeclared, and a document no card is not") {
      val plugin = right(Knowledge.of(name, Vector(source(None)), driveUnder(internal)))
      val gone = card("gone", internal)
      val noCard = gone.copy(data = ujson.Obj("note" -> "no card"))
      Vector(card("repo", internal), gone, noCard)
        .map(d => plugin.documents.exists(_.undeclared(d))) ==> Vector(false, true, false)
    }

    test("a Drive schedule's key another source's schedule has is refused") {
      Knowledge.of(
        name,
        Vector(drive(Vector(internal)), source(None, "shared_nightly", "internal")),
        driveUnder(internal)
      ) ==> Left(
        "the sources shared and shared_nightly both declare the sync schedule " +
          "shared-nightly-internal"
      )
    }
  }
}
