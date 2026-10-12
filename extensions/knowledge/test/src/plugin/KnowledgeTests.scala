package grit.knowledge.plugin

import java.time.{LocalTime, ZoneId}

import grit.core.id.{CorpusName, PluginName, ScheduleKey}
import grit.core.job.{Declared, SlotRule}
import grit.core.place.{Namespace, Place, Service}
import grit.core.triage.Corpus
import grit.core.visibility.{
  Compartment,
  Compartments,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Trust,
  Visibility
}
import grit.knowledge.github.{GitHubCards, SyncSchedule}

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
          10,
          label,
          Place.under(Namespace.Task, Vector.empty),
          Vector(right(SyncSchedule.of(right(ScheduleKey.of(key)), rule, 5)))
        )
      )
    )

  /** Every room labelled `otherwise`, GitHub's service trusted with `trusted`. */
  private def visibility(otherwise: Labelled, trusted: Label): Visibility =
    Visibility
      .of(
        Compartments.Shipped,
        RoomLabels.of(Vector.empty, otherwise).fold(p => sys.error(p.written), identity),
        Vector.empty,
        Vector.empty,
        Vector(Trust(service, trusted))
      )
      .fold(r => sys.error(r.toString), identity)

  private val internalService = visibility(Labelled.Mapped(internal), internal)

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

    test("a source whose service is trusted with less than its label is refused, naming it") {
      declared(Vector(source(Some(confidential))), internalService) ==> Left(
        "the source repo is labelled [level: confidential], above what its service github is " +
          "trusted with, [level: internal]: its runs' calls could not send"
      )
    }

    test("a source over an unmapped service place is refused unless its label holds unmapped") {
      val unmapped = Label.at(Level.Public, Compartment.Unmapped)
      val under =
        visibility(Labelled.Unmapped(Label.Public), Label.at(Level.Internal, Compartment.Unmapped))
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
  }
}
