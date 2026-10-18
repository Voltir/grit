package grit.knowledge.plugin

import java.time.{LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, Document, DocumentTerms}
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Declared, Job, SlotRule}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.visibility.{Label, Visibility}
import grit.knowledge.strand.{DreamParams, StrandDream}

/** The strands plugin: one document per strand of its rooms, by its job [[StrandDream]], daily at
  * `at` in `zone`, on a schedule per label in `labels`, keyed `strands-dream-{label}` with the
  * label's written form, `+` written `-`, and declared at that label, each run summarising at
  * most [[StrandDocuments.PerRun]] strands spoken in within [[StrandDocuments.Reach]] or
  * holding a document; a room at a label not among them gets none. Its documents: label "room
  * discussion", weight 1, kept 30 days after a version stops being current, bounded at
  * [[StrandDocuments.Bound]] times its labels; at each start, a document at a label not among
  * them is withdrawn ([[Documents.sweep]]); it posts nothing.
  */
final class StrandDocuments private (
    val name: PluginName,
    keyed: Vector[(ScheduleKey, Label)],
    rule: SlotRule,
    terms: DocumentTerms
) extends Plugin {

  private val labels: Vector[Label] = keyed.map(_._2)

  val version: Int = 1

  private val dream = new StrandDream(StrandDocuments.PerRun, labels)

  override val documents: Option[Documents] = {
    val kept = terms
    Some(new Documents {
      val terms: DocumentTerms = kept
      val posting: Option[DocumentPosting] = None

      /** A document at a label no schedule is declared at: no run reads it as its own. */
      override def undeclared(document: Document): Boolean = !labels.contains(document.label)
    })
  }

  override val jobs: Vector[Job[?]] = Vector(dream)

  override val schedules: Vector[Declared[?]] =
    keyed.map((key, l) =>
      Declared(key, dream, rule, DreamParams(StrandDocuments.PerRun, StrandDocuments.Reach), l)
    )
}

object StrandDocuments {

  /** 200: the most current documents one label's runs keep. */
  val Bound: Int = 200

  /** 5: the most strands one run summarises. */
  val PerRun: Int = 5

  /** 2 days: how recently a strand with no document must have been spoken in to be looked at.
    */
  val Reach: FiniteDuration = 2.days

  /** The plugin at `labels`, its runs daily `at` in `zone`, under the deployment's
    * `visibility`; or why not: no label, one given twice, one holding a compartment
    * `visibility` does not declare, or one whose written form makes no schedule key.
    */
  def of(
      name: PluginName,
      labels: Vector[Label],
      at: LocalTime,
      zone: ZoneId,
      visibility: Visibility
  ): Either[String, StrandDocuments] =
    for {
      _ <- Either.cond(labels.nonEmpty, (), "the strands plugin is declared at a label at least")
      _ <- labels
        .diff(labels.distinct)
        .headOption
        .toLeft(())
        .left
        .map(l => s"the strands plugin is declared at ${Label.written(l)} twice")
      _ <- labels
        .flatMap(l => visibility.compartments.undeclared(l).map(_ => l))
        .headOption
        .toLeft(())
        .left
        .map(l =>
          s"the strands plugin's label ${Label.written(l)} holds a compartment not declared"
        )
      keyed <- labels.foldLeft[Either[String, Vector[(ScheduleKey, Label)]]](Right(Vector()))(
        (acc, l) =>
          acc.flatMap(done =>
            ScheduleKey
              .of(s"strands-dream-${Knowledge.slug(l)}")
              .left
              .map(why => s"the strands plugin's label ${Label.written(l)} makes no key: $why")
              .map(k => done :+ (k, l))
          )
      )
      terms <- DocLabel
        .of("room discussion")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, Bound * labels.size))
    } yield new StrandDocuments(name, keyed, SlotRule.Daily(at, zone), terms)
}
