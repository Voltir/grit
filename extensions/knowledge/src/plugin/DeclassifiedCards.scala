package grit.knowledge.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Declared, Interval, Job, SlotRule}
import grit.core.plugin.{DocumentPosting, Documents, Exports, Plugin}
import grit.core.visibility.{Label, Visibility}
import grit.knowledge.card.CardShelf
import grit.knowledge.declassify.{CardDeclassify, DeclassifyParams}

/** The declassified-cards plugin: for each card of `cards` with an application in force, one
  * document holding its rewrite under that application's policy, below the card's label by the
  * policy's compartment, by its job [[CardDeclassify]] on one schedule at `at` by `rule`, keyed
  * `cards-declassify-{label}` with the label's written form, `+` written `-`, whose runs read
  * only what `at` dominates and rewrite at most [[DeclassifiedCards.PerRun]] cards each. A
  * rewrite is withdrawn at the first run after its card changes or goes, or its policy or
  * application is superseded. Its documents: label "declassified card", weight 1, kept 30 days
  * after a version stops being current, bounded at [[DeclassifiedCards.Bound]]; it posts
  * nothing.
  */
final class DeclassifiedCards private (
    val name: PluginName,
    key: ScheduleKey,
    at: Label,
    rule: SlotRule,
    kept: DocumentTerms,
    cards: Exports[CardShelf]
) extends Plugin {

  val version: Int = 1

  private val job = new CardDeclassify(DeclassifiedCards.PerRun, cards)

  override val needs: Vector[Exports[?]] = Vector(cards)

  override val documents: Option[Documents] = Some(new Documents {
    val terms: DocumentTerms = kept
    val posting: Option[DocumentPosting] = None
  })

  override val jobs: Vector[Job[?]] = Vector(job)

  override val schedules: Vector[Declared[?]] =
    Vector(Declared(key, job, rule, DeclassifyParams(DeclassifiedCards.PerRun), at))
}

object DeclassifiedCards {

  /** 500: the most current documents its runs keep. */
  val Bound: Int = 500

  /** 5: the most cards one run rewrites. */
  val PerRun: Int = 5

  /** Every half hour: its runs' rule unless a deployment gives one. */
  val Cadence: SlotRule = SlotRule.Every(Interval.HalfHour)

  /** The plugin over `cards`' cards, its runs at `at` by `rule`, under the deployment's
    * `visibility`; or why not: `at` holding a compartment `visibility` does not declare, or the
    * unmapped mark, or a written form that makes no schedule key.
    */
  def of(
      name: PluginName,
      cards: Exports[CardShelf],
      at: Label,
      visibility: Visibility,
      rule: SlotRule = Cadence
  ): Either[String, DeclassifiedCards] =
    for {
      _ <- Either.cond(
        !at.dominates(Label.Unmapped),
        (),
        "the declassified cards plugin is declared at a label holding what no one labelled"
      )
      keyed <- DreamSchedules.keyed(
        "declassified cards",
        "cards-declassify",
        Vector(at),
        visibility
      )
      key <- keyed.headOption.map(_._1).toRight("the declassified cards plugin has no schedule")
      terms <- DocLabel
        .of("declassified card")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, Bound))
    } yield new DeclassifiedCards(name, key, at, rule, terms, cards)
}
