package grit.knowledge.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{CorpusName, PluginName, ScheduleKey}
import grit.core.job.{Declared, Job}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.visibility.{Label, Room, Visibility}
import grit.knowledge.github.GitHubCards
import grit.knowledge.sync.{Sync, SyncParams}

/** The knowledge plugin: one card per unit of each of its sources, by its job [[Sync]] on each
  * source's sync schedules (keyed `{source}-{key}`, the source's `_` written `-`, each declared
  * at its source's label). Its documents: label "knowledge: what a source holds, and how to
  * read it", weight 1, kept 30 days after a version stops being current, bounded at the sum of
  * its sources' `maxUnits`; it posts nothing.
  */
final class Knowledge private (
    val name: PluginName,
    sources: Vector[(GitHubCards, Label)],
    terms: DocumentTerms
) extends Plugin {

  val version: Int = 1

  private val sync = new Sync(sources.map(_._1))

  override val documents: Option[Documents] = {
    val kept = terms
    Some(new Documents {
      val terms: DocumentTerms = kept
      val posting: Option[DocumentPosting] = None
    })
  }

  override val jobs: Vector[Job[?]] = Vector(sync)

  override val schedules: Vector[Declared[?]] =
    sources.flatMap((cards, label) =>
      cards.syncs.flatMap(s =>
        Knowledge
          .keyOf(cards.corpus, s.key)
          .toOption
          .map(key => Declared(key, sync, s.rule, SyncParams(cards.corpus, s.perRun), label))
      )
    )
}

object Knowledge {

  /** The plugin over `cards` under the deployment's `visibility`, each source at its declared
    * label or, when it declares none, its service place's; or why not: no source; two of one
    * corpus; two sync schedules of one key; a source labelled below its service's place, so
    * that its runs could not read the service (an unmapped place is below only labels holding
    * `unmapped`); or one whose service is trusted with less than its label, so that its runs'
    * calls could not send. A service's place takes no label through grit (ADR 0033 labels
    * rooms by command in them), so its declared label is the one in force, and a plugin this
    * returns has runs that are not refused for either reason.
    */
  def of(
      name: PluginName,
      cards: Vector[CardSource],
      visibility: Visibility
  ): Either[String, Knowledge] = {
    val github = cards.map { case CardSource.GitHub(c) => c }
    val corpora = github.map(_.corpus)
    for {
      _ <- Either.cond(github.nonEmpty, (), "a knowledge plugin has at least one source")
      _ <- corpora
        .diff(corpora.distinct)
        .headOption
        .toLeft(())
        .left
        .map(c => s"two sources are the corpus ${CorpusName.value(c)}")
      sources <- github.foldLeft[Either[String, Vector[(GitHubCards, Label)]]](Right(Vector.empty))(
        (acc, c) => acc.flatMap(done => labelled(c, visibility).map(l => done :+ (c, l)))
      )
      _ <- keysOnce(github)
      kept <- terms(github.map(_.maxUnits))
    } yield new Knowledge(name, sources, kept)
  }

  /** The label `cards` is declared at under `visibility`: its own, or its service place's when
    * it declares none; or why not, as [[of]] says.
    */
  private def labelled(cards: GitHubCards, visibility: Visibility): Either[String, Label] = {
    val source = CorpusName.value(cards.corpus)
    val service = cards.service
    // As Tx.roomLabel reads the place with nothing recorded through grit and no access reported.
    val place =
      visibility.compartments.admit(visibility.rooms.label(Room(service.place, None)).label)
    val trusted = visibility.trusted(service)
    val label = cards.label.getOrElse(place)
    if (!label.dominates(place))
      Left(
        s"the source $source is labelled ${Label.shown(label)}, below its service " +
          s"${service.name}'s place, ${Label.shown(place)}: its runs could not read the service"
      )
    else if (!trusted.dominates(label))
      Left(
        s"the source $source is labelled ${Label.shown(label)}, above what its service " +
          s"${service.name} is trusted with, ${Label.shown(trusted)}: its runs' calls could not send"
      )
    else Right(label)
  }

  /** Unit when no two of `sources`' sync schedules share a key ([[keyOf]]); else the two
    * sources and the key.
    */
  private def keysOnce(sources: Vector[GitHubCards]): Either[String, Unit] =
    sources
      .flatMap(c => c.syncs.map(s => (c.corpus, keyOf(c.corpus, s.key))))
      .foldLeft[Either[String, Map[String, CorpusName]]](Right(Map.empty)) {
        case (acc, (corpus, key)) =>
          acc.flatMap(seen =>
            key.flatMap { k =>
              val written = ScheduleKey.value(k)
              seen.get(written) match {
                case Some(first) =>
                  Left(
                    s"the sources ${CorpusName.value(first)} and ${CorpusName.value(corpus)} " +
                      s"both declare the sync schedule $written"
                  )
                case None => Right(seen.updated(written, corpus))
              }
            }
          )
      }
      .map(_ => ())

  /** The documents' terms, bounded at the sum of `units`, clipped to an `Int`. */
  private def terms(units: Vector[Int]): Either[String, DocumentTerms] =
    DocLabel
      .of("knowledge: what a source holds, and how to read it")
      .flatMap(
        DocumentTerms
          .of(_, DocWeight.Unscaled, 30.days, units.map(_.toLong).sum.min(Int.MaxValue).toInt)
      )

  /** The key of `source`'s schedule `key`: `{source}-{key}`, `source`'s `_` written `-`. */
  private def keyOf(source: CorpusName, key: ScheduleKey): Either[String, ScheduleKey] =
    ScheduleKey.of(s"${CorpusName.value(source).replace('_', '-')}-${ScheduleKey.value(key)}")
}
