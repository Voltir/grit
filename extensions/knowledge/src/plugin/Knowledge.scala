package grit.knowledge.plugin

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms}
import grit.core.id.{CorpusName, PluginName, ScheduleKey}
import grit.core.job.{Declared, Job}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.visibility.{Label, Party, Room, Visibility}
import grit.knowledge.sync.{Source, Sync, SyncParams, SyncSchedule}

/** The knowledge plugin: one card per unit of each of its sources, by its job [[Sync]] on each
  * source's sync schedules, the source's `_` written `-` in each key. A GitHub source's are
  * keyed `{source}-{key}` and declared at its label; a Drive source declares each once per
  * label, keyed `{source}-{key}-{label}`, the label in its written form ([[Label.written]])
  * with `+` written `-`, and declared at that label. Its documents: label "knowledge: what a
  * source holds, and how to read it", weight 1, kept 30 days after a version stops being
  * current, bounded at the sum of each GitHub source's `maxUnits` and each Drive source's
  * `maxUnits` times its labels; it posts nothing.
  */
final class Knowledge private (
    val name: PluginName,
    sources: Vector[Source[?]],
    declaring: Vector[Knowledge.Declaring],
    terms: DocumentTerms
) extends Plugin {

  val version: Int = 1

  private val sync = new Sync(sources)

  override val documents: Option[Documents] = {
    val kept = terms
    Some(new Documents {
      val terms: DocumentTerms = kept
      val posting: Option[DocumentPosting] = None
    })
  }

  override val jobs: Vector[Job[?]] = Vector(sync)

  override val schedules: Vector[Declared[?]] =
    declaring.map(d =>
      Declared(d.key, sync, d.sync.rule, SyncParams(d.corpus, d.sync.perRun), d.label)
    )
}

object Knowledge {

  /** One schedule [[Knowledge.of]] declares: `corpus`'s `sync`, keyed `key`, at `label`. */
  private final case class Declaring(
      corpus: CorpusName,
      sync: SyncSchedule,
      key: ScheduleKey,
      label: Label
  )

  /** The plugin over `cards` under the deployment's `visibility`, a GitHub source at its
    * declared label or, when it declares none, its service place's, and a Drive source at each
    * of its labels; or why not: no source; two of one corpus; two sync schedules of one key;
    * a source labelled below its service's place (for a Drive source, any of its labels), so
    * that its runs could not read the service (an unmapped place is below only labels holding
    * `unmapped`); or one whose service is cleared for less than its label (any of them), so
    * that its runs' calls could not send. A service's place takes no label through grit (ADR
    * 0033 labels rooms by command in them), so its declared label is the one in force, and a
    * plugin this returns has runs that are not refused for either reason.
    */
  def of(
      name: PluginName,
      cards: Vector[CardSource],
      visibility: Visibility
  ): Either[String, Knowledge] = {
    val sources: Vector[Source[?]] = cards.map {
      case CardSource.GitHub(c) => c
      case CardSource.Drive(c) => c
    }
    val corpora = sources.map(_.corpus)
    for {
      _ <- Either.cond(sources.nonEmpty, (), "a knowledge plugin has at least one source")
      _ <- corpora
        .diff(corpora.distinct)
        .headOption
        .toLeft(())
        .left
        .map(c => s"two sources are the corpus ${CorpusName.value(c)}")
      declaring <- cards.foldLeft[Either[String, Vector[Declaring]]](Right(Vector.empty))(
        (acc, c) => acc.flatMap(done => declared(c, visibility).map(done ++ _))
      )
      _ <- keysOnce(declaring)
      kept <- terms(cards.map {
        case CardSource.GitHub(c) => c.maxUnits.toLong
        case CardSource.Drive(c) => c.maxUnits.toLong * c.labels.size
      })
    } yield new Knowledge(name, sources, declaring, kept)
  }

  /** The schedules `card` declares under `visibility`, as [[Knowledge]] says; or why not, as
    * [[of]] says.
    */
  private def declared(
      card: CardSource,
      visibility: Visibility
  ): Either[String, Vector[Declaring]] =
    card match {
      case CardSource.GitHub(c) =>
        labelled(c, c.label, visibility).flatMap(keyed(c, _, None))
      case CardSource.Drive(c) =>
        c.labels.foldLeft[Either[String, Vector[Declaring]]](Right(Vector.empty))((acc, l) =>
          for {
            done <- acc
            label <- labelled(c, Some(l), visibility)
            more <- keyed(c, label, Some(Label.written(label).replace('+', '-')))
          } yield done ++ more
        )
    }

  /** Each of `source`'s syncs at `label`, keyed `{source}-{key}`, or with a `slug`,
    * `{source}-{key}-{slug}`, the source's `_` written `-`; or why a key is none.
    */
  private def keyed(
      source: Source[?],
      label: Label,
      slug: Option[String]
  ): Either[String, Vector[Declaring]] =
    source.syncs.foldLeft[Either[String, Vector[Declaring]]](Right(Vector.empty))((acc, s) =>
      for {
        done <- acc
        key <- ScheduleKey.of(
          (Vector(CorpusName.value(source.corpus).replace('_', '-'), ScheduleKey.value(s.key))
            ++ slug).mkString("-")
        )
      } yield done :+ Declaring(source.corpus, s, key, label)
    )

  /** The label `cards` is declared at under `visibility`: `label`, or its service place's when
    * it is none; or why not, as [[of]] says.
    */
  private def labelled(
      cards: Source[?],
      label: Option[Label],
      visibility: Visibility
  ): Either[String, Label] = {
    val source = CorpusName.value(cards.corpus)
    val service = cards.service
    // As Tx.roomLabel reads the place with nothing recorded through grit and no access reported.
    val place =
      visibility.compartments.admit(visibility.rooms.label(Room(service.place, None)).label)
    val cleared = visibility.clearances.of(Party.Service(service))
    val at = label.getOrElse(place)
    if (!at.dominates(place))
      Left(
        s"the source $source is ${Label.described(at)}, below its service " +
          s"${service.name}'s place, ${Label.shown(place)}: its runs could not read the service"
      )
    else if (!cleared.dominates(at))
      Left(
        s"the source $source is ${Label.described(at)}, above what its service " +
          s"${service.name} is cleared for, ${Label.clearance(cleared)}: " +
          "its runs' calls could not send"
      )
    else Right(at)
  }

  /** Unit when no two of `declaring` share a key; else the two sources and the key. */
  private def keysOnce(declaring: Vector[Declaring]): Either[String, Unit] =
    declaring
      .foldLeft[Either[String, Map[String, CorpusName]]](Right(Map.empty)) { (acc, d) =>
        acc.flatMap { seen =>
          val written = ScheduleKey.value(d.key)
          seen.get(written) match {
            case Some(first) =>
              Left(
                s"the sources ${CorpusName.value(first)} and ${CorpusName.value(d.corpus)} " +
                  s"both declare the sync schedule $written"
              )
            case None => Right(seen.updated(written, d.corpus))
          }
        }
      }
      .map(_ => ())

  /** The documents' terms, bounded at the sum of `units`, clipped to an `Int`. */
  private def terms(units: Vector[Long]): Either[String, DocumentTerms] =
    DocLabel
      .of("knowledge: what a source holds, and how to read it")
      .flatMap(
        DocumentTerms
          .of(_, DocWeight.Unscaled, 30.days, units.sum.min(Int.MaxValue).toInt)
      )
}
