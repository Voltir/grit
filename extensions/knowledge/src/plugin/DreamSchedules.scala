package grit.knowledge.plugin

import grit.core.document.{Document, DocumentTerms}
import grit.core.id.ScheduleKey
import grit.core.plugin.{DocumentPosting, Documents}
import grit.core.visibility.{Label, Visibility}

/** What a plugin running one dreaming job per label shares: its schedules' keys and its
  * documents, each kept at one of those labels.
  */
private[plugin] object DreamSchedules {

  /** Each of `labels` with the key `{job}-{label}`, the label's written form with `+` written
    * `-`; or why not, naming `what` the plugin is: no label, one given twice, one holding a
    * compartment `visibility` does not declare, or one whose written form makes no key.
    */
  def keyed(
      what: String,
      job: String,
      labels: Vector[Label],
      visibility: Visibility
  ): Either[String, Vector[(ScheduleKey, Label)]] =
    for {
      _ <- Either.cond(labels.nonEmpty, (), s"the $what plugin is declared at a label at least")
      _ <- labels
        .diff(labels.distinct)
        .headOption
        .toLeft(())
        .left
        .map(l => s"the $what plugin is declared at ${Label.written(l)} twice")
      _ <- labels
        .flatMap(l => visibility.compartments.undeclared(l).map(_ => l))
        .headOption
        .toLeft(())
        .left
        .map(l => s"the $what plugin's label ${Label.written(l)} holds a compartment not declared")
      keyed <- labels.foldLeft[Either[String, Vector[(ScheduleKey, Label)]]](Right(Vector()))(
        (acc, l) =>
          acc.flatMap(done =>
            ScheduleKey
              .of(s"$job-${Knowledge.slug(l)}")
              .left
              .map(why => s"the $what plugin's label ${Label.written(l)} makes no key: $why")
              .map(k => done :+ (k, l))
          )
      )
    } yield keyed

  /** Documents under `terms`, posted nothing, one at a label not among `labels` undeclared. */
  def documents(terms: DocumentTerms, labels: Vector[Label]): Documents = {
    val kept = terms
    new Documents {
      val terms: DocumentTerms = kept
      val posting: Option[DocumentPosting] = None

      /** A document at a label no schedule is declared at: no run reads it as its own. */
      override def undeclared(document: Document): Boolean = !labels.contains(document.label)
    }
  }
}
