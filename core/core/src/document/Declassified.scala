package grit.core.document

import grit.core.flow.Downgrade
import grit.core.id.{DocKey, DocumentVersionId, PluginName, ShortHash}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.visibility.{DeclassificationPolicy, Integrity, Label}

/** `policy` applied to the document `document` reads, by the `version`th application to that
  * document: the one in force for it, since a document has at most one.
  */
final case class PolicyApplication private[grit] (
    policy: DeclassificationPolicy,
    document: Pointer,
    version: Int
) extends caps.Pure

/** A rewrite of the document version `source`, without `applied`'s compartment, by the
  * declassification ([[Declassified.declassification]]); the highest of the check's
  * probabilities that it keeps what the policy removes, hints at it, or instructs its reader is
  * `judged`. Only [[DocumentKeeper.declassified]] keeps it, at `label`, deriving every field of
  * the row from it; nothing outside core reads its text.
  */
final class Declassified private[document] (
    private[core] val text: DocText,
    private[core] val from: Declassified.Source,
    val applied: PolicyApplication,
    val judged: Probability,
    val label: Label
) extends caps.Pure {
  def source: DocumentVersionId = from.version
}

object Declassified {

  /** The document version a rewrite is made from, as it was read: its version, its plugin and
    * key, its place, its label, and the pointer that reads it.
    */
  final case class Source private[core] (
      version: DocumentVersionId,
      plugin: PluginName,
      key: DocKey,
      place: Place,
      label: Label,
      pointer: Pointer
  ) extends caps.Pure

  /** What a kept rewrite was made from, as its data names it: its `source` version, and the
    * compartment (`over`) and `policy` version of the policy, and the `application` version, it
    * was made under. A rewrite is current while these are its source's current version and the
    * versions in force.
    */
  final case class Made(
      source: DocumentVersionId,
      over: DeclassificationPolicy.Over,
      policy: Int,
      application: Int
  ) extends caps.Pure {

    /** `{"source", "over", "policy", "application"}`: a kept rewrite's data. */
    def written: ujson.Obj =
      ujson.Obj(
        "source" -> ujson.Num(DocumentVersionId.value(source).toDouble),
        "over" -> ujson.Str(over.written),
        "policy" -> ujson.Num(policy.toDouble),
        "application" -> ujson.Num(application.toDouble)
      )
  }

  object Made {

    /** A rewrite of `source` under `applied`. */
    def of(source: DocumentVersionId, applied: PolicyApplication): Made =
      Made(source, applied.policy.over, applied.policy.version, applied.version)

    /** What `data` says a rewrite was made from ([[Made.written]]); `None` when it is no
      * rewrite's data.
      */
    def read(data: ujson.Value): Option[Made] =
      for {
        o <- data.objOpt
        source <- o.get("source").flatMap(_.numOpt).flatMap(n => DocumentVersionId.of(n.toLong))
        over <- o
          .get("over")
          .flatMap(_.strOpt)
          .flatMap(DeclassificationPolicy.Over.read(_).toOption)
        policy <- o.get("policy").flatMap(_.numOpt).map(_.toInt)
        application <- o.get("application").flatMap(_.numOpt).map(_.toInt)
      } yield Made(source, over, policy, application)
  }

  /** 0.1: the check's probability at or above which nothing is declassified. */
  val Below: Probability = Probability.clamped(0.1)

  /** A declassification: `rewrite`, made from `source` under `applied`, at its policy's
    * `lowered` of the source's label, when `leak`, the highest of the check's probabilities, is
    * below [[Below]]; `None` otherwise, when `lowered` is `None`, and when `applied` names
    * another document than `source`'s pointer reads. Its check judges the rewrite only against
    * its own policy and source: what the policy fails to name passes, and were rewrites ever
    * declassified again, each step's residual would add to the last's.
    */
  private[core] val declassification: Downgrade[
    (rewrite: DocText, source: Source, applied: PolicyApplication, leak: Probability),
    Option[Declassified]
  ] =
    new Downgrade(
      Downgrade.Described(
        "declassification",
        Downgrade.Axis.Classification,
        Downgrade.Authority.Policy,
        Downgrade.Capacity.Text
      ),
      in =>
        if (in.leak >= Below || in.applied.document != in.source.pointer) None
        else
          in.applied.policy
            .lowered(in.source.label)
            .map(new Declassified(in.rewrite, in.source, in.applied, in.leak, _))
    )

  /** What [[DocumentKeeper.declassified]] keeps of a declassified rewrite, every field derived
    * from it: its text at `label`, believed at `integrity`, at `place`, under `key`, with
    * `data`, and no pointer or refs; and its audit row, of `source`, `applied` and `judged`.
    */
  final class Row private[document] (
      val key: DocKey,
      val label: Label,
      val integrity: Integrity,
      val place: Place,
      val text: DocText,
      val data: ujson.Value,
      val source: DocumentVersionId,
      val applied: PolicyApplication,
      val judged: Probability
  ) extends caps.Pure {
    private[document] def believedAtMost(most: Integrity): Row =
      new Row(key, label, integrity.meet(most), place, text, data, source, applied, judged)
  }

  /** `d`'s row, believed at `integrity`: under its source's plugin and a hash of its source's
    * key, so that a key naming what the policy removes is never repeated, with data naming its
    * source version and application alone; or why not.
    */
  private[document] def row(d: Declassified, integrity: Integrity): Either[String, Row] =
    DocKey
      .of(s"${PluginName.value(d.from.plugin)}:${ShortHash.of(DocKey.value(d.from.key))}")
      .map(key =>
        new Row(
          key,
          d.label,
          integrity,
          d.from.place,
          d.text,
          Made.of(d.source, d.applied).written,
          d.source,
          d.applied,
          d.judged
        )
      )
}
