package grit.core.document

import grit.core.flow.Downgrade
import grit.core.id.{DocKey, DocumentVersionId, PluginName, Sha256, ShortHash}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.screen.{Benched, Checks, Screen, Screened}
import grit.core.visibility.{DeclassificationPolicy, Integrity, Label}

/** `policy` applied to the document `document` reads, by the `version`th application to that
  * document: the one in force for it, since a document has at most one.
  */
final case class PolicyApplication private[grit] (
    policy: DeclassificationPolicy,
    document: Pointer,
    version: Int
) extends caps.Pure

object PolicyApplication {

  /** An application in force, `applied`, with its policy's `instructions` and `checks` when its
    * transaction reads the policy's label ([[DocumentKeeper.declassifying]]), both or neither.
    * `keptFrom` is the lowest policy version of its compartment whose rewrites of its document
    * stand until `applied`'s policy version decides it ([[Declassified.Made.stands]]): that
    * version's own when it was published to withdraw; else the highest earlier version published
    * to withdraw, or revoked; else 1.
    */
  final class InForce private[grit] (
      val applied: PolicyApplication,
      val keptFrom: Int,
      private[core] val policy: Option[
        (instructions: String, checks: Either[Checks.Unreadable, Checks])
      ]
  ) extends caps.Pure
}

/** A rewrite of the document version `source`, without `applied`'s compartment, by the
  * declassification ([[Declassified.declassification]]), its screen passed on its `attempt`;
  * `judged`, that screen's [[grit.core.screen.Screened.judged]]. Only
  * [[DocumentKeeper.declassified]] keeps it, at `label`, deriving every field of the row from
  * it; nothing outside core reads its text.
  */
final class Declassified private[document] (
    private[core] val text: DocText,
    private[core] val from: Declassified.Source,
    val applied: PolicyApplication,
    val judged: Probability,
    val attempt: Declassified.Attempt,
    val label: Label
) extends caps.Pure {
  def source: DocumentVersionId = from.version
}

object Declassified {

  /** The document version a rewrite is made from, as it was read: its version, its plugin and
    * key, its place, its label, the pointer that reads it, and the `revision` of the file whose
    * text was rewritten.
    */
  final case class Source(
      version: DocumentVersionId,
      plugin: PluginName,
      key: DocKey,
      place: Place,
      label: Label,
      pointer: Pointer,
      revision: String
  ) extends caps.Pure

  /** What a kept rewrite was made from, as its data names it: its `source` version and file
    * `revision`, and the compartment (`over`) and `policy` version of the policy, and the
    * `application` version, it was made under. A rewrite goes on being shared while it
    * [[stands]].
    */
  final case class Made(
      source: DocumentVersionId,
      revision: String,
      over: DeclassificationPolicy.Over,
      policy: Int,
      application: Int
  ) extends caps.Pure {

    /** `{"source", "revision", "over", "policy", "application"}`: a kept rewrite's data. */
    def written: ujson.Obj =
      ujson.Obj(
        "source" -> ujson.Num(DocumentVersionId.value(source).toDouble),
        "revision" -> ujson.Str(revision),
        "over" -> ujson.Str(over.written),
        "policy" -> ujson.Num(policy.toDouble),
        "application" -> ujson.Num(application.toDouble)
      )

    /** Whether this rewrite stands as one of the document version `source`, its file at
      * `revision`, under the application in force for it, `in`: it was made from them, under
      * `in`'s application version, and under `in`'s policy version, or under an earlier one from
      * `in.keptFrom` while `in`'s has not `decided` the document (withheld it). Only `in`'s own
      * rewrites stand once its version was published to withdraw.
      */
    def stands(
        in: PolicyApplication.InForce,
        source: DocumentVersionId,
        revision: String,
        decided: Boolean
    ): Boolean = {
      val applied = in.applied
      this.source == source && this.revision == revision && over == applied.policy.over &&
      application == applied.version &&
      (policy == applied.policy.version ||
        (!decided && policy >= in.keptFrom && policy < applied.policy.version))
    }
  }

  object Made {

    /** A rewrite of `source`, its file at `revision`, under `applied`. */
    def of(source: DocumentVersionId, revision: String, applied: PolicyApplication): Made =
      Made(source, revision, applied.policy.over, applied.policy.version, applied.version)

    /** What `data` says a rewrite was made from ([[Made.written]]); `None` when it is no
      * rewrite's data, or one without a revision.
      */
    def read(data: ujson.Value): Option[Made] =
      for {
        o <- data.objOpt
        source <- o.get("source").flatMap(_.numOpt).flatMap(n => DocumentVersionId.of(n.toLong))
        revision <- o.get("revision").flatMap(_.strOpt)
        over <- o
          .get("over")
          .flatMap(_.strOpt)
          .flatMap(DeclassificationPolicy.Over.read(_).toOption)
        policy <- o.get("policy").flatMap(_.numOpt).map(_.toInt)
        application <- o.get("application").flatMap(_.numOpt).map(_.toInt)
      } yield Made(source, revision, over, policy, application)
  }

  /** Which ask of a declassify wrote the rewrite: its first, or the one retry. */
  enum Attempt(val number: Int) extends caps.Pure {
    case First extends Attempt(1)
    case Second extends Attempt(2)
  }

  /** A declassification: `rewrite`, its `attempt`, made from `source` under `applied`, at its
    * policy's `lowered` of the source's label, when its `screened` answers fire nothing against
    * the source's `baseline` ([[grit.core.screen.Screened.fired]]); `None` otherwise, when
    * `lowered` is `None`, and when `applied` names another document than `source`'s pointer
    * reads. Its screen judges the rewrite only against its own policy's signals and its own
    * source: what the policy fails to name passes, and were rewrites ever declassified again,
    * each step's residual would add to the last's.
    */
  private[core] val declassification: Downgrade[
    (
        rewrite: DocText,
        source: Source,
        applied: PolicyApplication,
        screened: Screened,
        baseline: Option[Screen.Baseline],
        attempt: Attempt
    ),
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
        if (!in.screened.fired(in.baseline).passes || in.applied.document != in.source.pointer)
          None
        else
          in.applied.policy
            .lowered(in.source.label)
            .map(
              new Declassified(
                in.rewrite,
                in.source,
                in.applied,
                in.screened.judged(in.baseline),
                in.attempt,
                _
              )
            )
    )

  /** What [[DocumentKeeper.declassified]] keeps of a declassified rewrite, every field derived
    * from it: its text at `label`, believed at `integrity`, at `place`, under `key`, with
    * `data`, and no pointer or refs; and its audit row, of `source`, `applied`, `judged` and
    * `attempt`.
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
      val judged: Probability,
      val attempt: Attempt
  ) extends caps.Pure {
    private[document] def believedAtMost(most: Integrity): Row =
      new Row(
        key,
        label,
        integrity.meet(most),
        place,
        text,
        data,
        source,
        applied,
        judged,
        attempt
      )
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
          Made.of(d.source, d.from.revision, d.applied).written,
          d.source,
          d.applied,
          d.judged,
          d.attempt
        )
      )

  /** What a policy version's bench is kept under ([[grit.core.act.Declassifying.screening]]):
    * a digest of its checks and of the screen's own questions and thresholds, so a bench goes
    * stale when either changes.
    */
  opaque type Screening = Sha256

  object Screening {
    private[core] def of(digest: Sha256): Screening = digest

    /** The screening [[written]] wrote; `None` for other text. */
    private[grit] def read(hex: String): Option[Screening] = Sha256.read(hex)

    extension (s: Screening) {

      /** As 64 lowercase hex digits. */
      def written: String = Sha256.hex(s)
    }

    given CanEqual[Screening, Screening] = CanEqual.derived
  }

  /** A policy version's checks as [[DocumentKeeper.checks]] read them, or why they do not read.
    */
  final class PolicyChecks private[grit] (
      val policy: DeclassificationPolicy,
      private[core] val checks: Either[Checks.Unreadable, Checks]
  ) extends caps.Pure

  /** A policy version's bench, kept by [[DocumentKeeper.keepBenched]]: `benched`, the verdict
    * on `policy`'s examples, under `screening`. Made only by [[grit.core.act.Dreaming.bench]].
    */
  final class BenchRow private[core] (
      val policy: DeclassificationPolicy,
      val screening: Screening,
      val benched: Benched
  ) extends caps.Pure
}
