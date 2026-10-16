package grit.core.classify

import grit.core.place.Service
import grit.core.route.{Routed, Uncleared}
import grit.core.visibility.{Label, Party}

/** The deployment's classifier behind its clearance check (ADR 0031): its states go to one
  * service, cleared like any party, as the deployment's visibility declares. A capability
  * shared like the classifier it holds.
  */
sealed trait ClearedClassifier extends caps.SharedCapability {

  /** What its calls' ledger rows record as their upstream: its service's place, written
    * `service:{name}`, which no model's upstream can be ([[grit.core.model.Upstream.of]] refuses
    * `:`); `None` for [[ClearedClassifier.none]]. The classifier in force when a row is
    * recorded, which for the turn's is its record step's.
    */
  def upstream: Option[String]

  /** The classifier to ask about what was read under `routed`; `Left` when its service is not
    * cleared for `routed`, and then nothing may be sent to it.
    */
  def at(routed: Routed): Either[Uncleared, Classifier^]

  /** [[at]]'s classifier; when that is `Left`, one that answers every call `Unavailable` at
    * once, naming its service and the classification, and sends nothing: what a site whose
    * fallback is the classifier's unavailable path asks.
    */
  final def asking(routed: Routed): Classifier^ = at(routed) match {
    case Right(classifier) => classifier
    case Left(uncleared) => unavailable(uncleared)
  }

  /** What [[asking]] asks in this one's place when it is not cleared for `uncleared`. */
  protected def unavailable(uncleared: Uncleared): Classifier
}

object ClearedClassifier {

  /** `classifier`, whose states go to `service`. */
  def of(service: Service, classifier: Classifier^): ClearedClassifier^ =
    new Of(service, classifier)

  /** No classifier: every call `Unavailable` with `why`. */
  def none(why: String): ClearedClassifier = new None(why)

  private final class Of(service: Service, classifier: Classifier^) extends ClearedClassifier {
    def upstream: Option[String] = Some(service.place.written)
    def at(routed: Routed): Either[Uncleared, Classifier^] =
      if (routed.clears(Party.Service(service))) Right(classifier)
      else Left(Uncleared(routed.classification.label))
    protected def unavailable(uncleared: Uncleared): Classifier =
      Classifier.none(
        s"its service, ${service.place.written}, is not cleared for what was read, " +
          Label.shown(uncleared.classification)
      )
  }

  private final class None(why: String) extends ClearedClassifier {
    def upstream: Option[String] = scala.None
    def at(routed: Routed): Either[Uncleared, Classifier^] = Right(Classifier.none(why))
    protected def unavailable(uncleared: Uncleared): Classifier = Classifier.none(why)
  }
}
