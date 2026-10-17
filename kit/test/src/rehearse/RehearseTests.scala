package grit.kit.rehearse

import java.time.Instant

import grit.core.classify.ClearedClassifier
import grit.core.id.ShadowName
import grit.core.spend.DailyCap
import grit.kit.deployment.{Deployments, Topics}
import grit.lifecycle.shadow.{ShadowAsking, ShadowVariant}
import grit.lifecycle.triage.TriageQuestions
import grit.models.{JevConfig, StubClassifier}

import utest.*

/** [[Rehearse]]: what a dry rehearsal launches its deployment's workflows with. */
object RehearseTests extends TestSuite {

  private def variant(name: String, model: Option[String]) =
    ShadowVariant(
      ShadowName.of(name).getOrElse(sys.error("a name")),
      TriageQuestions.V1,
      model,
      DailyCap.of("0.01").getOrElse(sys.error("a cap")),
      Instant.EPOCH
    )

  val tests = Tests {
    test(
      "each shadow asks the rehearsal's classifier, of the model it names, else its topics' classifier's own: Jev's default or the stub's"
    ) {
      val tapped = ClearedClassifier.none("tapped")
      def shown(name: ShadowName, asking: ShadowAsking^{tapped}): (String, String, Boolean) =
        (name.toString, asking.requested, asking.classifier == tapped)
      def asked(topics: Topics) =
        Deployments
          .of(
            topics = topics,
            shadows = Vector(variant("named", Some("jev-1.2")), variant("plain", None))
          )
          .map(d => Rehearse.shadowing(d, tapped).toVector.map(shown).sorted)
      (asked(Topics.Jev), asked(Topics.Stub)) ==> (
        Right(Vector(("named", "jev-1.2", true), ("plain", JevConfig.DefaultModel, true))),
        Right(Vector(("named", "jev-1.2", true), ("plain", StubClassifier.Model, true)))
      )
    }
  }
}
