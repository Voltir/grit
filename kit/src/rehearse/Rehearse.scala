package grit.kit.rehearse

import grit.core.classify.ClearedClassifier
import grit.core.id.ShadowName
import grit.kit.deployment.Deployment
import grit.kit.run.Launch
import grit.lifecycle.shadow.ShadowAsking

/** Rehearsing a situation through a deployment's shipped workflow bodies. */
object Rehearse {

  /** Each of `d`'s shadows by name, asking its question through `classifier` of the model the
    * live engine's would ask of ([[Launch.shadowModel]]).
    */
  private[rehearse] def shadowing(
      d: Deployment,
      classifier: ClearedClassifier^
  ): Map[ShadowName, ShadowAsking^{classifier}] =
    d.shadows
      .map(v => v.name -> ShadowAsking(v.questions, Launch.shadowModel(d.topics, v), classifier))
      .toMap
}
