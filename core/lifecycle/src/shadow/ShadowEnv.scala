package grit.lifecycle.shadow

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.id.ShadowName
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.Db
import grit.core.triage.TriageShadows
import grit.lifecycle.triage.TriageQuestion

/** What a shadow works with besides its `Durable`: where it reads the heard message and its
  * thread (`reads`, through `db` outside a transaction, under `tuning`, as triage reads
  * them) and keeps what the variant made of it (`shadows`), each declared variant by name
  * (`variants`; a shadow of a variant not among them asks nothing), and `clock`, for when a
  * row was kept and how long its call took.
  */
final case class ShadowEnv(
    reads: StitchReads,
    shadows: TriageShadows,
    variants: Map[ShadowName, ShadowAsking^],
    db: Db^,
    clock: Clock^,
    tuning: Tuning
)

/** A declared variant as its shadows ask: triage's question in `wording`, of the model
  * `requested` (as the deployment names it, kept with each answer), through `classifier`.
  */
final case class ShadowAsking(
    wording: TriageQuestion.Wording,
    requested: String,
    classifier: Classifier^
)
