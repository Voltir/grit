package grit.lifecycle.shadow

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.id.ShadowName
import grit.core.recipe.RoomReads
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.Db
import grit.core.triage.{Corpora, TriageShadows}
import grit.lifecycle.triage.TriageQuestions

/** What a shadow works with besides its `Durable`: where it reads the heard message, its
  * thread and its room (`reads` and `rooms`, through `db` outside a transaction, under
  * `tuning`, as triage reads them) and keeps what the variant made of it (`shadows`), each
  * declared variant by name (`variants`; a shadow of a variant not among them asks nothing),
  * the deployment's catalog of corpora a question set's per-source questions are
  * asked of (`sources`), and `clock`, for when a row was kept and how long its call took.
  */
final case class ShadowEnv(
    reads: StitchReads,
    rooms: RoomReads,
    shadows: TriageShadows,
    variants: Map[ShadowName, ShadowAsking^],
    sources: Corpora,
    db: Db^,
    clock: Clock^,
    tuning: Tuning
)

/** A declared variant as its shadows ask: `questions`, of the model `requested` (as the
  * deployment names it, kept with each answer), through `classifier`.
  */
final case class ShadowAsking(
    questions: TriageQuestions,
    requested: String,
    classifier: Classifier^
)
