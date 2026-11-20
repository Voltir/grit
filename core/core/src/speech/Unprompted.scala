package grit.core.speech

import grit.core.id.TurnRef
import grit.core.spend.Budget

/** Whether and within what grit speaks where it was not addressed (`speaking`, read when it
  * decides), the deployment's `budget`, and `start`, which queues an unprompted turn once grit
  * drafts on it, a heard message's or an occasion's; why not, when it could not.
  */
final case class Unprompted(
    speaking: Speaking,
    budget: Budget,
    start: TurnRef => Either[String, Unit]
)
