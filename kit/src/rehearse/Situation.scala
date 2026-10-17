package grit.kit.rehearse

import grit.core.identity.Account
import grit.core.speech.Reach
import grit.core.store.Origin

/** What a rehearsal is run on: a message an edge would hand over now, written by `by`, an
  * account the database holds or one it has never seen.
  */
enum Situation {

  /** `text`, heard at `origin` where grit listens; `reach` is where a reply would go, in the
    * edge's own address form, and whom it names: with no address, nothing is answered, and no
    * turn starts.
    */
  case Heard(origin: Origin, by: Account, text: String, reach: Reach)

  /** `text`, said to grit at `origin`. */
  case Said(origin: Origin, by: Account, text: String)
}
