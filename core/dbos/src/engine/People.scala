package grit.dbos.engine

import grit.core.identity.Identities

/** What an engine's start makes of the stored people (ADR 0032). */
enum People {

  /** Made what `identities` declares, as [[Engine.start]] says: an account it does not declare
    * goes back to a person of its own, and a handle it does not declare is cleared. Only the
    * deployment's own engine starts so, with its own declaration.
    */
  case Declared(identities: Identities)

  /** Left as stored, every link, handle and person: how an engine that is not the deployment's
    * (a tool's, or one writing a database for the eval) starts on a database a deployment may
    * have run on.
    */
  case AsStored
}
