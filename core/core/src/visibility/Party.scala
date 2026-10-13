package grit.core.visibility

import grit.core.model.{ModelId, Upstream}

/** A subject grit sends content to outside itself (Bell–LaPadula). The deployment clears each
  * ([[Cleared]]); one it does not name is cleared for public alone.
  */
enum Party {

  /** An outside service: sent a call's arguments (ADR 0031), or a classifier's states. */
  case Service(service: grit.core.place.Service)

  /** `model` served by `upstream` alone. */
  case Model(model: ModelId, upstream: Upstream)
}
