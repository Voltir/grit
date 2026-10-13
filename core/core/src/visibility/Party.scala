package grit.core.visibility

import grit.core.model.{ModelId, ModelRef, Upstream}

/** A subject grit sends content to outside itself (Bell–LaPadula). The deployment clears each
  * ([[Cleared]]); one it does not name is cleared for public alone.
  */
enum Party derives CanEqual {

  /** An outside service: sent a call's arguments (ADR 0031), or a classifier's states. */
  case Service(service: grit.core.place.Service)

  /** `model` served by `upstream` alone. */
  case Model(model: ModelId, upstream: Upstream)
}

object Party {

  /** `ref`'s party; `None` when OpenRouter picks its upstream, which no clearance can name. */
  def of(ref: ModelRef): Option[Party] = ref.upstream.map(Party.Model(ref.model, _))
}
