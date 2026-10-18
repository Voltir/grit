package grit.core.plugin

import grit.core.need.Exported

/** A plugin another may need ([[grit.core.need.Exported]]): it exports `S`, a service over its
  * own documents, the only way a plugin reads another's (ADR 0027).
  */
trait Exports[S <: caps.Pure] extends Plugin, Exported[S]
