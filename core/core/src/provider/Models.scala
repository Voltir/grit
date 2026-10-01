package grit.core.provider

import grit.core.model.{Catalog, Pinned}

/** Capability to reach models as a catalog says: which catalog is in force, and a provider
  * for each role's pin.
  */
trait Models extends caps.SharedCapability {

  /** The catalog in force now; `Left` says why none can be read. */
  def catalog(): Either[String, Catalog]

  /** A provider whose every call is made under `pinned`: its model, budget, upstream,
    * effort and settings. It may be made on this call.
    */
  def provider(pinned: Pinned): Provider^
}
