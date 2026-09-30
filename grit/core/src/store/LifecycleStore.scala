package grit.core.store

import grit.core.period.LifecycleSettings

/** The lifecycle's settings in force, one set for the whole database: read afresh by each
  * sweep and each turn's assembly, so a change applies before the next of either.
  */
trait LifecycleStore {

  /** The settings in force; [[LifecycleSettings.Default]] when none are stored.
    * `Invalid` when the stored ones break [[LifecycleSettings.of]]'s rules, as they can
    * once changed by hand.
    */
  def current()(using Tx^): Either[StoreError, LifecycleSettings]

  /** Replaces the settings in force with `settings`. */
  def set(settings: LifecycleSettings)(using Tx^): Either[StoreError, Unit]
}
