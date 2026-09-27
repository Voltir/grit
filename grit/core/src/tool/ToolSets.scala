package grit.core.tool

import grit.core.store.{StoreError, Tx}

/** Every tool set a turn was offered, each kept once under its id and never changed. */
trait ToolSets {

  /** Keeps `set` under [[ToolSet.id]]; a set already kept is left as it is. */
  def keep(set: ToolSet)(using Tx^): Either[StoreError, Unit]

  /** The set kept under `id`; `StoreError.Invalid` naming it when none is. */
  def get(id: ToolSetId)(using Tx^): Either[StoreError, ToolSet]
}
