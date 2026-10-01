package grit.core.store

import grit.core.tool.{ToolSet, ToolSetId, ToolSets}

/** An in-memory [[ToolSets]] for tests, keeping [[StoreContract]]. It ignores the `Tx`. */
final class InMemoryToolSets extends ToolSets {

  @caps.unsafe.untrackedCaptures
  var sets = Vector.empty[ToolSet]

  def keep(set: ToolSet)(using Tx^): Either[StoreError, Unit] = {
    if (!sets.exists(_.id == set.id)) sets = sets :+ set
    Right(())
  }

  def get(id: ToolSetId)(using Tx^): Either[StoreError, ToolSet] =
    sets.find(_.id == id).toRight(StoreError.Invalid(s"no tool set ${ToolSetId.value(id)} is kept"))
}
