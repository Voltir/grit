package grit.core.plugin

import grit.core.id.WorkflowId
import grit.core.period.CloseOrdinal

/** One run posting to `plugin` at `version`, from its cursor at `cursor`. */
final case class PostRef(plugin: PluginName, version: Int, cursor: CloseOrdinal) {

  /** `post:{plugin}:{version}:{cursor}`: enqueued twice, it runs once. A run that moved the
    * cursor is followed under the new cursor's id.
    */
  def workflowId: WorkflowId =
    WorkflowId(
      s"${PostRef.Prefix}:${PluginName.value(plugin)}:$version:${CloseOrdinal.value(cursor)}"
    )
}

object PostRef {

  private val Prefix = "post"

  /** The run whose workflow id is `id`, or `None` if `id` is not a post's. */
  def fromWorkflowId(id: WorkflowId): Option[PostRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Prefix, plugin, version, cursor) =>
        for {
          p <- PluginName.of(plugin).toOption
          v <- version.toIntOption
          c <- cursor.toLongOption.flatMap(CloseOrdinal.of)
        } yield PostRef(p, v, c)
      case _ => None
    }
}
