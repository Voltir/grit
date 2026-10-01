package grit.core.plugin

import grit.core.id.{PluginName, WorkflowId}
import grit.core.period.CloseOrdinal

/** The `attempt`-th run (from 0) posting to `plugin` at `version` from its cursor at
  * `cursor`.
  */
final case class PostRef(plugin: PluginName, version: Int, cursor: CloseOrdinal, attempt: Int) {

  /** `post:{plugin}:{version}:{cursor}:{attempt}`: enqueued twice, it runs once. */
  def workflowId: WorkflowId =
    WorkflowId(s"${PostRef.prefix(plugin, version, cursor)}$attempt")
}

object PostRef {

  private val Name = "post"

  /** How many runs are made from one cursor before it is left for a person: a run that
    * leaves the cursor where it was failed, and the next is made on the next sweep.
    */
  val Attempts = 3

  /** What the workflow id of every run of `plugin`, at any version, starts with, and no other
    * plugin's.
    */
  def prefix(plugin: PluginName): String = s"$Name:${PluginName.value(plugin)}:"

  /** What the workflow id of every run from `cursor` of `plugin` at `version` starts with,
    * and no other run's.
    */
  def prefix(plugin: PluginName, version: Int, cursor: CloseOrdinal): String =
    s"${prefix(plugin)}$version:${CloseOrdinal.value(cursor)}:"

  /** The run whose workflow id is `id`, or `None` if `id` is not a post's. */
  def fromWorkflowId(id: WorkflowId): Option[PostRef] =
    WorkflowId.value(id).split(':') match {
      case Array(Name, plugin, version, cursor, attempt) =>
        for {
          p <- PluginName.of(plugin).toOption
          v <- version.toIntOption
          c <- cursor.toLongOption.flatMap(CloseOrdinal.of)
          a <- attempt.toIntOption.filter(_ >= 0)
        } yield PostRef(p, v, c, a)
      case _ => None
    }
}
