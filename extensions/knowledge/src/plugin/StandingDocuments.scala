package grit.knowledge.plugin

import grit.core.document.DocumentTerms
import grit.core.id.PluginName
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.tool.ToolName
import grit.knowledge.standing.Standing

/** What each earned closing of a conversation settled, kept as one document
  * ([[grit.knowledge.standing.Standing]]), leaving out the lines whose add names one of the
  * tools in `leftOut`.
  */
final class StandingDocuments(val name: PluginName, val leftOut: Set[ToolName]) extends Plugin {

  val version: Int = 1

  override val documents: Option[Documents] = Some(new Documents {
    val terms: DocumentTerms = Standing.Terms
    val posting: Option[DocumentPosting] = Some(Standing.posting(leftOut))
  })
}
