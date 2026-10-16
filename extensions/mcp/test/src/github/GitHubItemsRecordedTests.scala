package grit.mcp.github

import grit.mcp.client.FakeMcpServer

/** [[GitHubItemsContract]] over GitHub's recorded answers (extensions/mcp/README.md). */
object GitHubItemsRecordedTests extends GitHubItemsContract {
  protected def repository: String = "octocat/Hello-World"
  protected def issue: Int = 7
  protected def pull: Int = 7

  protected def answer(tool: String, arguments: ujson.Obj): ujson.Obj = {
    val method = arguments.obj.get("method").flatMap(_.strOpt)
    val recorded = (tool, method) match {
      case ("issue_read", Some("get")) => "issue-read"
      case ("issue_read", Some("get_comments")) => "issue-read-comments"
      case ("pull_request_read", Some("get")) => "pull-request-read"
      case ("pull_request_read", Some("get_reviews")) => "pull-request-read-reviews"
      case ("get_file_contents", _) => "get-file-contents"
      case _ => throw new java.lang.AssertionError(s"no recording of $tool $method")
    }
    FakeMcpServer.githubAnswer(recorded)
  }
}
