package grit.mcp.github

import grit.mcp.scope.{Attribution, Bound, McpScope}

/** Scopes for GitHub's MCP server: bounds on `owner` and `repo`, the arguments its repository
  * tools require, and its search tools held by the repository each result names.
  */
object GitHubScope {

  /** The repository `fullName`, `{owner}/{name}`, as a bound on `owner` and `repo`, or why not:
    * not two non-blank segments.
    */
  def repository(fullName: String): Either[String, Bound] =
    fullName.split("/", -1).toVector match {
      case Vector(owner, name) if !owner.isBlank && !name.isBlank =>
        Bound.of("owner" -> owner, "repo" -> name)
      case _ => Left(s"a repository is {owner}/{name}, not '$fullName'")
    }

  /** Every repository `login`'s organisation or user owns, as a bound on `owner`; `Left` when
    * blank or holding `/`.
    */
  def owner(login: String): Either[String, Bound] =
    if (login.isBlank || login.contains('/')) Left(s"an owner is a login with no /, not '$login'")
    else Bound.of("owner" -> login)

  /** The scope held to `first` and `rest`, GitHub's search tools held by their answers:
    * `search_code` by each result's `repository`, `{owner}/{name}`, and `search_issues` and
    * `search_pull_requests` by each result's `repository_url`,
    * `https://api.github.com/repos/{owner}/{name}`. Their `total_count` is set to the results
    * kept, and `fields`, which could leave the repository out, is never sent. `Left` when a bound
    * names an argument other than `owner` and `repo`.
    */
  def within(first: Bound, rest: Bound*): Either[String, McpScope] =
    Searches.flatMap(McpScope.attributed(_, first, rest*))

  /** Where the issue and pull-request searches' results name their repository. */
  private val Api = "https://api.github.com/repos/"

  /** The search tools, each with how its answers name each result's repository. */
  private val Searches: Either[String, Map[String, Attribution]] = {
    def by(key: String, read: String -> Option[String], form: String): Either[String, Attribution] =
      Attribution.of(
        "items",
        Some("total_count"),
        Vector("owner", "repo"),
        Set("fields"),
        result =>
          result.objOpt.flatMap(_.get(key)).flatMap(_.strOpt) match {
            case None => Left(s"it has no $key string")
            case Some(at) =>
              read(at).map(_.split("/", -1).toVector) match {
                case Some(segments @ Vector(owner, name)) if owner.nonEmpty && name.nonEmpty =>
                  Right(segments)
                case _ => Left(s"its $key $at is not $form")
              }
          }
      )
    for {
      code <- by("repository", Some(_), "{owner}/{name}")
      issues <- by(
        "repository_url",
        at => Option.when(at.startsWith(Api))(at.stripPrefix(Api)),
        s"$Api{owner}/{name}"
      )
    } yield Map("search_code" -> code, "search_issues" -> issues, "search_pull_requests" -> issues)
  }
}
