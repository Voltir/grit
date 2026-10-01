package grit.mcp.github

import grit.mcp.scope.Bound

/** Bounds for GitHub's MCP server, on `owner` and `repo`, the arguments its repository tools
  * require.
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
}
