package grit.mcp.client

import grit.core.edge.{EdgeRefusal, Variable}

import utest.*

/** [[McpServer]], a declared server, and [[Bearer]], its token read from the environment. */
object McpServerTests extends TestSuite {

  private val token = Variable("GITHUB_MCP_TOKEN")

  private def server(name: String = "github", endpoint: String = "https://example.com/mcp") =
    McpServer.of(name, endpoint, token).map(s => (s.name, s.endpoint.toString))

  val tests = Tests {
    test("a name is a lowercase letter then up to 15 lowercase letters, digits or _") {
      Vector("g", "github", "git_hub2", "abcdefghijklmnop").map(n => server(n).map(_._1)) ==>
        Vector(Right("g"), Right("github"), Right("git_hub2"), Right("abcdefghijklmnop"))
      Vector("", "GitHub", "2git", "_git", "git-hub", "git.hub", "abcdefghijklmnopq")
        .map(n => server(n).isLeft) ==> Vector.fill(7)(true)
    }

    test("an endpoint is https, or http on a loopback host only") {
      Vector(
        "https://api.githubcopilot.com/mcp/readonly",
        "http://localhost:8080/mcp",
        "http://127.0.0.1:9/mcp",
        "http://[::1]:9/mcp"
      ).map(e => server(endpoint = e).map(_._2)) ==> Vector(
        Right("https://api.githubcopilot.com/mcp/readonly"),
        Right("http://localhost:8080/mcp"),
        Right("http://127.0.0.1:9/mcp"),
        Right("http://[::1]:9/mcp")
      )
      server(endpoint = "http://example.com/mcp") ==>
        Left(
          "github's endpoint must be https (http only on a loopback host): 'http://example.com/mcp'"
        )
      Vector(
        "http://127.0.0.1.example.com/mcp",
        "http://128.0.0.1/mcp",
        "ftp://localhost/mcp",
        "/mcp",
        "https:///mcp",
        "https://exa mple.com"
      ).map(e => server(endpoint = e).isLeft) ==> Vector.fill(6)(true)
    }

    test("a token is the variable's value; unset, blank or not visible ASCII names the variable") {
      Bearer.of(Map("GITHUB_MCP_TOKEN" -> "github_pat_x1"), token).map(_.value) ==>
        Right("github_pat_x1")
      Bearer.of(Map.empty, token) ==> Left(EdgeRefusal.Missing(token))
      Bearer.of(Map("GITHUB_MCP_TOKEN" -> "  "), token) ==>
        Left(EdgeRefusal.Malformed(token, "it is blank"))
      Bearer.of(Map("GITHUB_MCP_TOKEN" -> "github_pat_x1\n"), token) ==>
        Left(
          EdgeRefusal.Malformed(
            token,
            "it holds a character that is not visible ASCII (a space or a line break?)"
          )
        )
    }

    test("a token is never shown") {
      Bearer.of(Map("GITHUB_MCP_TOKEN" -> "github_pat_x1"), token).map(_.toString) ==>
        Right("Bearer(<redacted>)")
    }
  }
}
