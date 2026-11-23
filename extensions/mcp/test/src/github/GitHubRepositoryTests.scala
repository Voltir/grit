package grit.mcp.github

import java.net.URI

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.core.host.{HostError, RemoteRepository}
import grit.mcp.client.{Credential, KeyFiles}

import utest.*

/** What [[GitHubRepository]] asks of GitHub and how it reads the answers, over
  * [[FakeGitHubServer]].
  */
object GitHubRepositoryTests extends TestSuite {

  private def over[A](body: FakeGitHubServer => A): A = {
    val fake = FakeGitHubServer.start()
    try body(fake)
    finally fake.stop()
  }

  private val authorizer = {
    val token = Variable("FAKE_GITHUB_TOKEN")
    Credential
      .open(Credential.static(token), Map("FAKE_GITHUB_TOKEN" -> "ghs_x"), KeyFiles.Local)
      .fold(r => throw new java.lang.AssertionError(r.message), identity)
  }

  val tests = Tests {
    test("open reads the default branch, then asks for a pull request from the branch into it") {
      over { fake =>
        val at = fake.repository().open("grit/w1", "Fix it", "Fixed.")
        (at, fake.requests, fake.posted) ==> (
          Right(URI.create("https://github.com/acme/app/pull/1")),
          Vector(("GET", "repos/acme/app", ""), ("POST", "repos/acme/app/pulls", "")),
          Vector(
            ujson.Obj(
              "title" -> "Fix it",
              "head" -> "grit/w1",
              "base" -> "main",
              "body" -> "Fixed."
            )
          )
        )
      }
    }
    test("a pull request is asked for by branch, of every state, newest first") {
      over { fake =>
        val _ = fake.repository().pullRequest("grit/w1")
        fake.requests ==> Vector(
          (
            "GET",
            "repos/acme/app/pulls",
            "head=acme%3Agrit%2Fw1&state=all&sort=created&direction=desc&per_page=100"
          )
        )
      }
    }
    test("a pull request from another repository's branch of that name is not this one's") {
      over { fake =>
        fake.foreign("grit/w1", "someone/app")
        fake.repository().pullRequest("grit/w1") ==> Right(None)
      }
    }
    test("an open pull request is the branch's over a newer closed one") {
      over { fake =>
        val r = fake.repository()
        val first = r.open("grit/w1", "Fix it", "Fixed.")
        fake.close("grit/w1", "abc")
        val second = r.open("grit/w1", "Fix it", "Again.")
        (first == second, r.pullRequest("grit/w1")) ==> (
          false,
          second.map(at => Some(RemoteRepository.PullRequest.Open(at)))
        )
      }
    }
    test("a refusal to open other than one already open is Failed with GitHub's message") {
      over { fake =>
        fake.answering(
          "repos/acme/app/pulls",
          422,
          """{"message":"Validation Failed","errors":[{"message":"No commits between main and grit/w1"}]}"""
        )
        fake.repository().open("grit/w1", "Fix it", "Fixed.") ==>
          Left(HostError.Failed("GitHub answered 422: Validation Failed"))
      }
    }
    test("a failure names GitHub's status and its message, control characters dropped, cut") {
      over { fake =>
        fake.answering(
          "repos/acme/app",
          500,
          ujson.write(ujson.Obj("message" -> ("Bo\u0007om" + "x" * 300)))
        )
        fake.answering("repos/acme/app/pulls", 503, "<html>down</html>")
        (
          fake.repository().open("grit/w1", "Fix it", "Fixed."),
          fake.repository().pullRequest("grit/w1")
        ) ==> (
          Left(HostError.Failed("GitHub answered 500: Boom" + "x" * 196)),
          Left(HostError.Failed("GitHub answered 503"))
        )
      }
    }
    test("a redirect is never followed: Failed, and nothing more is asked") {
      over { fake =>
        fake.answering("repos/acme/app/pulls", 301, "", Map("Location" -> s"${fake.api}elsewhere"))
        (fake.repository().pullRequest("grit/w1"), fake.requests.size) ==>
          (Left(HostError.Failed("GitHub answered 301")), 1)
      }
    }
    test("an answer past BodyMost bytes is Failed") {
      over { fake =>
        fake.answering("repos/acme/app/pulls", 200, "[" + " " * GitHubRepository.BodyMost + "]")
        fake.repository().pullRequest("grit/w1") ==>
          Left(HostError.Failed(s"GitHub's answer passed ${GitHubRepository.BodyMost} bytes"))
      }
    }
    test("a request without the token GitHub takes is Failed, the token never named") {
      over { fake =>
        val wrong = GitHubRepository
          .of(fake.api, "acme/app", authorizer, Clock.system()) match {
          case Right(r) => r
          case Left(why) => throw new java.lang.AssertionError(why)
        }
        wrong.pullRequest("grit/w1") ==> Left(
          HostError.Failed("GitHub answered 401: Bad credentials")
        )
      }
    }
    test("a repository is https, or http on a loopback host, named by plain segments") {
      def of(api: String, name: String) =
        GitHubRepository.of(URI.create(api), name, authorizer, Clock.system()) match {
          case Right(r) => Right(r.pushedTo)
          case Left(why) => Left(why)
        }
      (
        of("https://api.github.com/", "acme/app"),
        of("http://api.github.com/", "acme/app").isLeft,
        of("https://api.github.com/", "acme/..").isLeft,
        of("https://api.github.com/", "ac me/app").isLeft,
        of("https://api.github.com/", "acme/app/x").isLeft
      ) ==> (Right("https://github.com/acme/app.git"), true, true, true, true)
    }
  }
}
