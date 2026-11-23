package grit.kit.run

import java.nio.file.Files

import grit.core.clock.Clock
import grit.core.place.{Namespace, Place}
import grit.core.secret.{TestSecrets, Variable}
import grit.host.checkout.Redaction
import grit.host.git.{GitFixture, WorkingCopies}

import utest.*

/** [[Pushing]] made from a clone's path, over a real clone and its bare origin. */
object PushingTests extends TestSuite {

  val tests = Tests {
    test(
      "git pushing a run's branch is handed no secret: neither the token's variable nor any other its environment holds"
    ) {
      GitFixture.inRepository { repo =>
        val copies =
          new WorkingCopies(
            GitFixture.repository,
            repo.source,
            repo.root,
            repo.processes,
            Clock.system()
          )
        val work = GitFixture.work("w1")
        val copy =
          copies.opened(work).fold(e => throw new java.lang.AssertionError(e.message), identity)
        val _ = GitFixture.git(
          repo,
          copy,
          "-c",
          "user.name=t",
          "-c",
          "user.email=t@example.com",
          "commit",
          "--allow-empty",
          "-qm",
          "in copy"
        )
        // The bare origin's own hook sees what its receiving git inherited from the push.
        val seen = repo.root.resolveSibling("seen.txt")
        val hook = repo.origin.resolve("hooks").resolve("pre-receive")
        Files.writeString(hook, s"#!/bin/sh\nenv > '$seen'\n")
        val _ = hook.toFile.setExecutable(true)
        val planted = repo.environment ++ Map(
          "GRIT_REMOTE_REPOSITORY_TOKEN" -> "ghs_plantedRemoteToken123",
          "ANTHROPIC_API_KEY" -> "sk-ant-plantedKey0123456",
          "LANG" -> "C.UTF-8"
        )
        val pushing = Pushing.of(
          repo.source,
          GitFixture.targetOf(new GitFixture.FakeRemoteRepository(repo.origin.toString)),
          TestSecrets.of(Variable("GRIT_REMOTE_REPOSITORY_TOKEN"), "ghs_plantedRemoteToken123"),
          planted,
          Redaction.of(Map.empty),
          Place.under(Namespace.Service, Vector("github", "acme", "app")),
          "acme/app"
        ) match {
          case Right(p) => p
          case Left(why) => throw new java.lang.AssertionError(why)
        }
        val offered = pushing.branch(work).offer("Fix it", "Fixed.")
        val env = if (Files.exists(seen)) Files.readString(seen) else ""
        (
          offered.isRight,
          env.contains("LANG=C.UTF-8"),
          Vector(
            "GRIT_REMOTE_REPOSITORY_TOKEN",
            "ghs_planted",
            "ANTHROPIC_API_KEY",
            "sk-ant-planted"
          )
            .filter(env.contains)
        ) ==> (true, true, Vector.empty)
      }
    }
    test("a run's branch is pushed from the clone Pushing was made at, whatever its copy holds") {
      GitFixture.inRepository { repo =>
        val copies =
          new WorkingCopies(
            GitFixture.repository,
            repo.source,
            repo.root,
            repo.processes,
            Clock.system()
          )
        val work = GitFixture.work("w1")
        val copy =
          copies.opened(work).fold(e => throw new java.lang.AssertionError(e.message), identity)
        val _ = GitFixture.git(
          repo,
          copy,
          "-c",
          "user.name=t",
          "-c",
          "user.email=t@example.com",
          "commit",
          "--allow-empty",
          "-qm",
          "in copy"
        )
        val planted = Files.createDirectories(repo.root.resolveSibling("planted"))
        Files.writeString(copy.resolve(".git"), s"gitdir: $planted\n")
        val pushing = Pushing.of(
          repo.source,
          GitFixture.targetOf(new GitFixture.FakeRemoteRepository(repo.origin.toString)),
          TestSecrets.of(Variable("GRIT_REMOTE_REPOSITORY_TOKEN"), "ghs_plantedRemoteToken123"),
          repo.environment,
          Redaction.of(Map.empty),
          Place.under(Namespace.Service, Vector("github", "acme", "app")),
          "acme/app"
        ) match {
          case Right(p) => p
          case Left(why) => throw new java.lang.AssertionError(why)
        }
        val offered = pushing.branch(work).offer("Fix it", "Fixed.")
        // None where the ref is not there, as in an origin never pushed to.
        def tip(in: java.nio.file.Path, ref: String): Option[String] =
          scala.util.Try(GitFixture.git(repo, in, "rev-parse", ref).output.text.trim).toOption
        (offered.isRight, tip(repo.origin, "refs/heads/grit/w1")) ==>
          (true, tip(repo.source, "grit/w1"))
      }
    }
  }
}
