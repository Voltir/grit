package grit.app.config

import java.nio.file.Path

import grit.core.edge.Console
import grit.core.place.Repository

import utest.*

object RepositoriesEnvTests extends TestSuite {

  private def repository(name: String): Repository =
    Repository.of(name).getOrElse(throw new java.lang.AssertionError(name))

  val tests = Tests {
    test(
      "GRIT_REPOSITORIES declares name=path pairs; unset declares none; a pair that is not name=path, a bad name or a name twice is refused"
    ) {
      (
        RepositoriesEnv.declared(Map("GRIT_REPOSITORIES" -> " grit=/src/grit, app = /src/app ")),
        RepositoriesEnv.declared(Map.empty),
        RepositoriesEnv.declared(Map("GRIT_REPOSITORIES" -> "grit")),
        RepositoriesEnv
          .declared(Map("GRIT_REPOSITORIES" -> "my-app=/src/app"))
          .left
          .map(
            _.takeWhile(_ != ':')
          ),
        RepositoriesEnv.declared(Map("GRIT_REPOSITORIES" -> "grit=/a,grit=/b"))
      ) ==> (
        Right(
          Vector(
            repository("grit") -> Path.of("/src/grit"),
            repository("app") -> Path.of("/src/app")
          )
        ),
        Right(Vector.empty),
        Left("GRIT_REPOSITORIES: 'grit' is not name=path"),
        Left("GRIT_REPOSITORIES"),
        Left("GRIT_REPOSITORIES declares grit twice")
      )
    }

    test(
      "working copies are made under GRIT_COPIES, else XDG_DATA_HOME's grit/copies, else HOME's"
    ) {
      (
        RepositoriesEnv.copies(Map("GRIT_COPIES" -> "/c", "XDG_DATA_HOME" -> "/x", "HOME" -> "/h")),
        RepositoriesEnv.copies(Map("XDG_DATA_HOME" -> "/x", "HOME" -> "/h")),
        RepositoriesEnv.copies(Map("XDG_DATA_HOME" -> "", "HOME" -> "/h")),
        RepositoriesEnv.copies(Map.empty)
      ) ==> (
        Right(Path.of("/c")),
        Right(Path.of("/x/grit/copies")),
        Right(Path.of("/h/.local/share/grit/copies")),
        Left("working copies need a directory: set GRIT_COPIES, XDG_DATA_HOME or HOME")
      )
    }
    test(
      "--in's repository is found among those declared; one not declared is refused naming those that are"
    ) {
      val declared =
        Vector(repository("grit") -> Path.of("/src/grit"), repository("app") -> Path.of("/a"))
      (
        RepositoriesEnv.cloneOf(declared, repository("app")),
        RepositoriesEnv.cloneOf(declared, repository("other")),
        RepositoriesEnv.cloneOf(Vector.empty, repository("other"))
      ) ==> (
        Right(Path.of("/a")),
        Left("GRIT_REPOSITORIES declares no repository other; it declares grit, app"),
        Left("GRIT_REPOSITORIES declares no repository other; it declares none")
      )
    }

    test("a run in a working copy goes on only on an engine this process runs") {
      (
        RepositoriesEnv.startedHere(Console.Runner.Here),
        RepositoriesEnv.startedHere(Console.Runner.Elsewhere(None))
      ) ==> (
        Right(()),
        Left(
          "grit work --in runs only on an engine it starts, and another process runs this " +
            "database's engine, which may not believe the repository's files as this one does: " +
            "stop it, or run without --in"
        )
      )
    }
  }
}
