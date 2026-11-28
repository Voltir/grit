package grit.app.main

import grit.core.host.Argv
import grit.kit.deployment.Offered

import utest.*

/** The command each allowlisted commit must pass first, as `GRIT_COMPILE` names it. */
object CompileChoiceTests extends TestSuite {

  private val allowlisted = Offered.Allowlisted(Offered.ReferenceAllowlist)

  private def compile(offered: Offered, env: (String, String)*): Either[String, Option[Argv]] =
    RunChoices.compileChoice(env.toMap, offered)

  val tests = Tests {
    test(
      "GRIT_COMPILE: unset is none; set, its words split at whitespace, when the allowlist " +
        "permits them"
    ) {
      (
        compile(allowlisted),
        compile(allowlisted, "GRIT_COMPILE" -> " ./mill  --ticker false __.compile "),
        compile(Offered.Read)
      ) ==> (
        Right(None),
        Argv.of("./mill", "--ticker", "false", "__.compile").map(Some(_)),
        Right(None)
      )
    }

    test(
      "GRIT_COMPILE is refused when the allowlist does not permit it, when the tools are not " +
        "allowlisted, so no commit is checked, and when it names no command"
    ) {
      (
        compile(allowlisted, "GRIT_COMPILE" -> "make all"),
        compile(Offered.Read, "GRIT_COMPILE" -> "./mill __.compile"),
        compile(Offered.All, "GRIT_COMPILE" -> "./mill __.compile"),
        compile(allowlisted, "GRIT_COMPILE" -> "  ")
      ) ==> (
        Left("GRIT_COMPILE: make all is not on the allowlist"),
        Left("GRIT_COMPILE: GRIT_TOOLS is not allowlist, so no commit is checked"),
        Left("GRIT_COMPILE: GRIT_TOOLS is not allowlist, so no commit is checked"),
        Left("GRIT_COMPILE names no command")
      )
    }
  }
}
