package grit.host.checkout

import java.nio.file.Files

import grit.core.host.{Argv, RelPath, Runs, RunsContract}

/** The runs contract, kept by [[LocalRuns]] over real programs in a temporary checkout. */
object LocalRunsTests extends RunsContract {

  protected def reading[A](body: (Runs^, RelPath => Option[String]) => A): A =
    LocalRunsFilesTests.inCheckout { root =>
      val runs = LocalRunsFilesTests.over(root)
      def held(file: RelPath): Option[String] =
        Some(root.resolve(RelPath.value(file))).filter(Files.isRegularFile(_)).map(Files.readString)
      try body(runs, held)
      finally runs.close()
    }

  protected val quick: Argv = LocalRunsFilesTests.sh("echo hi; exit 3")
  protected val slow: Argv = LocalRunsFilesTests.sh("echo started; exec sleep 30")
  protected val later: Argv = LocalRunsFilesTests.sh("sleep 0.3; echo done")
  protected val flood: Argv = LocalRunsFilesTests.sh("head -c 17000000 /dev/zero")
  protected val missing: Argv =
    Argv.of("grit-no-such-program").getOrElse(throw new IllegalArgumentException("missing"))
}
