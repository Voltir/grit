package grit.host.checkout

import grit.core.host.{Argv, Runs, RunsContract}

/** The runs contract, kept by [[LocalRuns]] over real programs in a temporary checkout. */
object LocalRunsTests extends RunsContract {

  protected def fresh[A](body: Runs^ => A): A = LocalRunsFilesTests.inCheckout { root =>
    val runs = LocalRunsFilesTests.over(root)
    try body(runs)
    finally runs.close()
  }

  protected val quick: Argv = LocalRunsFilesTests.sh("echo hi; exit 3")
  protected val slow: Argv = LocalRunsFilesTests.sh("echo started; exec sleep 30")
  protected val later: Argv = LocalRunsFilesTests.sh("sleep 0.3; echo done")
  protected val flood: Argv = LocalRunsFilesTests.sh("head -c 17000000 /dev/zero")
}
