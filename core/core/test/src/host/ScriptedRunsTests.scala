package grit.core.host

import scala.concurrent.duration.*

/** The runs contract, kept by the scripted runs. */
object ScriptedRunsTests extends RunsContract {

  private def argv(program: String): Argv =
    Argv.of(program).getOrElse(throw new IllegalArgumentException(program))

  protected val quick: Argv = argv("quick")
  protected val slow: Argv = argv("slow")
  protected val later: Argv = argv("later")
  protected val flood: Argv = argv("flood")

  private val scripts = Map(
    quick -> ScriptedRuns.Script("hi\n", "", Some(Duration.Zero), 3),
    slow -> ScriptedRuns.Script("started\n", "", None, 0),
    later -> ScriptedRuns.Script("", "done\n", Some(300.millis), 0),
    flood -> ScriptedRuns.Script("", "", Some(Duration.Zero), 0, flood = true)
  )

  protected def fresh[A](body: Runs^ => A): A = body(new ScriptedRuns(scripts))
}
