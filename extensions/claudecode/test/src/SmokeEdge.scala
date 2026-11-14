package grit.claudecode

import java.time.Instant

import scala.io.StdIn

import grit.commands.routes.RouteFixtures.{Worker, got}
import grit.commands.worker.Holders
import grit.core.clock.Clock
import grit.core.edge.ScriptedSessions
import grit.core.id.ClaimHolder
import grit.core.place.Directory
import grit.core.session.Event

/** The edge for a smoke run of Claude Code by hand, on the system's clock over scripted sessions:
  * `SmokeEdge <checkout> <brief>` keeps `<brief>` as the brief of a session in `<checkout>`,
  * prints the edge's address, then, at each line read from stdin and at its end, everything the
  * edge recorded and how many briefs it read; it closes at stdin's end. README.md, "Smoke run".
  */
object SmokeEdge {

  def main(args: Array[String]): Unit = args match {
    case Array(checkout, brief) =>
      val script = new ScriptedSessions.Script
      val holder = Holders.of(Worker, got(Directory.of(checkout)), None)
      script.brief(holder, ScriptedSessions.document(brief, Instant.now().minusSeconds(60)))
      HookFixtures.served(script, Clock.system()) { port =>
        println(s"http://127.0.0.1:$port")
        while (Option(StdIn.readLine()).isDefined) report(script)
        report(script)
      }
    case _ => System.err.println("usage: SmokeEdge <checkout> <brief>")
  }

  private def report(script: ScriptedSessions.Script): Unit = {
    val calls = script.called
    calls.foreach {
      case ScriptedSessions.Call.Recorded(_, _, batches) =>
        batches.foreach { b =>
          b.events.foreach { e =>
            println(
              s"${b.id} ${b.holder.fold("-")(ClaimHolder.value)} ${e.n} ${e.at} ${e.kind.name} ${Event.Shape.value(e.shape)}"
            )
          }
        }
      case _ => ()
    }
    println(s"briefs read: ${calls.count(_.isInstanceOf[ScriptedSessions.Call.Briefed])}")
  }
}
