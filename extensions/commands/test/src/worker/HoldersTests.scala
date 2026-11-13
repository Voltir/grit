package grit.commands.worker

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.place.Directory

import utest.*

/** [[Holders.of]]: the holder a session or an agent it starts works as, readable and total. */
object HoldersTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  private val Desktop = got(WorkerName.of("desktop"))
  private val Grit = got(Directory.of("/work/grit"))
  private val Session = got(Harness.of("claude-code").flatMap(SessionId.of(_, "s-1")))

  private def holder(
      worker: WorkerName = Desktop,
      dir: Directory = Grit,
      agent: Option[Holders.Agent] = None
  ): String = ClaimHolder.value(Holders.of(worker, dir, agent))

  val tests = Tests {
    test("a session's holder is its worker, then its directory's last segment and hash") {
      Vector(
        holder(),
        holder(worker = got(WorkerName.of("laptop"))),
        holder(dir = got(Directory.of("/home/nick/Projects/Grit Worktree"))),
        holder(dir = got(Directory.of("/")))
      ) ==> Vector(
        "desktop/grit-dcfe7476",
        "laptop/grit-dcfe7476",
        "desktop/grit-worktree-403a8737",
        "desktop/8a5edab2"
      )
    }

    test(
      "an agent's holder adds its type, by its own hash when slugging changed it, and its starting session's hash"
    ) {
      Vector("coder-opus", "Plan Mode", "日本").map(t =>
        holder(agent = Some(Holders.Agent(t, Session)))
      ) ==> Vector(
        "desktop/grit-dcfe7476/coder-opus/0e77e4b3dbc2",
        "desktop/grit-dcfe7476/plan-mode-412d7e24/0e77e4b3dbc2",
        "desktop/grit-dcfe7476/cf2abf0c/0e77e4b3dbc2"
      )
    }

    test("the longest inputs give a holder that fits, each part cut to its slug's length") {
      val longest = holder(
        got(WorkerName.of("w" * WorkerName.Longest)),
        got(Directory.of("/" + "d" * 200)),
        Some(
          Holders.Agent(
            "t" * 300,
            got(SessionId.of(got(Harness.of("h" * Harness.Longest)), "s" * SessionId.Longest))
          )
        )
      )
      longest.split('/').toVector.map(_.length) ==>
        Vector(WorkerName.Longest, Holders.Slugged + 9, Holders.Slugged + 9, 12)
    }
  }
}
