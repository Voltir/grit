package grit.commands.routes

import scala.concurrent.duration.*

import grit.commands.declare.HoldingWatch
import grit.commands.machine.Worktrees
import grit.core.document.DocRef
import grit.core.edge.{
  CommandError,
  EdgeContracts,
  EdgeWatch,
  ScriptedWatch,
  WatchError,
  WatchFor,
  Watched
}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, Harness, PluginName, SessionId}
import grit.core.place.Directory
import grit.core.plugin.CommandOutcome
import grit.core.session.Event
import grit.core.tool.ToolName
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `POST /wait`: a watch read from its body, waited on as the caller, its outcome answered. */
object WaitRouteTests extends TestSuite {

  private val Plugin: PluginName = got(PluginName.of("tracked"))
  private val Review: DocRef = got(DocRef.of("state", "review"))

  /** Reads `{"document": "<key>"}` as a wait for it to hold [[Review]]. */
  private val holding = HoldingWatch(
    Set("document"),
    (_, body) =>
      body.value.get("document") match {
        case None => Right(None)
        case Some(d) =>
          d.strOpt
            .toRight("document is a key")
            .flatMap(DocKey.of)
            .flatMap(WatchFor.Holding.of(Plugin, _, Set(Review)))
            .map(Some(_))
            .left
            .map(CommandError.Unread(Resolve, _))
      },
    met
  )

  private def met(h: WatchFor.Holding, held: Watched.Held): String =
    s"${DocKey.value(h.key)} holds ${held.refs.map(_.ref).mkString} at version ${held.version}"

  private val Resolve = ToolName("resolve")

  /** Reads `{"named": "<name>"}` as a wait for the key `resolve` answers it with to hold
    * [[Review]]; `resolve` refusing, unread, saying why.
    */
  private val resolving = HoldingWatch(
    Set("named"),
    (ran, body) =>
      body.value.get("named") match {
        case None => Right(None)
        case Some(n) =>
          ran(Resolve, ujson.Obj("text" -> n)).flatMap {
            case CommandOutcome.Done(key) =>
              DocKey
                .of(key)
                .flatMap(WatchFor.Holding.of(Plugin, _, Set(Review)))
                .map(Some(_))
                .left
                .map(CommandError.Unread(Resolve, _))
            case CommandOutcome.Refused(why) => Left(CommandError.Unread(Resolve, why))
          }
      },
    met
  )

  private def waiting(script: ScriptedWatch.Script, vouched: Boolean = true): Routes^ =
    routed(
      watch = new ScriptedWatch(if (vouched) Set(Owner.account) else Set.empty, script),
      holding = Some(holding)
    )

  private def waited(r: Routes^, body: ujson.Obj) = {
    val got = r.answer(request(Verb.Post, "/wait", ujson.write(body)))
    (got.status, got.body)
  }

  val tests = Tests {
    test("a commit is waited on in a worktree at the caller's machine's place, or by a holder") {
      val script = new ScriptedWatch.Script
      val r = waiting(script)
      val since = Now.minusSeconds(60)
      waited(
        r,
        ujson.Obj(
          "commit" -> ujson.Obj("worktree" -> "/work/c"),
          "since" -> since.toString,
          "within" -> 0
        )
      )
      waited(r, ujson.Obj("commit" -> ujson.Obj("holder" -> "desktop/fixer")))
      script.asked.map(a => (a.by, a.room, a.what, a.within)) ==> Vector(
        (
          Owner.account,
          EdgeContracts.Room,
          WatchFor.Committed(
            WatchFor.Committer.At(Worktrees.at(Machine, got(Directory.of("/work/c")))),
            since
          ),
          Duration.Zero
        ),
        (
          Owner.account,
          EdgeContracts.Room,
          WatchFor.Committed(WatchFor.Committer.As(got(ClaimHolder.of("desktop/fixer"))), Now),
          Routes.Within
        )
      )
    }

    test("a watch waits the whole seconds `within` asks, at most the longest a watch waits") {
      val script = new ScriptedWatch.Script
      val r = waiting(script)
      Vector(60, 3600, 1e10, 1e300).foreach(s =>
        waited(r, ujson.Obj("document" -> "grit/plan", "within" -> s))
      )
      val plan = got(WatchFor.Holding.of(Plugin, got(DocKey.of("grit/plan")), Set(Review)))
      script.asked.map(a => (a.what, a.within)) ==> Vector(
        plan -> 60.seconds,
        plan -> EdgeWatch.Longest,
        plan -> EdgeWatch.Longest,
        plan -> EdgeWatch.Longest
      )
    }

    test("a GET is 405, and nothing is waited on") {
      val script = new ScriptedWatch.Script
      val got = waiting(script).answer(
        request(Verb.Get, "/wait", ujson.write(ujson.Obj("commit" -> ujson.Obj("holder" -> "h"))))
      )
      got.status ==> Status.NotAllowed(Vector(Verb.Post))
      script.asked ==> Vector.empty
    }

    test("a body this edge does not read is 400, saying why, and nothing is waited on") {
      val script = new ScriptedWatch.Script
      val r = waiting(script)
      Vector(
        ujson.Obj("commit" -> ujson.Obj("worktree" -> "work/c")),
        ujson.Obj("commit" -> ujson.Obj("worktree" -> "/a", "holder" -> "h")),
        ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "since" -> "yesterday"),
        ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "within" -> -1),
        ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "within" -> 1.5),
        ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "within" -> "60"),
        ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "sinse" -> Now.toString),
        ujson.Obj("document" -> "grit/plan", "since" -> Now.toString),
        ujson.Obj("document" -> 7),
        ujson.Obj("nothing" -> true)
      ).map(waited(r, _)) ==> Vector(
        (Status.BadRequest, "worktree is an absolute path"),
        (Status.BadRequest, "commit names a worktree or a holder"),
        (Status.BadRequest, "since is an ISO-8601 instant"),
        (Status.BadRequest, "within is whole seconds, 0 or more"),
        (Status.BadRequest, "within is whole seconds, 0 or more"),
        (Status.BadRequest, "within is whole seconds, 0 or more"),
        (Status.BadRequest, "this watch reads no sinse: only commit, since, within"),
        (Status.BadRequest, "this watch reads no since: only document, within"),
        (Status.BadRequest, "document is a key"),
        // Its keys are read before the holding watch is, which may run a command.
        (Status.BadRequest, "this watch reads no nothing: only document, within")
      )
      script.asked ==> Vector.empty
    }

    test(
      "what met a watch is answered: a commit by its sha, subject and worktree, a holding as met says"
    ) {
      val script = new ScriptedWatch.Script
      val worktree = Worktrees.at(Machine, got(Directory.of("/work/c")))
      val session = got(SessionId.of(got(Harness.of("made-up")), "s1"))
      script.commit(
        session,
        None,
        worktree,
        Now,
        got(Event.Shape.committed(worktree, "0123456789abcdef", Some("feat: a thing")))
      )
      val version = script.hold(Plugin, got(DocKey.of("grit/plan")), Set(Review), Now)
      val r = waiting(script)
      (
        waited(r, ujson.Obj("commit" -> ujson.Obj("worktree" -> "/work/c"), "within" -> 0)),
        waited(r, ujson.Obj("document" -> "grit/plan", "within" -> 0))
      ) ==> (
        (
          Status.Ok,
          s"committed 0123456789ab feat: a thing in machine:desktop/work/c, on session s1 at $Now"
        ),
        (Status.Ok, s"grit/plan holds review at version $version")
      )
    }
    test(
      "a holding watch reads its body through the caller's commands"
    ) {
      val script = new ScriptedWatch.Script
      val version = script.hold(Plugin, got(DocKey.of("grit/7")), Set(Review), Now)
      val r = routed(
        commands = scripted(Vector(listed("resolve", reads = true))) { (_, args) =>
          args.obj.get("text").flatMap(_.strOpt) match {
            case Some("grit/plan") => Right(CommandOutcome.Done("grit/7"))
            case Some("grit/gone") => Left(CommandError.Unavailable("the database is down"))
            case other =>
              Right(CommandOutcome.Refused(s"project grit has no artifact ${other.getOrElse("")}"))
          }
        },
        watch = new ScriptedWatch(Set(Owner.account), script),
        holding = Some(resolving)
      )
      Vector("grit/plan", "grit/none", "grit/gone").map(n =>
        waited(r, ujson.Obj("named" -> n, "within" -> 0))
      ) ==> Vector(
        (Status.Ok, s"grit/7 holds review at version $version"),
        (Status.BadRequest, "project grit has no artifact grit/none"),
        (Status.Unavailable, "the database is unavailable: the database is down")
      )
    }

    test(
      "a lapsed watch is 408; a busy or unavailable one 503; an account no person is linked to 403"
    ) {
      val script = new ScriptedWatch.Script
      val body = ujson.Obj("commit" -> ujson.Obj("holder" -> "h"), "within" -> 0)
      val lapsed = waited(waiting(script), body)
      script.refusing(Some(WatchError.Busy))
      val busy = waited(waiting(script), body)
      script.refusing(Some(WatchError.Unavailable("connection refused")))
      val unavailable = waited(waiting(script), body)
      script.refusing(None)
      Vector(lapsed, busy, unavailable, waited(waiting(script, vouched = false), body)) ==> Vector(
        (Status.Timeout, "nothing met the watch within 0s"),
        (
          Status.Unavailable,
          s"${EdgeWatch.Most} watches are under way already: wait for one to end"
        ),
        (Status.Unavailable, "the database is unavailable: connection refused"),
        (Status.Forbidden, "no person is linked to slack:T1/U-reader")
      )
    }
  }
}
