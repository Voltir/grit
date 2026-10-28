package grit.projects.plugin

import java.time.Instant

import grit.core.document.{DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.need.PluginReads
import grit.core.plugin.{Answered, InMemoryPlugins, KeepingCommand, ReadingCommand}
import grit.core.store.Tx
import grit.core.tool.ToolName
import grit.dbos.sql.TestTx
import grit.projects.method.{Method, MethodFixtures, TransitionChecks}
import grit.projects.name.MethodName

/** A projects plugin over an in-memory store, and its commands asked as an edge asks them, for
  * the plugin package's suites.
  */
object CommandFixtures {

  import MethodFixtures.{change, note, transitions, views}

  val at: Instant = Instant.parse("2026-10-27T12:00:00Z")

  val person: Principal.Person =
    TestAccounts.principal(TestAccounts.account("slack:T1/U1")) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("not a person")
    }

  /** What every write by [[person]] records as its author. */
  val by: String = "slack:T1/U1"

  val methodName: MethodName = MethodFixtures.method

  def method(moves: Vector[grit.projects.method.Transition] = transitions): Method =
    Method
      .of(methodName, 1, Vector(change, note), moves, views, TransitionChecks.all)
      .fold(why => throw new java.lang.AssertionError(why.toString), identity)

  def plugin(m: Method = method()): Projects =
    Projects
      .of(Vector(m -> Vector.empty), methodName)
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** A fresh store, and its projects keeper under `terms`. */
  final class Store(terms: DocumentTerms = Projects.Terms) {
    val documents = new InMemoryDocuments
    val keeper: DocumentKeeper = documents.keeper(Projects.Name, terms)
    def reads: PluginReads = PluginReads(new InMemoryPlugins().docs(Projects.Name), keeper)
    def count: Int =
      inTx(keeper.newest(100_000)).fold(e => throw new java.lang.AssertionError(e.toString), _.size)
  }

  def inTx[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  /** The answer to `name` with `args`, read and run as an edge runs a command. */
  def ask(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): Answered = askAt(at)(projects, store, name, args*)

  /** As [[ask]], asked at `when`. */
  def askAt(when: Instant)(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): Answered =
    projects.commands.find(c => ToolName.value(c.spec.name) == name) match {
      case Some(c: KeepingCommand[a]) =>
        val read = c.spec.args.read(ujson.Obj.from(args))
        read.fold(
          e => throw new java.lang.AssertionError(e.message),
          a =>
            inTx(c.run(a, person, store.keeper, when))
              .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        )
      case Some(c: ReadingCommand[a]) =>
        val read = c.spec.args.read(ujson.Obj.from(args))
        read.fold(
          e => throw new java.lang.AssertionError(e.message),
          a =>
            inTx(c.run(a, person, store.reads, when))
              .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        )
      case _ => throw new java.lang.AssertionError(s"no command $name")
    }

  def done(answer: Answered): String = answer match {
    case Answered.Done(text) => text
    case Answered.Refused(why) => throw new java.lang.AssertionError(s"refused: $why")
  }

  def started(projects: Projects, store: Store): Unit =
    done(ask(projects, store, "project_new", "project" -> "p"))

  def kept(
      projects: Projects,
      store: Store,
      local: String,
      kind: String,
      body: String
  ): Unit =
    done(
      ask(
        projects,
        store,
        "project_keep",
        "key" -> s"p/$local",
        "kind" -> kind,
        "title" -> s"Title $local",
        "body" -> body
      )
    )

  def shown(projects: Projects, store: Store, key: String): String =
    done(ask(projects, store, "project_show", "key" -> key))
}
