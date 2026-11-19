package grit.projects.plugin

import java.time.Instant

import grit.core.claim.{Claims, InMemoryClaims}
import grit.core.document.{Document, DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.id.{ClaimHolder, DocKey}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome, InMemoryPlugins, KeepingCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Visibility}
import grit.dbos.sql.TestTx
import grit.projects.method.{
  ArtifactKind,
  Example,
  Method,
  MethodFixtures,
  MethodJson,
  RoleView,
  TransitionChecks
}
import grit.projects.name.{LocalKey, MethodName}

/** A projects plugin over an in-memory store, and its commands asked as an edge asks them, for
  * the plugin package's suites.
  */
object CommandFixtures {

  import MethodFixtures.{change, note, transitions, views}

  val at: Instant = Instant.parse("2026-10-27T12:00:00Z")

  private def personOf(account: String): Principal.Person =
    TestAccounts.principal(TestAccounts.account(account)) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("not a person")
    }

  /** Who asks, unless a test asks as [[other]]. */
  val person: Principal.Person = personOf("slack:T1/U1")

  /** What every write by [[person]] records as its author. */
  val by: String = "slack:T1/U1"

  /** A second person, for a test telling who wrote what. */
  val other: Principal.Person = personOf("slack:T1/U2")

  /** What every write by [[other]] records as its author. */
  val otherBy: String = "slack:T1/U2"

  val methodName: MethodName = MethodFixtures.method

  /** Version `version` of [[methodName]], of `kinds`, `moves` and `roles`: by default version 1
    * of change and note, their moves and a builder of changes ([[MethodFixtures]]), checked by
    * every check grit has.
    */
  def method(
      moves: Vector[grit.projects.method.Transition] = transitions,
      version: Int = 1,
      kinds: Vector[ArtifactKind] = Vector(change, note),
      roles: Vector[RoleView] = views
  ): Method =
    Method
      .of(methodName, version, kinds, moves, roles, TransitionChecks.all)
      .fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** [[method]]'s kinds and moves, its one role a manager, shown notes to depth 1 and always
    * carrying `<project>/process`.
    */
  val managing: Method = method(roles =
    Vector(
      MethodFixtures.right(
        RoleView.of(
          MethodFixtures.role("manager"),
          Set(note.name),
          1,
          Set(MethodFixtures.right(LocalKey.of("process")))
        )
      )
    )
  )

  /** The brief kept for `holder`, as a window would find it at `when`. */
  def briefOf(store: Store, holder: String, when: Instant = at.plusSeconds(1)): Option[Document] = {
    inTx(store.documents.declare(Vector(Projects.Name -> Projects.Terms)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    inTx(store.documents.briefOf(MethodFixtures.right(ClaimHolder.of(holder)), when))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

  def plugin(turns: TurnCommands = TurnCommands.Shipped): Projects =
    Projects
      .of(TransitionChecks.all, methodName, turns)
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** `m` and its worked `examples` as `project_declare` takes them. */
  def declaring(m: Method, examples: Vector[Example] = Vector.empty): (String, ujson.Value) =
    "method" -> ujson.write(MethodJson.written(m, examples))

  /** A fresh store, its projects keeper under `terms`, and its projects claims, `declared`
    * declared in it through `project_declare`.
    */
  final class Store(
      terms: DocumentTerms = Projects.Terms,
      declared: Vector[Method] = Vector(method())
  ) {
    val documents = new InMemoryDocuments
    val keeper: DocumentKeeper = documents.keeper(Projects.Name, terms)
    val claims: Claims = new InMemoryClaims().of(Projects.Name, _ => None)
    declared.foreach(m => done(ask(plugin(), this, "project_declare", declaring(m))))
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
  ): CommandOutcome = askAs(person, at)(projects, store, name, args*)

  /** As [[ask]], asked at `when`. */
  def askAt(when: Instant)(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome = askAs(person, when)(projects, store, name, args*)

  /** As [[ask]], asked by `who` through an edge at `when`. */
  def askAs(who: Principal.Person, when: Instant = at)(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome = askBy(direct(who), when)(projects, store, name, args*)

  /** `who` giving a command directly, believed at `Authority`, as the local operator is. */
  def direct(who: Principal.Person = person): CommandBy =
    CommandBy.Direct(who, Integrity.Authority)

  /** As [[ask]], asked for `who` at `when`, from a room `cleared` (public unless given). */
  def askBy(who: CommandBy, when: Instant = at, cleared: Clearance = Clearance.of(Label.Public))(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome = askThrough(store.keeper, store.claims, who, when, cleared)(
    projects,
    store,
    name,
    args*
  )

  /** As [[askBy]], a keeping command writing through `keeper` and claiming through `claims` in
    * place of `store`'s, run as an edge or a turn runs it ([[KeepingCommand.run]]) in `room`
    * under `visibility`: a direct caller handed their own integrity met with `room`'s cap, each
    * document believed at no more than that (a turn's, its call `stamped` met with the cap), and
    * what `store` wrote kept only when it answers `Done`.
    */
  def askThrough(
      keeper: DocumentKeeper,
      claims: Claims,
      who: CommandBy = direct(),
      when: Instant = at,
      cleared: Clearance = Clearance.of(Label.Public),
      room: Place = Place.Everywhere,
      visibility: Visibility = Visibility.Shipped,
      stamped: Integrity = Integrity.Authority
  )(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome = {
    given Tx = TestTx.fake(cleared, visibility)
    // As the edge does, a direct caller is handed their integrity met with the room's cap.
    val (by, believed) = who match {
      case CommandBy.Direct(p, b) =>
        val most = visibility.believedIn(room, b)
        (CommandBy.Direct(p, most), most)
      case turn: CommandBy.Turn => (turn, stamped)
    }
    projects.commands.find(c => ToolName.value(c.spec.name) == name) match {
      case Some(c: KeepingCommand[a]) =>
        val read = c.spec.args.read(ujson.Obj.from(args))
        read.fold(
          e => throw new java.lang.AssertionError(e.message),
          a => {
            // A transaction a refusal rolls back: a refusal leaves through `Left`, undoing it.
            var refused: Option[CommandOutcome] = None
            store.documents.savepoints
              .atomic(
                KeepingCommand
                  .run(c, a, by, keeper, claims, room, believed, when)
                  .flatMap {
                    case done: CommandOutcome.Done => Right(done)
                    case no: CommandOutcome.Refused =>
                      refused = Some(no)
                      Left(StoreError.Invalid("refused"))
                  }
              )
              .fold(
                e => refused.getOrElse(throw new java.lang.AssertionError(e.toString)),
                identity
              )
          }
        )
      case Some(c: ReadingCommand[a]) =>
        val read = c.spec.args.read(ujson.Obj.from(args))
        read.fold(
          e => throw new java.lang.AssertionError(e.message),
          a =>
            c.run(a, by, store.reads, when)
              .fold(e => throw new java.lang.AssertionError(e.toString), identity)
        )
      case _ => throw new java.lang.AssertionError(s"no command $name")
    }
  }

  /** As [[ask]], asked by `who` believed at `believed`, in a room no cap covers. */
  def askBelieved(believed: Integrity, who: Principal.Person = person)(
      projects: Projects,
      store: Store,
      name: String,
      args: (String, ujson.Value)*
  ): CommandOutcome = askBy(CommandBy.Direct(who, believed))(projects, store, name, args*)

  /** How far the current document of `key` at public is believed, as [[Integrity.written]]
    * writes it: what a projects command kept it at.
    */
  def believedOf(store: Store, key: String): Option[String] =
    DocKey
      .of(key)
      .fold(
        why => throw new java.lang.AssertionError(why),
        k =>
          inTx(store.keeper.current(k, Label.Public)).fold(
            e => throw new java.lang.AssertionError(e.toString),
            _.map(d => Integrity.written(d.integrity))
          )
      )

  def done(answer: CommandOutcome): String = answer match {
    case CommandOutcome.Done(text) => text
    case CommandOutcome.Refused(why) => throw new java.lang.AssertionError(s"refused: $why")
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

  /** The version `project_show` gives `key` at, 0 when it gives none. */
  def versionOf(projects: Projects, store: Store, key: String): Long =
    shown(projects, store, key).linesIterator
      .collectFirst { case l if l.startsWith("version: ") => l.drop(9).toLongOption }
      .flatten
      .getOrElse(0L)

  /** Project `p` holding `n` open notes, `p/n01` to `p/n<n>`, each kept a second after the one
    * before, from a second after [[at]].
    */
  def notes(projects: Projects, store: Store, n: Int): Unit = {
    started(projects, store)
    (1 to n).foreach(i =>
      done(
        askAt(at.plusSeconds(i))(
          projects,
          store,
          "project_keep",
          "key" -> f"p/n$i%02d",
          "kind" -> "note",
          "title" -> s"Title $i",
          "body" -> "Body."
        )
      )
    )
  }

  /** Why `name`'s arguments refuse `args`, or `None` when they read. */
  def refusedArgs(projects: Projects, name: String, args: (String, ujson.Value)*): Option[String] =
    projects.commands
      .find(c => ToolName.value(c.spec.name) == name)
      .flatMap(_.spec.args.read(ujson.Obj.from(args)).left.toOption.map(_.message))
}
