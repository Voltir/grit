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
import grit.projects.artifact.{Artifact, ArtifactHead, ArtifactJson, Rank}
import grit.projects.method.{
  ArtifactKind,
  Example,
  Method,
  MethodFixtures,
  MethodJson,
  RoleView,
  TransitionChecks
}
import grit.projects.name.{Handle, MethodName}

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

  /** What `project_declare` answers a caller who may not declare: not believed at Authority, or
    * not giving the command directly.
    */
  val declareRefused: String =
    "A method is declared only by a caller believed at Authority giving the command directly, " +
      "never through a turn or by a session believed lower. Ask a person to declare it from the " +
      "method's file."

  /** What moving `p/a` to done answers when only people make that move and its mover is not one. */
  val personOnlyMove: String =
    "p/1 a cannot move to done here: that move is made only by a caller believed at Authority " +
      "giving the command directly, never through a turn or by a session believed lower; ask " +
      "the person to make it"

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
          Set(MethodFixtures.right(Handle.of("process")))
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

  /** Project `p`, started, put back in the old form (ADR 0063): its index holding no `next`, and
    * each of `artifacts` kept at public under its key, believed as given, with `data` the old
    * form's ([[grit.projects.artifact.OldFormFixtures.data]]).
    */
  def oldForm(
      store: Store,
      artifacts: (key: String, data: ujson.Value, believed: Integrity)*
  ): Unit = {
    def key(k: String): DocKey =
      DocKey.of(k).fold(why => throw new java.lang.AssertionError(why), identity)
    def ref(kind: String): grit.core.document.DocRef =
      grit.core.document.DocRef
        .of(kind, "p")
        .fold(why => throw new java.lang.AssertionError(why), identity)
    inTx {
      for {
        index <- store.keeper.current(key("p"), Label.Public)
        d <- index.toRight(StoreError.Invalid("no index"))
        _ <- store.keeper.write(
          d.key,
          d.label,
          d.integrity,
          d.place,
          d.text,
          ujson.Obj.from(d.data.obj.toSeq.filterNot(_._1 == "next")),
          None,
          at,
          Set(ref("pinned"))
        )
      } yield ()
    }.fold(e => throw new java.lang.AssertionError(e.toString), identity)
    artifacts.foreach { a =>
      inTx(
        store.keeper.write(
          key(a.key),
          Label.Public,
          a.believed,
          Place.Everywhere,
          grit.core.document.DocText
            .of("Old.")
            .fold(why => throw new java.lang.AssertionError(why), identity),
          a.data,
          None,
          at,
          Set(ref("project"))
        )
      ).fold(e => throw new java.lang.AssertionError(e.toString), _ => ())
    }
  }

  /** Project `p`, started, as a re-key leaves [[oldForm]]'s `artifacts`: numbered in key order,
    * each kept still under its old key.
    */
  def rekeyed(
      projects: Projects,
      store: Store,
      artifacts: (key: String, data: ujson.Value, believed: Integrity)*
  ): Unit = {
    oldForm(store, artifacts*)
    done(ask(projects, store, "project_rekey", "project" -> "p"))
  }

  /** The artifact kept under `key` at public given `rank` in place, as a re-key of the old form
    * leaves siblings tied at [[Rank.Middle]].
    */
  def rankedAt(store: Store, key: String, rank: String): Unit =
    rewritten(
      store,
      key,
      _.copy(rank = Rank.of(rank).fold(why => throw new java.lang.AssertionError(why), identity))
    )

  /** The artifact kept under `key` at public given the head `change` makes of it, kept at
    * `label`, as no command would write it.
    */
  def rewritten(
      store: Store,
      key: String,
      change: ArtifactHead => ArtifactHead,
      label: Label = Label.Public
  ): Unit = {
    val docKey = DocKey.of(key).fold(why => throw new java.lang.AssertionError(why), identity)
    {
      given Tx = TestTx.fake(Clearance.of(label))
      for {
        was <- store.keeper.current(docKey, Label.Public)
        d <- was.toRight(StoreError.Invalid(s"nothing kept under $key"))
        a <- Artifact.of(d).left.map(StoreError.Invalid(_))
        written <- ArtifactJson
          .write(Artifact(change(a.head), a.body))
          .left
          .map(StoreError.Invalid(_))
        (text, data, refs) = written
        _ <- store.keeper.write(
          docKey,
          label,
          d.integrity,
          d.place,
          text,
          data,
          d.pointer,
          at,
          refs
        )
      } yield ()
    }.fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

  /** The artifact kept under `key` at public kept instead at `label`, as a re-key leaves an
    * artifact the old form kept above its project's label, and the public one withdrawn.
    */
  def keptAbove(store: Store, key: String, label: Label): Unit = {
    rewritten(store, key, identity, label)
    inTx(
      store.keeper.withdraw(
        DocKey.of(key).fold(why => throw new java.lang.AssertionError(why), identity),
        Label.Public,
        at
      )
    )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
  }

  def kept(
      projects: Projects,
      store: Store,
      local: String,
      kind: String,
      body: String
  ): Unit =
    done(keeping(projects, store, local, kind, body))

  /** The answer to a keep of `p/<local>` ([[naming]]), of `kind`, titled `Title <local>`, holding
    * `body`, with `more`.
    */
  def keeping(
      projects: Projects,
      store: Store,
      local: String,
      kind: String,
      body: String,
      more: (String, ujson.Value)*
  ): CommandOutcome =
    ask(
      projects,
      store,
      "project_keep",
      (naming(projects, store, local) ++ Vector[(String, ujson.Value)](
        "kind" -> kind,
        "title" -> s"Title $local",
        "body" -> body
      ) ++ more)*
    )

  /** How a keep names artifact `p/<local>`: by that key when the project has it, else as a new
    * artifact taking `local` as its handle.
    */
  def naming(projects: Projects, store: Store, local: String): Vector[(String, ujson.Value)] =
    ask(projects, store, "project_show", "key" -> s"p/$local", "head" -> true) match {
      case CommandOutcome.Done(_) => Vector("key" -> s"p/$local")
      case CommandOutcome.Refused(_) => Vector("project" -> "p", "handle" -> local)
    }

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
          "project" -> "p",
          "handle" -> f"n$i%02d",
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
