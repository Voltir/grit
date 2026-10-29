package grit.app.main

import grit.core.id.{DocKey, PluginName}
import grit.core.identity.{Account, TestAccounts}
import grit.core.place.Place
import grit.core.plugin.Plugin
import grit.core.visibility.{Integrity, Subject}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** Labelled rooms and two cleared people, for suites running plugins' commands as an edge runs
  * them for a person in a room.
  */
private[main] object CommandRooms {

  private def place(text: String): Place =
    Place.read(text).fold(e => sys.error(e), identity)

  /** Labelled internal, and its speech capped at outside. */
  val team: Place = place("slack:T1/C-team")

  /** Labelled internal, capped nowhere. */
  val desk: Place = place("slack:T1/C-desk")

  /** Labelled confidential. */
  val board: Place = place("slack:T1/C-board")

  val ann: Account = TestAccounts.account("slack:T1/U-ann")
  val bea: Account = TestAccounts.account("slack:T1/U-bea")
  val nobody: Account = TestAccounts.account("slack:T1/U-nobody")

  /** [[ann]] cleared internal, [[bea]] confidential; the rooms above labelled and capped. */
  private val commanded: grit.core.visibility.Visibility = {
    import grit.core.visibility.*
    val internal = Label.at(Level.Internal)
    val confidential = Label.at(Level.Confidential)
    (for {
      labels <- RoomLabels
        .of(
          Vector(team -> internal, desk -> internal, board -> confidential),
          Labelled.Mapped(Label.Public)
        )
        .left
        .map(_.written)
      compartments <- Compartments.of(Vector()).left.map(_.toString)
      staff <- GroupName.of("staff").left.map(_.toString)
      bosses <- GroupName.of("bosses").left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          labels,
          Vector(Group(staff, Set(ann)), Group(bosses, Set(bea))),
          Vector(Grant(staff, internal), Grant(bosses, confidential)),
          caps = Vector(Cap(team, Integrity.Outside))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => sys.error(e), identity)
  }

  /** An engine on a fresh database named for `suite`, under [[commanded]] with [[ann]] and [[bea]]
    * seen, and the commands of the plugins `plugins` makes of it.
    */
  def commanding[A](suite: String, plugins: Engine^ => Vector[Plugin])(
      body: (DbConfig, Engine^, grit.core.edge.PluginCommands^) => A
  ): A = {
    val config = TestPostgres.freshDatabase(suite)
    val engine = LiveEngine.open(config, "test", visibility = commanded)
    try {
      engine.jot.write(Subject.Public)(
        engine.principals.name(ann, "Ann").flatMap(_ => engine.principals.name(bea, "Bea"))
      ) ==> Right(())
      body(config, engine, engine.commands(plugins(engine)))
    } finally engine.close()
  }

  /** Every current document of `plugin`, as (key, label, integrity), read at maintenance's
    * clearance.
    */
  def kept(
      config: DbConfig,
      engine: Engine^,
      plugin: PluginName
  ): Vector[(String, String, String)] =
    LiveDb
      .transaction(config)(engine.reads(plugin).documents.newest(100))
      .fold(
        e => sys.error(e.toString),
        _.map(d =>
          (
            DocKey.value(d.key),
            grit.core.visibility.Label.written(d.label),
            Integrity.written(d.integrity)
          )
        ).sorted
      )
}
