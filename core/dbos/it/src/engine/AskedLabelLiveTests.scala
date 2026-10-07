package grit.dbos.engine

import java.time.Instant

import grit.core.clock.SetClock
import grit.core.id.{PluginName, PrincipalId, TestCallSlots, TurnRef, TurnSeq}
import grit.core.job.JobTests.Count
import grit.core.job.{JobTests, ScheduleContract, When}
import grit.core.store.Origin
import grit.core.visibility.{
  Compartment,
  Compartments,
  Grant,
  Group,
  GroupName,
  Label,
  Level,
  RoomLabels,
  Visibility
}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlJot, SqlSchedules, SqlTombstones, TestPostgres}

import org.postgresql.ds.PGSimpleDataSource
import utest.*

/** An asked schedule is kept at what its asking turn reads beyond its room: the desk's
  * transaction is opened for that turn, and the label is read from it, never passed.
  */
object AskedLabelLiveTests extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("asked_label")
    LiveEngine.open(c, "test").close()
    c
  }

  private def ok[A](e: Either[String, A]): A =
    e.fold(e => throw new java.lang.AssertionError(e), identity)

  private val trial: Compartment = ok(Compartment.of("trial"))
  private val trialLabel: Label = Label.at(Level.Public, trial)
  private val cleared = PrincipalId("slack:T1/U-cleared")
  private val uncleared = PrincipalId("slack:T1/U-uncleared")

  private val visibility: Visibility = {
    val name = ok(GroupName.of("trialists"))
    ok(for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(Group(name, Set(cleared))),
          Vector(Grant(name, trialLabel))
        )
        .left
        .map(_.toString)
    } yield v)
  }

  private val schedules = new SqlSchedules(new SqlTombstones)

  private val standup = new JobTests.Counting("standup")

  /** The label of the schedule asked from a turn in a `{trial}` room, asked by `by`. */
  private def askedBy(by: PrincipalId, thread: String): Option[Label] = {
    val turn = TurnRef(
      LiveDb.conversation(config, Origin.Slack("T1", "C1", thread), trialLabel).id,
      TurnSeq.First
    )
    LiveDb.asking(config, turn, by, Some(s"C1/$thread"))
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    val desk = schedules.desk(
      ok(PluginName.of("remind")),
      Vector(standup.name),
      new SqlJot(ds, new Opener(visibility)),
      new SetClock(Instant.parse("2026-10-07T09:00:00Z"))
    )
    val id = desk
      .ask(
        TestCallSlots.at(turn),
        ScheduleContract.booking(standup),
        When.At(Instant.parse("2026-10-20T09:00:00Z")),
        ScheduleContract.hour,
        Count(1)
      )
      .fold(r => throw new java.lang.AssertionError(s"$r"), _.id)
    LiveDb
      .transaction(config)(schedules.read(id))
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.map(_.label))
  }

  val tests = Tests {
    test("one asked by a person cleared for the room's label is kept at it") {
      askedBy(cleared, "2.1") ==> Some(trialLabel)
    }

    test("one asked by an uncleared person is kept public") {
      askedBy(uncleared, "2.2") ==> Some(Label.Public)
    }
  }
}
