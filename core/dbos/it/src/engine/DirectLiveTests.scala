package grit.dbos.engine

import grit.core.id.{EntryId, SourceId, TurnRef}
import grit.core.identity.{Account, Standing, Vouched}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Level,
  RoomLabels,
  Subject,
  TestLabels,
  Visibility
}
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, TestPostgres}

import utest.*

/** A direct message to grit, against a real Postgres: a room of one person, labelled at their
  * clearance when its thread begins and read at most at their clearance now.
  */
object DirectLiveTests extends TestSuite {

  private def sourced(text: String): Account.Sourced =
    Account
      .read(text)
      .toOption
      .flatMap(Account.Sourced.of)
      .getOrElse(throw new java.lang.AssertionError(s"no source's account: $text"))

  private val dana = sourced("slack:T1/U-dana")
  private val ed = sourced("slack:T1/U-ed")

  private val ConfidentialTrial = Label.at(Level.Confidential, TestLabels.trial)

  /** [[TestLabels.trial]] declared, every room internal, and `trial` naming `named`, granted
    * confidential·trial; T1's full members, `members`, granted it too.
    */
  private def naming(named: Set[Account]): Visibility =
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      rooms <- RoomLabels
        .of(Vector.empty, grit.core.visibility.Labelled.Mapped(Label.at(Level.Internal)))
        .left
        .map(_.written)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(
            Group(TestLabels.group("trial"), named),
            Group(TestLabels.group("members"), Set.empty, Set(Vouchings.T1))
          ),
          Vector(
            Grant(TestLabels.group("trial"), ConfidentialTrial),
            Grant(TestLabels.group("members"), ConfidentialTrial)
          )
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** `trial` names dana. */
  private val Cleared = naming(Set(dana))

  /** `trial` names no one: dana fell. */
  private val Fallen = naming(Set.empty)

  private def fresh(suite: String): DbConfig = {
    val c = TestPostgres.freshDatabase(suite)
    LiveEngine.open(c, "test", visibility = Cleared).close()
    c
  }

  /** Runs `body` with an engine on `config` under `visibility`, closed after. */
  private def engine[A](config: DbConfig, visibility: Visibility)(body: Engine => A): A = {
    val e = LiveEngine.open(config, "test", visibility = visibility)
    try body(e)
    finally e.close()
  }

  /** `text`, dana's message `ts` in her direct message's thread `thread`, as its turn. */
  private def told(e: Engine, by: Account.Sourced, thread: String, ts: String): TurnRef =
    e.inbox
      .ingest(Origin.Direct(by, thread), SourceId(ts), Message.User(s"message $ts"), by)
      .fold(err => throw new java.lang.AssertionError(s"ingest: $err"), identity)

  /** The label `origin`'s conversation was created at. */
  private def created(config: DbConfig, origin: Origin): Label =
    LiveDb.conversation(config, origin).label

  /** What `subject` floors at, and the ids of `turn`'s conversation's entries it reads. */
  private def opened(e: Engine, subject: Subject, turn: TurnRef): (Label, Vector[String]) =
    e.jot
      .write(subject) { (tx: Tx^) ?=>
        new SqlEntryStore().list(turn.conversationId).map(es => (Tx.floor(tx), es))
      }
      .fold(
        err => throw new java.lang.AssertionError(s"opening: $err"),
        read => (read._1, read._2.map(x => EntryId.value(x.id)))
      )

  private def ids(turn: TurnRef, sources: String*): Vector[String] =
    sources.toVector.map(s =>
      EntryId.value(grit.core.inbox.InboundId.of(turn.conversationId, SourceId(s)))
    )

  val tests = Tests {
    test(
      "a direct message's conversation is created at its person's clearance, one named in no group's at public"
    ) {
      val config = fresh("direct_created")
      engine(config, Cleared) { e =>
        told(e, dana, "1.0", "1.0")
        told(e, ed, "2.0", "2.0")
      }
      (created(config, Origin.Direct(dana, "1.0")), created(config, Origin.Direct(ed, "2.0"))) ==>
        (ConfidentialTrial, Label.Public)
    }

    test(
      "after a fall by restart, a turn of the old thread opens at the meet and reads none of its thread"
    ) {
      val config = fresh("direct_fallen")
      val before = engine(config, Cleared) { e =>
        val turn = told(e, dana, "1.0", "1.0")
        (opened(e, Subject.Turn(turn), turn), turn)
      }
      val (was, turn) = before
      val after = engine(config, Fallen) { e =>
        (
          opened(e, Subject.Turn(turn), turn),
          opened(e, Subject.Conversation(turn.conversationId), turn)
        )
      }
      (was, after) ==> (
        (ConfidentialTrial, ids(turn, "1.0")),
        ((Label.Public, Vector()), (Label.Public, Vector()))
      )
    }

    test(
      "after a fall, a new message in the old thread is Sealed and records nothing, a redelivery of its own is its turn, and a new thread begins at the clearance now"
    ) {
      val config = fresh("direct_sealed")
      val turn = engine(config, Cleared)(e => told(e, dana, "1.0", "1.0"))
      val old: Origin.Direct = Origin.Direct(dana, "1.0")
      val (refused, again, next) = engine(config, Fallen) { e =>
        (
          e.inbox.ingest(old, SourceId("1.1"), Message.User("more"), dana),
          e.inbox.ingest(old, SourceId("1.0"), Message.User("message 1.0"), dana),
          told(e, dana, "2.0", "2.0")
        )
      }
      val recorded = engine(config, Cleared)(e => e.inbox.recorded(old, Set(SourceId("1.1"))))
      (
        refused,
        again,
        recorded,
        created(config, Origin.Direct(dana, "2.0")),
        next.conversationId != turn.conversationId
      ) ==> (
        Left(InboxError.Sealed(old)),
        Right(turn),
        Right(Set()),
        Label.Public,
        true
      )
    }

    test(
      "after a fall by an attestation ended, with no restart, the next open is at the meet and reads none of the thread"
    ) {
      val config = fresh("direct_lapsed")
      val viaRealm = naming(Set.empty)
      engine(config, viaRealm) { e =>
        val voucher = e.voucher(Set(Vouchings.T1), Vouchings.Claimed)
        LiveDb
          .transaction(config)(voucher.vouch(Vouched(dana, Standing.Full(None))))
          .isRight ==> true
        val turn = told(e, dana, "1.0", "1.0")
        val was = opened(e, Subject.Turn(turn), turn)
        LiveDb.transaction(config)(voucher.vouch(Vouched(dana, Standing.Outside))).isRight ==> true
        (was, opened(e, Subject.Turn(turn), turn)) ==> (
          (ConfidentialTrial, ids(turn, "1.0")),
          (Label.Public, Vector())
        )
      }
    }

    test(
      "after a raise, the old thread keeps its lower label and a new thread is created at the higher"
    ) {
      val config = fresh("direct_raised")
      val old = engine(config, Fallen)(e => told(e, dana, "1.0", "1.0"))
      val (reopened, fresh2) = engine(config, Cleared) { e =>
        (opened(e, Subject.Turn(old), old)._1, told(e, dana, "2.0", "2.0"))
      }
      (
        created(config, Origin.Direct(dana, "1.0")),
        reopened,
        created(config, Origin.Direct(dana, "2.0")),
        fresh2.conversationId != old.conversationId
      ) ==> (Label.Public, Label.Public, ConfidentialTrial, true)
    }
  }
}
