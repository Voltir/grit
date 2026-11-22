package grit.dbos.engine

import java.time.Instant

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.Principal
import grit.core.place.{Namespace, Place}
import grit.core.session.{Event, Session}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.{DbConfig, LiveDb, SqlSessions}

/** Sessions working as a claim holder, the lease of its claims, for the claims suites. */
object HolderSessions {

  /** A session in `config` run by `by`, kept at `label`, working as `holder`, its one event at
    * `at`, then its end at `at` too when `ended`: `session`, or one of the holder's own; a child of
    * `parent` when given.
    */
  def work(
      config: DbConfig,
      by: Principal.Person,
      holder: ClaimHolder,
      at: Instant,
      label: Label = Label.Public,
      session: Option[SessionId] = None,
      parent: Option[SessionId] = None,
      ended: Boolean = false
  ): Unit = {
    val recorded = for {
      harness <- Harness.of("claude-code")
      own <- SessionId.of(harness, s"works-${ClaimHolder.value(holder)}")
      id = session.getOrElse(own)
      shape <- Event.Shape.of(ujson.Obj())
      event <- Event.of(at.toEpochMilli, at, Event.Kind.Edited, shape)
      end <- Event.of(at.toEpochMilli + 1, at, Event.Kind.Ended, shape)
      place = Place.under(Namespace.Fs, Vector("/work/grit"))
      header = Session.Header(id, parent, by, label, place, Some(holder))
      events = if (ended) Vector(event, end) else Vector(event)
      done <- LiveDb
        .transaction(config, Clearance.of(label))(new SqlSessions().record(header, events))
        .left
        .map(_.toString)
    } yield done
    recorded.fold(e => throw new java.lang.AssertionError(e), _ => ())
  }
}
