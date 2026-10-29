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

  /** A session of its own in `config` run by `by`, kept at `label`, working as `holder`, its one
    * event at `at`.
    */
  def work(
      config: DbConfig,
      by: Principal.Person,
      holder: ClaimHolder,
      at: Instant,
      label: Label = Label.Public
  ): Unit = {
    val recorded = for {
      harness <- Harness.of("claude-code")
      id <- SessionId.of(harness, s"works-${ClaimHolder.value(holder)}")
      shape <- Event.Shape.of(ujson.Obj())
      event <- Event.of(at.toEpochMilli, at, Event.Kind.Edited, shape)
      place = Place.under(Namespace.Fs, Vector("/work/grit"))
      header = Session.Header(id, None, by, label, place, Some(holder))
      done <- LiveDb
        .transaction(config, Clearance.of(label))(new SqlSessions().record(header, Vector(event)))
        .left
        .map(_.toString)
    } yield done
    recorded.fold(e => throw new java.lang.AssertionError(e), _ => ())
  }
}
