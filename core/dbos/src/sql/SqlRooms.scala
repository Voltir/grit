package grit.dbos.sql

import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{Label, Visibility}

/** The label a conversation is created at (ADR 0030, 0032): the one rule every creation of a
  * conversation in this module calls.
  */
private[dbos] object SqlRooms {

  /** The label `origin`'s conversation is created at in the transaction open, under the
    * deployment's `visibility`: a direct message's, its person's clearance now (the asker
    * [[Opener.asker]] resolves, its account kept as a new person's when not seen before); any
    * other's, its room's label ([[Visibility.roomLabel]]).
    */
  def label(visibility: Visibility, origin: Origin)(using Tx^): Either[StoreError, Label] =
    origin match {
      case Origin.Direct(account, _) =>
        for {
          _ <- SqlIdentities.enroll(Set(account))
          asker <- Opener.asker(origin, None)
        } yield asker.fold(Label.Public)(visibility.cleared)
      case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Task(_, _) =>
        Right(visibility.roomLabel(origin.room))
    }
}
