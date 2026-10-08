package grit.slack.edge

import scala.concurrent.duration.{Duration, FiniteDuration}

import grit.core.identity.{Account, Realm, Vouched}
import grit.core.store.{LastWord, Linking, StoreError, Tx, Voucher}

/** A voucher of `realms` for the edge's tests: it keeps each answer it is given, in order, and
  * links no one (what grit's voucher makes of an answer is its own suites'). An account it was
  * given an answer for was last answered [[ago]] ago; one the test says grit has [[seen]], and
  * no answer was given for, never was.
  */
final class Kept(val realms: Set[Realm]) extends Voucher {

  // Each var holds an immutable value, written and read only on the test's own thread, through
  // the edge's calls it makes and waits on.
  @caps.unsafe.untrackedCaptures
  var vouched = Vector.empty[Vouched]

  @caps.unsafe.untrackedCaptures
  var seen = Set.empty[Account]

  @caps.unsafe.untrackedCaptures
  var ago: FiniteDuration = Duration.Zero

  def vouch(v: Vouched)(using Tx^): Either[StoreError, Vector[Linking]] =
    if (!realms.exists(_.holds(v.account))) Right(Vector(Linking.Outside(v.account)))
    else {
      vouched = vouched :+ v
      Right(Vector.empty)
    }

  def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
    Right(word(account))

  def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]] =
    Right((seen ++ vouched.map(_.account)).filter(realm.holds).toVector.map(word))

  private def word(account: Account): LastWord =
    LastWord(account, Option.when(vouched.exists(_.account == account))(ago))
}
