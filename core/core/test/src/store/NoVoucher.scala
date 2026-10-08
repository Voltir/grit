package grit.core.store

import grit.core.identity.{Account, Realm, Vouched}

/** A voucher of no realms, as the kit builds for an edge that attests nothing: it records
  * nothing, and no account has a last word.
  */
object NoVoucher extends Voucher {
  def realms: Set[Realm] = Set.empty
  def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]] =
    Right(Vector(Linking.Outside(vouched.account)))
  def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
    Right(LastWord(account, None))
  def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]] =
    Right(Vector.empty)
}
