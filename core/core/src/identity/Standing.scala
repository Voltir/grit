package grit.core.identity

/** What a realm's source says now of one of its accounts. */
enum Standing {

  /** A full member, with the email the source verified for them, if any. */
  case Full(email: Option[Email])

  /** Not a full member (a guest, a member of another organisation, a bot or an app, invited,
    * deactivated), or the source cannot tell.
    */
  case Outside
}

/** What `account`'s realm's source says of it now. */
final case class Vouched(account: Account, standing: Standing)
