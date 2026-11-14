package grit.commands.caller

import grit.commands.worker.WorkerName
import grit.core.edge.{Asked, RealmSource}
import grit.core.id.Sha256
import grit.core.identity.{Account, Realm, Standing}
import grit.core.visibility.Integrity
import grit.edge.http.{Challenge, Request, Response, Status}

/** Who a request is from and which machine it is on, two facts its credential names apart:
  * `account`, whose words are believed at `believed`, on the machine `worker`.
  */
final case class Caller(account: Account, believed: Integrity, worker: WorkerName) extends caps.Pure

/** Who each request to an edge is from, by the deployment's configuration. */
final class Callers private (rule: Callers.Rule) extends caps.Pure {

  /** The request's caller, as [[Callers.of]] or [[Callers.owner]] reads it, or the challenge its
    * 401 carries.
    */
  def from(request: Request): Either[Challenge, Caller] = rule match {
    case Callers.Rule.Owner(owner) =>
      if (request.headerValues(Callers.Authorization).isEmpty) Right(owner)
      else Left(Challenge.Refused)
    case Callers.Rule.Tokens(tokens) =>
      Callers.bearer(request).toRight(Challenge.Asked).flatMap { text =>
        val sent = Sha256.text(text).bytes
        // Every token is compared, none skipped once one matches, and each over all 32 bytes
        // with no early exit, so how long an answer takes says nothing of which matched or how
        // closely.
        tokens
          .foldLeft(Option.empty[Caller]) { (found, token) =>
            val differ = token.digest.bytes.zip(sent).foldLeft(0) { case (d, (a, b)) =>
              d | (a ^ b)
            }
            if (differ == 0) Some(token.caller) else found
          }
          .toRight(Challenge.Refused)
      }
  }

  /** Whether a request with no credential has a caller ([[Callers.owner]]): then the edge
    * serving these admits loopback's names alone.
    */
  def uncredentialed: Boolean = rule match {
    case Callers.Rule.Owner(_) => true
    case Callers.Rule.Tokens(_) => false
  }

  /** Every account a request may be from, each once. */
  def accounts: Vector[Account] = rule match {
    case Callers.Rule.Owner(owner) => Vector(owner.account)
    case Callers.Rule.Tokens(tokens) => tokens.map(_.caller.account).distinct
  }

  /** What the realm's source says when the edge serving these attests `realm`: every caller's
    * account in `realm` a full member, with no email; every other account outside.
    */
  def source(realm: Realm): RealmSource = new RealmSource {
    private val members: Map[Account, Standing] =
      accounts.filter(realm.holds).map(_ -> Standing.Full(None)).toMap
    def ask(account: Account): Asked = Asked.Said(members.getOrElse(account, Standing.Outside))
    def all(of: Realm): Either[Asked.Unreached, Map[Account, Standing]] =
      Right(if (of == realm) members else Map.empty)
  }
}

object Callers {

  /** `owner` for every request with no `Authorization` header; one with it is
    * [[Challenge.Refused]].
    */
  def owner(owner: Caller): Callers = new Callers(Rule.Owner(owner))

  /** A request is from the caller of the token whose digest is that of its `Authorization:
    * Bearer` text, the scheme matched without regard to case; none, two, another scheme or an
    * empty text is [[Challenge.Asked]]; a text no token's digest is that of,
    * [[Challenge.Refused]]. Or why not: two tokens of one digest. Two tokens may name one account,
    * at two integrities or on two machines, and one machine two accounts.
    */
  def of(first: Token, rest: Token*): Either[String, Callers] = {
    val tokens = first +: rest.toVector
    val digests = tokens.map(_.digest)
    digests
      .diff(digests.distinct)
      .headOption
      .map(d => s"two credentials have the digest ${d.hex}: give each its own")
      .toLeft(new Callers(Rule.Tokens(tokens)))
  }

  /** A credential the edge knows by `digest`, the SHA-256 digest of its text's UTF-8 bytes
    * ([[Sha256.text]]): a request bearing that text is from `caller`. A digest is no secret; the
    * text is never configured.
    */
  final case class Token(digest: Sha256, caller: Caller) extends caps.Pure

  /** 401 carrying `why`, its body one line saying what to send. */
  def refused(why: Challenge): Response = why match {
    case Challenge.Asked =>
      Response.text(
        Status.Unauthorized(why),
        "refused: this edge needs a credential: send Authorization: Bearer <your credential>"
      )
    case Challenge.Refused =>
      Response.text(
        Status.Unauthorized(why),
        "refused: the credential sent is not one this edge knows"
      )
  }

  /** `Authorization`: the header a credential is sent in. */
  private val Authorization: String = "Authorization"

  /** The text of the one `Authorization` header `request` sends, when its scheme is `Bearer`
    * in any case and its text is not empty.
    */
  private def bearer(request: Request): Option[String] =
    request.headerValues(Authorization) match {
      case Vector(value) =>
        val (scheme, rest) = value.span(_ != ' ')
        Option.when(scheme.equalsIgnoreCase("Bearer"))(rest.dropWhile(_ == ' ')).filter(_.nonEmpty)
      case _ => None
    }

  /** Whom a request is from: the owner when it sends no credential, or the caller of the token
    * it bears.
    */
  private enum Rule {
    case Owner(owner: Caller)
    case Tokens(tokens: Vector[Token])
  }
}
