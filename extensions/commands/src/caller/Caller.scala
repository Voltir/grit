package grit.commands.caller

import grit.commands.worker.WorkerName
import grit.core.edge.{Asked, EdgeRefusal, RealmSource, Variable}
import grit.core.identity.{Account, Realm, Standing}
import grit.core.visibility.Integrity
import grit.edge.http.{Request, Response, Status}

/** Who a request is from: `account`, whose words are believed at `believed`, working on
  * `worker`.
  */
final case class Caller(account: Account, believed: Integrity, worker: WorkerName) extends caps.Pure

/** Who each request to an edge is from, by the deployment's configuration. */
final class Callers private (owner: Caller) extends caps.Pure {

  /** The request's caller, or `Unknown` when it names none these know. */
  def from(request: Request): Either[Callers.Unknown, Caller] =
    if (request.header("Authorization").isDefined) Left(Callers.Unknown.Unrecognised)
    else Right(owner)

  /** Whether a request with no credential has a caller: then the edge serving these must listen
    * on loopback, and admit loopback's names alone.
    */
  def uncredentialed: Boolean = true

  /** Every account a request may be from, each once. */
  def accounts: Vector[Account] = Vector(owner.account)

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

  /** The environment variables [[opened]] reads: none, while no caller has a credential. */
  def needs: Vector[Variable] = Vector.empty

  /** These callers, ready to be asked, with what `env` holds of [[needs]]; never refused while
    * none is read.
    */
  def opened(env: Map[String, String]): Either[EdgeRefusal, Callers] = Right(this)
}

object Callers {

  /** `owner` for every request with no `Authorization` header; one with it is
    * [[Unknown.Unrecognised]].
    */
  def owner(owner: Caller): Callers = new Callers(owner)

  /** Why a request has no caller. */
  enum Unknown {

    /** It carries a credential no caller has. */
    case Unrecognised
  }

  /** What a request with no caller is answered: 403, saying why. */
  def refused(why: Unknown): Response = why match {
    case Unknown.Unrecognised =>
      Response.text(Status.Forbidden, "refused: the request's credential names no caller")
  }
}
