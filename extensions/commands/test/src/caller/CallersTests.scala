package grit.commands.caller

import grit.commands.worker.WorkerName
import grit.core.edge.Asked
import grit.core.identity.{Realm, Standing, TestAccounts}
import grit.core.visibility.Integrity
import grit.edge.http.{Request, Response, Status, Verb}

import utest.*

/** [[Callers.owner]]: who a request is from while no caller has a credential. */
object CallersTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  private val realm = got(Realm.of("desk", "home"))
  private val owner =
    Caller(TestAccounts.account("desk:home/owner"), Integrity.Member, got(WorkerName.of("desktop")))
  private val callers = Callers.owner(owner)

  private def request(headers: (String, String)*): Request =
    got(Request.of(Verb.Post, "/command/x", headers.toVector, ""))

  val tests = Tests {
    test("a request with no credential is the owner's; one carrying a credential is unrecognised") {
      (
        callers.from(request("Host" -> "localhost")),
        callers.from(request("Authorization" -> "Bearer t0ken"))
      ) ==> (Right(owner), Left(Callers.Unknown.Unrecognised))
    }

    test("the source says the owner is a full member of the realm, and no one else is") {
      val source = callers.source(realm)
      val other = TestAccounts.account("desk:home/other")
      (
        source.ask(owner.account),
        source.ask(other),
        source.all(realm),
        source.all(got(Realm.of("desk", "elsewhere")))
      ) ==> (
        Asked.Said(Standing.Full(None)),
        Asked.Said(Standing.Outside),
        Right(Map(owner.account -> Standing.Full(None))),
        Right(Map.empty)
      )
    }

    test("a request with no caller is refused 403, saying why") {
      Callers.refused(Callers.Unknown.Unrecognised) ==>
        Response.text(Status.Forbidden, "refused: the request's credential names no caller")
    }
  }
}
