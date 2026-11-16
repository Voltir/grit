package grit.edge.http

import utest.*

object ChallengeTests extends TestSuite {

  val tests = Tests {
    test("a request with no caller is 401, carrying its challenge and saying what to send") {
      Vector(Challenge.Asked, Challenge.Refused).map(_.refused) ==> Vector(
        Response.text(
          Status.Unauthorized(Challenge.Asked),
          "refused: this edge needs a credential: send Authorization: Bearer <your credential>"
        ),
        Response.text(
          Status.Unauthorized(Challenge.Refused),
          "refused: the credential sent is not one this edge knows"
        )
      )
    }
  }
}
