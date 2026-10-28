package grit.core.visibility

import scala.compiletime.testing.typeCheckErrors

import grit.core.id.PrincipalId
import grit.core.identity.Principal
import grit.core.place.Place

import utest.*

/** What a [[Subject]] may be opened for, by its types. */
object SubjectTests extends TestSuite {

  /** A person's command, which compiles; naming [[Subject.Person]] here also makes an
    * incremental compile recheck this file's quoted sources when its parameters change.
    */
  private val asked: Subject =
    Subject.Person(new Principal.Person(PrincipalId.Local, Set.empty), Place.Everywhere)

  val tests: Tests = Tests {

    test("Subject.Person(Principal.Grit, room) does not compile") {
      val errors = typeCheckErrors(
        "grit.core.visibility.Subject.Person(grit.core.identity.Principal.Grit, grit.core.place.Place.Everywhere)"
      )
      assert(asked != Subject.Public)
      assert(errors.exists(_.message.contains("Principal.Person")))
    }
  }
}
