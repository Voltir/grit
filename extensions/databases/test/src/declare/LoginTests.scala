package grit.databases.declare

import grit.core.secret.Variable

import utest.*

/** Which driver logins [[Login.Driver.of]] takes. */
object LoginTests extends TestSuite {

  val tests = Tests {
    test("a driver login with a password among its plain properties is refused, in any case") {
      assert(
        Login.Driver.of(Map("PassWord" -> "x"), Map.empty) ==
          Left(
            "the driver's plain properties hold PassWord, a secret: give it as a secret's variable"
          ),
        Login.Driver.of(Map("sslpassword" -> "x"), Map.empty).isLeft
      )
    }

    test("a driver login with a socket factory and a secret's variable is a login") {
      val login =
        Login.Driver.of(Map("socketFactory" -> "f"), Map("sslpassword" -> Variable("DB_SSL_KEY")))
      assert(login.map(_.plain) == Right(Map("socketFactory" -> "f")))
    }
  }
}
