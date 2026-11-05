package grit.databases.connect

import grit.core.secret.Variable

import utest.*

/** Which JDBC URLs [[PostgresUrl.check]] takes, and what it says of the rest. */
object PostgresUrlTests extends TestSuite {

  private val Url = Variable("APP_DB_URL")

  private def check(url: String): Either[OpenRefusal, Unit] = PostgresUrl.check(Url, url)

  val tests = Tests {
    test("a host's database, a cloud connector's host-less one, and two hosts are taken") {
      assert(
        check("jdbc:postgresql://db.internal:5432/leads?sslmode=require") == Right(()),
        check(
          "jdbc:postgresql:///leads?cloudSqlInstance=p:r:i&socketFactory=com.example.Factory"
        ) == Right(()),
        check("jdbc:postgresql://a:5432,b:5433/leads") == Right(())
      )
    }

    test("another engine's URL, one without jdbc:, and one naming no database are not Postgres") {
      val not = Left(OpenRefusal.NotPostgres(Url))
      assert(
        check("jdbc:mysql://h/leads") == not,
        check("postgresql://h/leads") == not,
        check("jdbc:postgresql://h/") == not,
        check("jdbc:postgresql:leads") == not
      )
    }

    test("a user and password before the host, or as parameters in any case, are a credential") {
      val credentialed = Left(OpenRefusal.Credentialed(Url))
      assert(
        check("jdbc:postgresql://u:p@h/leads") == credentialed,
        check("jdbc:postgresql://h/leads?password=p") == credentialed,
        check("jdbc:postgresql://h/leads?sslmode=require&User=u") == credentialed,
        check("jdbc:postgresql://h/leads?sslpassword=p") == credentialed
      )
    }

    test("a parameter grit sets or guards is refused, naming it as written") {
      assert(
        check("jdbc:postgresql://h/leads?logServerErrorDetail=true") ==
          Left(OpenRefusal.Overrides(Url, "logServerErrorDetail")),
        check("jdbc:postgresql://h/leads?preferQueryMode=simple") ==
          Left(OpenRefusal.Overrides(Url, "preferQueryMode")),
        check("jdbc:postgresql://h/leads?service=app") == Left(
          OpenRefusal.Overrides(Url, "service")
        ),
        check("jdbc:postgresql://h/leads?maxResultBuffer=1G") ==
          Left(OpenRefusal.Overrides(Url, "maxResultBuffer"))
      )
    }
  }
}
