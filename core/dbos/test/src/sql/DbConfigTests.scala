package grit.dbos.sql

import utest.*

object DbConfigTests extends TestSuite {

  import DbConfig.{Invalid, PasswordVar, UrlVar, UserVar}

  private val url = "jdbc:postgresql://db.example:5432/prod"

  val tests = Tests {
    test("an unset or blank URL is refused, naming the variable and the URL's form") {
      Vector(Map.empty[String, String], Map(UrlVar -> " "), Map(UserVar -> "app"))
        .map(env => DbConfig.fromEnv(env).left.map(_.message)) ==> Vector.fill(3)(
        Left(
          "GRIT_DATABASE_URL is unset or blank: set it to grit's database, as jdbc:postgresql://host:port/db"
        )
      )
    }

    test("the URL is used as given; user and password default to docker-compose's login") {
      (
        DbConfig.fromEnv(Map(UrlVar -> url)),
        DbConfig.fromEnv(Map(UrlVar -> url, UserVar -> "app", PasswordVar -> "s3")),
        DbConfig.fromEnv(Map(UrlVar -> url, PasswordVar -> ""))
      ) ==> (
        Right(DbConfig(url, "grit", "grit")),
        Right(DbConfig(url, "app", "s3")),
        Right(DbConfig(url, "grit", ""))
      )
    }

    test("a blank user is rejected") {
      assert(DbConfig.fromEnv(Map(UrlVar -> url, UserVar -> "")) == Left(Invalid.Empty(UserVar)))
    }

    test("a URL that is not jdbc:postgresql is rejected") {
      val bad = Seq("postgres://u:p@h/db", "jdbc:mysql://h/db")
      bad.foreach { url =>
        assert(DbConfig.fromEnv(Map(UrlVar -> url)) == Left(Invalid.NotPostgresJdbc(UrlVar)))
      }
    }

    test("a place is the URL's hosts, each with its port, and database, never its parameters") {
      Vector(
        "jdbc:postgresql://db.example:6543/prod?user=u&password=hunter2",
        "jdbc:postgresql://a,b:5433,[::1]/prod",
        "jdbc:postgresql:prod",
        "jdbc:postgresql://db.example/"
      ).map(u => DbConfig(u, "u", "p").place.toString) ==> Vector(
        "db.example:6543/prod",
        "a:5432,b:5433,[::1]:5432/prod",
        "localhost:5432/prod",
        "db.example:5432/"
      )
    }

    test("neither toString nor an error message shows a secret") {
      val shown = DbConfig("jdbc:postgresql://h/db", "u", "hunter2").toString
      assert(!shown.contains("hunter2"))

      val url = "postgres://u:hunter2@h/db"
      val message = DbConfig.fromEnv(Map(UrlVar -> url)).left.map(_.message)
      assert(message.left.exists(m => m.contains(UrlVar) && !m.contains("hunter2")))
    }
  }
}
