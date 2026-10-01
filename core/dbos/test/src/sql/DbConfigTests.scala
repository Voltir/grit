package grit.dbos.sql

import utest.*

object DbConfigTests extends TestSuite {

  import DbConfig.{Invalid, Local, PasswordVar, UrlVar, UserVar}

  val tests = Tests {
    test("an empty environment is the local database") {
      assert(DbConfig.fromEnv(Map.empty) == Right(Local))
    }

    test("each variable overrides only its own field") {
      val url = "jdbc:postgresql://db.example:5432/prod"
      assert(DbConfig.fromEnv(Map(UrlVar -> url)) == Right(Local.copy(jdbcUrl = url)))
      assert(DbConfig.fromEnv(Map(UserVar -> "app")) == Right(Local.copy(user = "app")))
      assert(DbConfig.fromEnv(Map(PasswordVar -> "s3")) == Right(Local.copy(password = "s3")))
    }

    test("an empty password is accepted") {
      assert(DbConfig.fromEnv(Map(PasswordVar -> "")) == Right(Local.copy(password = "")))
    }

    test("a blank URL or user is rejected") {
      assert(DbConfig.fromEnv(Map(UrlVar -> " ")) == Left(Invalid.Empty(UrlVar)))
      assert(DbConfig.fromEnv(Map(UserVar -> "")) == Left(Invalid.Empty(UserVar)))
    }

    test("a URL that is not jdbc:postgresql is rejected") {
      val bad = Seq("postgres://u:p@h/db", "jdbc:mysql://h/db")
      bad.foreach { url =>
        assert(DbConfig.fromEnv(Map(UrlVar -> url)) == Left(Invalid.NotPostgresJdbc(UrlVar)))
      }
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
