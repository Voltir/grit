package grit.databases.connect

import utest.*

/** [[DatabaseReader.open]] against a live Postgres: the role it lands as, and a server it cannot
  * reach.
  */
object OpenLiveTests extends TestSuite {

  private lazy val db = AppDatabase("open_live")

  val tests = Tests {
    test("a login as the declared role opens") {
      val reader = db.reader(db.login("opener", Seq("SELECT ON app.leads")))
      reader.close()
    }

    test("a login that lands as other than the declared role is refused, saying who it was") {
      val user = db.login("lander", Seq("SELECT ON app.leads"))
      val declared = db.role("declared")
      val opened = DatabaseReader.open(db.database(declared), db.env(user))
      opened.foreach(_.close())
      assert(opened.left.toOption == Some(OpenRefusal.WrongRole(Seen(user, user))))
    }

    test("a server nothing listens for is refused unreachable, by the driver's connection state") {
      val user = db.role("nobody")
      val opened =
        DatabaseReader.open(db.database(user), db.env(user, "jdbc:postgresql://127.0.0.1:1/none"))
      opened.foreach(_.close())
      assert(opened.left.toOption.collect { case OpenRefusal.Unreachable(f) =>
        f.state
      } == Some(Some("08001")))
    }
  }
}
