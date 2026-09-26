package grit.dbos.engine

import grit.core.store.Origin
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** `schema.sql`, which every [[Engine.open]] applies, against a real Postgres. */
object SchemaTests extends TestSuite {

  val tests = Tests {
    test("opening an engine again applies the schema again, keeping the rows already there") {
      val config = TestPostgres.freshDatabase("schema_twice")
      Engine.open(config, "test").close()
      val origin = Origin.Task("schema", "twice")
      val first = LiveDb.conversation(config, origin)
      Engine.open(config, "test").close()
      LiveDb.conversation(config, origin).id ==> first.id
    }
  }
}
