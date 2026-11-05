package grit.databases.connect

import utest.*

/** Which texts [[OneStatement.of]] takes as one statement. */
object OneStatementTests extends TestSuite {

  private def of(text: String): Option[String] = OneStatement.of(text).map(OneStatement.text)

  val tests = Tests {
    test("a statement, with or without trailing semicolons, is one, trimmed of them") {
      assert(of(" SELECT 1 ") == Some("SELECT 1"), of("SELECT 1;  ;\n") == Some("SELECT 1"))
    }

    test("two statements are refused") {
      assert(of("SELECT 1; SELECT 2") == None)
    }

    // The bypass a READ ONLY transaction alone falls to: COMMIT ends it, and what follows runs
    // in a transaction of its own.
    test("a statement that commits and writes after it is refused") {
      assert(of("SELECT 1; COMMIT; INSERT INTO t VALUES (1)") == None)
    }

    // Over-approximate on purpose: grit does not lex SQL, so a ; anywhere but the end is refused.
    test("a semicolon inside a string literal is refused too") {
      assert(of("SELECT ';'") == None)
    }

    test("a blank text, or semicolons alone, is no statement") {
      assert(of("  ") == None, of(" ; ;") == None)
    }
  }
}
