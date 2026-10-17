package grit.outline.locate

import utest.*

object WordGlobTests extends TestSuite {

  def tests = Tests {
    test("a glob is found in a word whose literal pieces lie apart in it, and in no longer word") {
      val glob = WordGlob("Turn*Env")
      assert(
        glob.foundIn("val e: TurnToolsEnv = ???"),
        glob.foundIn("TurnEnv"),
        !glob.foundIn("MyTurnToolsEnv"),
        !glob.foundIn("TurnToolsEnvs"),
        !glob.foundIn("Turn Env")
      )
    }

    test("a glob is found only as a whole word, `$` counting as a word character") {
      val glob = WordGlob("*Shelf*")
      assert(
        glob.foundIn("trait DocumentShelf {"),
        glob.foundIn("Shelf"),
        !glob.foundIn("shelf"),
        WordGlob("placed").foundIn("x.placed(y)"),
        !WordGlob("placed").foundIn("x.placed$1"),
        !WordGlob("placed").foundIn("misplaced")
      )
    }

    test("matches takes the whole word, not a word inside it") {
      assert(
        WordGlob("Turn*").matches("TurnEnv"),
        !WordGlob("Turn*").matches("MyTurnEnv"),
        !WordGlob("Turn*").matches("TurnEnv x")
      )
    }
  }
}
