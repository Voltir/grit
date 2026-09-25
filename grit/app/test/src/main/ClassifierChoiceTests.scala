package grit.app.main

import utest.*

/** Which classifier places messages among topics: Jev with a key, the stub only when asked,
  * otherwise none.
  */
object ClassifierChoiceTests extends TestSuite {
  import Main.ClassifierChoice

  val tests = Tests {
    test("a key is Jev, even beside the stub's switch; an empty key is an error that hides it") {
      Main.classifierChoice(Map("JEV_API_KEY" -> "k", "GRIT_STUB_TOPICS" -> "1")) match {
        case Right(ClassifierChoice.Jev(config)) => assert(!config.toString.contains("k,"))
        case other => assert(other.toString == "Jev")
      }
      Main.classifierChoice(Map("JEV_API_KEY" -> " ")) ==> Left("JEV_API_KEY is empty")
    }

    test("no key: the stub only with GRIT_STUB_TOPICS=1, otherwise none, saying why") {
      Main.classifierChoice(Map("GRIT_STUB_TOPICS" -> "1")) ==> Right(ClassifierChoice.Stub)
      Main.classifierChoice(Map("GRIT_STUB_TOPICS" -> "yes")) ==>
        Right(ClassifierChoice.Off("JEV_API_KEY is not set"))
      Main.classifierChoice(Map.empty) ==> Right(ClassifierChoice.Off("JEV_API_KEY is not set"))
    }
  }
}
