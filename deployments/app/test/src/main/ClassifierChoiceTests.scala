package grit.app.main

import grit.kit.deployment.Topics

import utest.*

/** Which classifier places messages among topics: Jev with a key, the stub only when asked,
  * otherwise none. The key's value is read, and a blank one refused, by `Secrets`.
  */
object ClassifierChoiceTests extends TestSuite {

  val tests = Tests {
    test("a key is Jev, even beside the stub's switch") {
      Main.topics(Map("JEV_API_KEY" -> "sk-jev-1", "GRIT_STUB_TOPICS" -> "1")) ==> Topics.Jev
    }

    test("no key: the stub only with GRIT_STUB_TOPICS=1, otherwise none, saying why") {
      Main.topics(Map("GRIT_STUB_TOPICS" -> "1")) ==> Topics.Stub
      Main.topics(Map("GRIT_STUB_TOPICS" -> "yes")) ==> Topics.Off("JEV_API_KEY is not set")
      Main.topics(Map.empty) ==> Topics.Off("JEV_API_KEY is not set")
    }
  }
}
