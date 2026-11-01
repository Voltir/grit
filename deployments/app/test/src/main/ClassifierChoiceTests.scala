package grit.app.main

import grit.kit.deployment.Placer

import utest.*

/** Which classifier places messages on the agenda: Jev with a key, the stub only when asked,
  * otherwise none. The key's value is read, and a blank one refused, by `Secrets`.
  */
object ClassifierChoiceTests extends TestSuite {

  val tests = Tests {
    test("a key is Jev, even beside the stub's switch") {
      Deploying.placer(Map("JEV_API_KEY" -> "sk-jev-1", "GRIT_STUB_TOPICS" -> "1")) ==> Placer.Jev
    }

    test("no key: the stub only with GRIT_STUB_TOPICS=1, otherwise none, saying why") {
      Deploying.placer(Map("GRIT_STUB_TOPICS" -> "1")) ==> Placer.Stub
      Deploying.placer(Map("GRIT_STUB_TOPICS" -> "yes")) ==> Placer.Off("JEV_API_KEY is not set")
      Deploying.placer(Map.empty) ==> Placer.Off("JEV_API_KEY is not set")
    }
  }
}
