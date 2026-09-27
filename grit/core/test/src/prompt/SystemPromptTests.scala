package grit.core.prompt

import utest.*

/** [[SystemPrompt]]: the order a provider's cache depends on, and the ids a turn records. */
object SystemPromptTests extends TestSuite {

  private val base = Fragment(Layer.Base, Fragment.Grit, "You are grit.")
  private val edge = Fragment(Layer.Edge, Fragment.Grit, "One person, in a terminal.")
  private val reach = Fragment(Layer.Reach, Fragment.Grit, "Your tools act on /x.")
  private val near =
    Fragment(Layer.Place, "/x/AGENTS.md", "Instructions from /x/AGENTS.md:\n\nBe brief.")
  private val far = Fragment(Layer.Place, "/AGENTS.md", "Instructions from /AGENTS.md:\n\nBe kind.")

  val tests = Tests {
    test(
      "fragments given out of order render base, edge, reach, place, a layer's in its given order"
    ) {
      SystemPrompt.of(Vector(far, reach, near, base, edge)).render ==>
        "You are grit.\n\nOne person, in a terminal.\n\nYour tools act on /x.\n\n" +
        "Instructions from /AGENTS.md:\n\nBe kind.\n\nInstructions from /x/AGENTS.md:\n\nBe brief."
    }

    test("a fragment's id is the hash of its stored form") {
      // Pinned: fragment ids are recorded by every turn and name kept rows. Computed outside
      // grit: printf '%s' '{"layer":"base","source":"grit","text":"You are grit."}' |
      // sha256sum | cut -c1-16
      FragmentId.value(base.id) ==> "30617ef2dd8c700c"
    }

    test("fragments differing only in source or layer have different ids") {
      val ids =
        Set(base.id, base.copy(source = "/x/AGENTS.md").id, base.copy(layer = Layer.Edge).id)
      ids.size ==> 3
    }
  }
}
