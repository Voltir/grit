package grit.core.persona

import grit.core.prompt.{Fragment, Layer, Voice}

import utest.*

object PersonaTests extends TestSuite {

  val tests = Tests {
    test("a persona's name is kept trimmed") {
      Persona.of("  Pip \t").map(_.name) ==> Right("Pip")
    }

    test("a blank, multi-line, control-character or over-long name is refused, saying which") {
      Vector("", "   ", "Bo\nrt", "Bo\u0007rt", "b" * (Persona.MaxChars + 1))
        .map(Persona.of) ==> Vector(
        Left("a persona's name is blank"),
        Left("a persona's name is blank"),
        Left("a persona's name is one line, with no control character"),
        Left("a persona's name is one line, with no control character"),
        Left(s"a persona's name is at most ${Persona.MaxChars} characters")
      )
    }

    test("a name of exactly the longest length is kept") {
      Persona.of("b" * Persona.MaxChars).map(_.name) ==> Right("b" * Persona.MaxChars)
    }

    test("grit itself is named grit") {
      Persona.Grit.name ==> "grit"
    }

    test(
      "a persona's voice is told to the model in its words, from the persona; plain tells nothing"
    ) {
      val wizard =
        Voice
          .of("Talk like a kindly wizard.")
          .fold(e => throw new java.lang.AssertionError(e), identity)
      // Pinned: the words every voiced persona's readers are shaped by, byte for byte.
      Persona.Grit.speaking(wizard).voiced.map(f => (f.layer, f.source, f.text)) ==> Some(
        (
          Layer.Edge,
          Fragment.Persona,
          "How you sound here, as the people running you chose it:\n" +
            "Talk like a kindly wizard."
        )
      )
      (Persona.Grit.voiced, Persona.Grit.speaking(wizard).speaking(Voice.Named.Plain).voiced) ==>
        (None, None)
    }
  }
}
