package grit.eval.harness.corpus

import grit.core.persona.Persona

import utest.*

/** A deployment's persona read from the file written beside a corpus. */
object PersonaJsonTests extends TestSuite {

  val tests = Tests {
    test("read is the persona the file names") {
      PersonaJson.read("""{"name": " Bort "}""") ==> Persona.of("Bort")
    }

    test("a name the persona refuses is refused in its words, and a file of another form says so") {
      PersonaJson.read("""{"name": "  "}""") ==> Left("persona: a persona's name is blank")
      PersonaJson.read("""not json""") ==> Left("persona: not JSON")
    }
  }
}
