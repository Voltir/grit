package grit.core.prompt

import utest.*

/** [[Voice]]: what `/set voice` reads, and the person fragment each voice gives. */
object VoiceTests extends TestSuite {

  private def own(text: String): Voice =
    Voice.of(text).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("a named voice is read by its key, ignoring case and surrounding space") {
      Voice.of("colleague") ==> Right(Voice.Named.Colleague)
      Voice.of("  Sassy ") ==> Right(Voice.Named.Sassy)
      Voice.of("PLAIN") ==> Right(Voice.Named.Plain)
    }

    test("any other text is the person's own words, trimmed") {
      Voice.of("  be brief, and swear a little  ").map(Voice.fragment) ==>
        Right(
          Some(
            Fragment(
              Layer.Person,
              Fragment.Person,
              "How to talk to this person:\nbe brief, and swear a little"
            )
          )
        )
    }

    test("blank words, or more than MaxChars, are refused") {
      Voice.of("   ") ==> Left("a voice is a name or some words, not nothing")
      Voice.of("x" * Voice.MaxChars).map(_ => ()) ==> Right(())
      Voice.of("x" * (Voice.MaxChars + 1)) ==>
        Left(s"a voice is at most ${Voice.MaxChars} characters, not ${Voice.MaxChars + 1}")
    }

    test("plain adds no fragment; every other named voice adds its words, from the person") {
      Voice.fragment(Voice.Named.Plain) ==> None
      Voice.Named.values.toVector.filterNot(_ == Voice.Named.Plain).map { v =>
        Voice
          .fragment(v)
          .map(f => (f.layer, f.source, f.text.startsWith("How to talk to this person:\n")))
      } ==> Vector.fill(Voice.Named.values.size - 1)(Some((Layer.Person, Fragment.Person, true)))
    }

    test("the default is sassy, and says where its sass stops") {
      Voice.Default ==> Voice.Named.Sassy
      Voice.fragment(Voice.Default).map(_.text) ==> Some(
        "How to talk to this person:\n" +
          "Talk with some sass: dry wit, a raised eyebrow at a shaky plan, short replies that " +
          "get to the point. Tease the idea, never the person. The sass stops at the facts: a " +
          "failure, a declined call, a refusal or something you do not know is said plainly and " +
          "first, never played for a laugh or softened. The sass lives in your replies, never " +
          "in files, commands or anything written for the record."
      )
      own("sassy") ==> Voice.Named.Sassy
    }

    test("a stored key is read exactly; one this build does not know is none") {
      Voice.named("colleague") ==> Some(Voice.Named.Colleague)
      Voice.named("Colleague") ==> None
      Voice.named("pirate") ==> None
    }
  }
}
