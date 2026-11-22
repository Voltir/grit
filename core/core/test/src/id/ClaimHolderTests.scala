package grit.core.id

import grit.core.either.Got.got

import utest.*

/** [[ClaimHolder]]'s two forms: [[ClaimHolder.form]] reads back what [[ClaimHolder.session]] and
  * [[ClaimHolder.agent]] make, and an [[AgentType]] is a type an agent's holder keeps whole.
  */
object ClaimHolderTests extends TestSuite {

  private val Desk = got(ClaimHolder.session("local", "grit-0a1b2c3d"))

  val tests = Tests {
    test("form reads back a session's holder, and an agent's with its type, or none when hashed") {
      (
        ClaimHolder.form(Desk),
        ClaimHolder.form(got(ClaimHolder.agent(Desk, "coder-opus", "0123456789ab"))),
        ClaimHolder.form(got(ClaimHolder.agent(Desk, "explore-5e6f7a8b", "0123456789ab")))
      ) ==> ((
        HolderForm.Session,
        HolderForm.Agent(Some(got(AgentType.of("coder-opus")))),
        HolderForm.Agent(None)
      ))
    }
    test("a holder of neither form reads as other") {
      Vector("nick", "a/b/c", "a/b/c/d/e", "a//b").map(t =>
        ClaimHolder.form(got(ClaimHolder.of(t)))
      ) ==>
        Vector.fill(4)(HolderForm.Other)
    }
    test("the builders refuse an empty part, one holding '/', and an agent started by no session") {
      (
        ClaimHolder.session("", "grit"),
        ClaimHolder.session("local", "a/b"),
        ClaimHolder.agent(Desk, "", "t"),
        ClaimHolder.agent(Desk, "coder", "a/b"),
        ClaimHolder.agent(got(ClaimHolder.of("nick")), "coder", "t")
      ) ==> ((
        Left("a holder's machine is empty or holds '/': "),
        Left("a holder's place is empty or holds '/': a/b"),
        Left("a holder's agent's type is empty or holds '/': "),
        Left("a holder's tag is empty or holds '/': a/b"),
        Left("an agent's holder is started by a session's, not nick")
      ))
    }
    test("an agent type is a slug kept whole that is not eight hex digits, alone or after a '-'") {
      Vector("coder-opus", "9lives", "coder-", "deadbeef", "x-deadbeef", "Explore", "a" * 41)
        .map(AgentType.of(_).isRight) ==> Vector(true, true, false, false, false, false, false)
    }
  }
}
