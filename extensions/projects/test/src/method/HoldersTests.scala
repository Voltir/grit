package grit.projects.method

import grit.core.either.Got.got
import grit.core.id.{AgentType, ClaimHolder}

import utest.*

/** [[Holders.admits]]: which holders a role may be claimed for, by each holder's form. */
object HoldersTests extends TestSuite {

  private val Coders = Holders.Agents
    .of(Set("coder-opus", "coder-haiku").map(t => got(AgentType.of(t))))
    .getOrElse(throw new java.lang.AssertionError("two types are some"))

  /** A session's holder on `machine`, then the agents' it starts of a type kept whole, of
    * another kept whole, and of one kept by its hash; then a holder of neither form.
    */
  private def holders(machine: String): Vector[ClaimHolder] = {
    val session = got(ClaimHolder.session(machine, "grit-0a1b2c3d"))
    Vector(session) ++
      Vector("coder-opus", "wisdom", "explore-5e6f7a8b").map(t =>
        got(ClaimHolder.agent(session, t, "0123456789ab"))
      ) :+ got(ClaimHolder.of("nick"))
  }

  val tests = Tests {
    test(
      "anyone admits every holder, a session only a session's, agents only their types' agents"
    ) {
      Vector("local", "laptop").map(m =>
        Vector(Holders.Anyone, Holders.Session, Coders).map(h => holders(m).map(h.admits))
      ) ==> Vector.fill(2)(
        Vector(
          Vector(true, true, true, true, true),
          Vector(true, false, false, false, false),
          Vector(false, true, false, false, false)
        )
      )
    }
  }
}
