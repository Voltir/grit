package grit.tools

import grit.core.durable.Probes

import utest.*

/** What `Versioning.offered`'s declared type refuses: its toolbox acts through the run's
  * [[grit.core.host.Branch]], so it does not fit a type that captures nothing, and no body
  * built over the branch can hand it out as a pure value.
  */
object VersioningCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.host.Branch
      |import grit.core.place.Place
      |import grit.core.tool.{SetRefusal, Toolbox, Writing}
      |import grit.tools.Versioning
      |""".stripMargin

  private val itself =
    "def made(b: Branch^, d: Writing[Versioning.OfferArgs, Place]): Either[SetRefusal, Toolbox[caps.CapSet^{b}]] = Versioning.offered(b, d)"
  private val capturingNothing =
    "def made(b: Branch^, d: Writing[Versioning.OfferArgs, Place]): Either[SetRefusal, Toolbox[caps.CapSet^{}]] = Versioning.offered(b, d)"
  private val handedOut =
    "def made(b: Branch^, d: Writing[Versioning.OfferArgs, Place]): () -> Branch^{b} = () => b"

  private lazy val compiled = Probes.batch(prelude, List(itself, capturingNothing, handedOut))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: the toolbox at its declared type, capturing the branch, compiles") {
      compiled.errors(itself) ==> Nil
    }

    test("the toolbox typed as capturing nothing is rejected") {
      assert(compiled.errors(capturingNothing).exists(_.contains("capability `b` cannot flow")))
    }

    test("a pure function handing out the branch is rejected") {
      assert(compiled.errors(handedOut).exists(_.contains("capability `b` cannot flow")))
    }
  }
}
