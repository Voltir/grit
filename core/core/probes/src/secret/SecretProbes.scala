package grit.core.secret

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about a [[Secret]] outside its package: reading its value, making
  * one from a string, journaling one. Only [[Reveal]] and [[Secret.read]] reach it.
  */
object SecretProbes extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.secret.*
      |import grit.core.durable.Journaled
      |""".stripMargin

  private val control =
    """def f(env: Map[String, String]): Either[Secret.Refusal, String] =
      |  Secret.read(env, Variable("ANTHROPIC_API_KEY")).map(Reveal.text)
      |""".stripMargin

  private val valueRead = "def f(s: Secret): String = s.value\n"

  private val made =
    "def f(): Secret = new Secret(Variable(\"ANTHROPIC_API_KEY\"), \"sk-planted\")\n"

  private val journaled = "def f(): Journaled[Secret] = summon[Journaled[Secret]]\n"

  private val probes = List(control, valueRead, made, journaled)

  private lazy val compiled = Probes.batch(prelude, probes)

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: a secret is read from the environment and revealed") {
      errors(control) ==> Nil
    }

    test("a secret's value read outside its package is rejected") {
      assert(errors(valueRead).exists(_.contains("value value cannot be accessed")))
    }

    test("a secret made from a string outside its package is rejected") {
      assert(errors(made).exists(_.contains("cannot be accessed")))
    }

    test("a secret has no Journaled instance") {
      assert(errors(journaled).exists(_.contains("Secret needs a Journaled instance")))
    }
  }
}
