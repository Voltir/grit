package grit.edge.http

import utest.*

/** [[Listen]]: the forms of address it reads, the ones it refuses, and which are loopback. */
object ListenTests extends TestSuite {

  private def read(text: String): (String, Int) =
    Listen.parse(text).fold(why => throw new java.lang.AssertionError(why), l => (l.host, l.port))

  private def loopback(text: String): Boolean =
    Listen.parse(text).fold(why => throw new java.lang.AssertionError(why), _.loopback)

  val tests = Tests {
    test("an IPv4 literal, a bracketed IPv6 literal and a name are read with their port") {
      (
        read("127.0.0.1:7317"),
        read("[::1]:7317"),
        read("[2001:db8::7]:443"),
        read("localhost:0"),
        read("edge-1.example.org:65535")
      ) ==> (
        ("127.0.0.1", 7317),
        ("::1", 7317),
        ("2001:db8::7", 443),
        ("localhost", 0),
        ("edge-1.example.org", 65535)
      )
    }

    test("text that is not host:port is refused, each saying what it must be") {
      val refused = Vector(
        "",
        "7317",
        ":7317",
        "localhost:",
        "localhost:65536",
        "localhost:-1",
        "localhost:7x",
        "::1:7317",
        "[::1]7317",
        "[::1:7317",
        "[]:7317",
        "[zz::1]:7317",
        "[1:2:3:4:5:6:7:8:9]:7317",
        "[1::2::3]:7317",
        "256.0.0.1:7317",
        "1.2.3:7317",
        "ho st:7317",
        "host_name:7317"
      )
      refused.filter(Listen.parse(_).isRight) ==> Vector.empty
      Listen.parse("::1:7317") ==> Left(
        "an IPv6 host must be in brackets, as [::1]:7317"
      )
      Listen.parse("localhost:65536") ==> Left(
        "the port must be a number from 0 to 65535, not 65536"
      )
    }

    test("loopback is 127.0.0.0/8, ::1 however written, and localhost; a wildcard is not") {
      Vector("127.0.0.1", "127.255.0.9", "localhost", "LocalHost")
        .map(h => loopback(s"$h:1")) ==> Vector(true, true, true, true)
      Vector("[::1]", "[0:0:0:0:0:0:0:1]", "[0::1]", "[::0001]")
        .map(h => loopback(s"$h:1")) ==> Vector(true, true, true, true)
      Vector(
        "0.0.0.0",
        "[::]",
        "10.0.0.1",
        "128.0.0.1",
        "[::2]",
        "[::ffff:127.0.0.1]",
        "example.org",
        "localhost.example.org"
      ).map(h => loopback(s"$h:1")) ==> Vector.fill(8)(false)
    }

    test("shown is read back by parse as the same address") {
      val texts = Vector("127.0.0.1:7317", "[::1]:0", "[2001:db8::7]:443", "edge.example:80")
      texts.map(t => Listen.parse(t).map(_.shown)) ==> texts.map(Right(_))
      texts.map(t => Listen.parse(t).flatMap(l => Listen.parse(l.shown)) == Listen.parse(t)) ==>
        Vector.fill(4)(true)
    }
  }
}
