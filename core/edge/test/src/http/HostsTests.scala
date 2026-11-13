package grit.edge.http

import utest.*

/** [[Hosts.admit]]: which requests a listener's declared names let through, so that a page
  * another site serves reaches no route through DNS rebinding or a cross-site request.
  */
object HostsTests extends TestSuite {

  private def request(headers: (String, String)*): Request =
    Request
      .of(Verb.Post, "/command/x", headers.toVector, "")
      .fold(why => throw new java.lang.AssertionError(why), r => r)

  private def hosts(names: String*): Hosts =
    Hosts.of(names.toVector).fold(why => throw new java.lang.AssertionError(why), h => h)

  val tests = Tests {
    test("a Host naming a loopback name passes at any port and in any case") {
      Vector(
        "localhost:7317",
        "LocalHost:80",
        "localhost",
        "127.0.0.1:7317",
        "[::1]:7317",
        "[::1]"
      ).map(h => Hosts.loopback.admit(request("Host" -> h))) ==> Vector.fill(6)(true)
    }

    test("a Host naming no declared name, or none at all, is refused") {
      Vector(
        Hosts.loopback.admit(request("Host" -> "rebound.example:7317")),
        Hosts.loopback.admit(request("Host" -> "127.0.0.2:7317")),
        Hosts.loopback.admit(request())
      ) ==> Vector(false, false, false)
    }

    test("a name declared for a wildcard bind passes; loopback's then do not") {
      val box = hosts("Box.lan", "10.0.0.5")
      Vector(
        box.admit(request("Host" -> "box.LAN:7317")),
        box.admit(request("Host" -> "10.0.0.5:7317")),
        box.admit(request("Host" -> "localhost:7317"))
      ) ==> Vector(true, true, false)
    }

    test("an Origin passes only as http:// the request's own Host, in any case") {
      def at(origin: String) =
        Hosts.loopback.admit(request("Host" -> "localhost:7317", "Origin" -> origin))
      Vector(
        at("http://localhost:7317"),
        at("HTTP://LOCALHOST:7317"),
        at("http://localhost:3000"),
        at("https://localhost:7317"),
        at("http://127.0.0.1:7317"),
        at("null")
      ) ==> Vector(true, true, false, false, false, false)
    }

    test("an IPv6 Origin passes as its bracketed Host") {
      Hosts.loopback.admit(request("Host" -> "[::1]:7317", "Origin" -> "http://[::1]:7317")) ==>
        true
    }

    test("a Sec-Fetch-Site other than same-origin or none is refused, with no Origin sent") {
      def site(s: String) =
        Hosts.loopback.admit(request("Host" -> "localhost:7317", "Sec-Fetch-Site" -> s))
      Vector(site("same-origin"), site("None"), site("cross-site"), site("same-site")) ==>
        Vector(true, true, false, false)
    }

    test("of refuses no names, an empty one, or one no host is written as") {
      Vector(
        Hosts.of(Vector.empty),
        Hosts.of(Vector("")),
        Hosts.of(Vector("a b")),
        Hosts.of(Vector("[::1]"))
      ).map(_.left.toOption) ==> Vector(
        Some("a listener's hosts name one at least"),
        Some("\"\" is no host's name"),
        Some("\"a b\" is no host's name"),
        Some("\"[::1]\" is no host's name")
      )
    }

    test("a name is ASCII labels, each starting and ending with a letter or digit, or IPv6 hex") {
      val names = Vector(".", "-", "a..b", "-a.example", "a-.example", "١::١", "ｆe80::1")
      names.map(n => Hosts.of(Vector(n)).left.toOption) ==>
        names.map(n => Some(s"\"$n\" is no host's name"))
      Vector("my-host.example", "a", "127.0.0.1", "::1", "FE80::1", "::ffff:127.0.0.1")
        .map(n => Hosts.of(Vector(n)).isRight) ==> Vector(true, true, true, true, true, true)
    }
  }
}
