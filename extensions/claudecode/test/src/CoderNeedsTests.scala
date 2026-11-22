package grit.claudecode

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.commands.machine.MachineName
import grit.commands.routes.RouteFixtures.Machine

import utest.*

/** scripts/coder-needs in a checkout joined to the edge itself, every other program it asks
  * (getconf, docker, ssh, the scout binary) a fake in the machine's `bin`, and origin a bare
  * repository beside the checkout.
  */
object CoderNeedsTests extends TestSuite {

  private def env(name: String): Path =
    Path.of(sys.env.getOrElse(name, throw new java.lang.AssertionError(s"no $name")))

  /** The fakes of a machine that lacks nothing, over `f`'s joined checkout. */
  private final class Needs(val f: HookFixtures) {
    val machine: JoinFixtures = f.machine
    val scout: Path =
      machine.write(machine.root.resolve("scout-bin/scout"), "#!/bin/sh\nexit 2\n", true)

    /** A getconf whose `GNU_LIBC_VERSION` runs `answer`; any other variable it does not know. */
    def getconf(answer: String): Path =
      machine.write(
        machine.bin.resolve("getconf"),
        s"#!/bin/sh\n[ \"$$*\" = GNU_LIBC_VERSION ] || exit 1\n$answer\n",
        executable = true
      )

    /** A docker whose `info` exits `info`, and whose compose prints `port`, when there is one,
      * only when asked where a compose file's postgres publishes 5432.
      */
    def docker(info: Int, port: String): Path =
      machine.write(
        machine.bin.resolve("docker"),
        s"""#!/bin/sh
           |case "$$1" in
           |  info) exit $info ;;
           |  compose)
           |    [ "$$2" = -f ] && [ "$$4 $$5 $$6" = 'port postgres 5432' ] || exit 1
           |    [ -n '$port' ] && echo '$port' ;;
           |esac
           |""".stripMargin,
        executable = true
      )

    /** The bare repository every checkout here has as origin. */
    val origin: Path = machine.root.resolve("origin.git")

    /** The real scripts/scout, and a stand-in for each tool the gate fetches, put into
      * `checkout`.
      */
    def ready(checkout: Path): Unit = {
      val _ = machine.write(
        checkout.resolve("scripts/scout"),
        Files.readString(env("GRIT_SCOUT_SCRIPT")),
        executable = true
      )
      Fetched.foreach { tool =>
        val _ = machine.write(checkout.resolve(s"tools/$tool"), "#!/bin/sh\n", executable = true)
      }
    }

    locally {
      val _ = getconf("echo 'glibc 2.39'")
      val _ = docker(0, "0.0.0.0:5432")
      ready(f.proj)
      val _ = machine.sh(machine.root, "git init -q --bare origin.git")
      val _ = machine.sh(f.proj, s"git remote add origin '${origin}'")
    }

    /** coder-needs run in `in`, given `more` besides SCOUT naming the fake binary. */
    def run(more: Map[String, String] = Map.empty, in: Path = f.proj): Bash.Done =
      Bash.run(
        in,
        machine.home,
        machine.bin,
        Seq(env("GRIT_CODER_NEEDS").toString),
        env = Map("SCOUT" -> scout.toString) ++ more
      )
  }

  private def needs[A](body: Needs => A): A = HookFixtures(f => body(new Needs(f)))

  /** A PATH of `bin` and every system program but `name`. */
  private def without(n: Needs, name: String): String = {
    val tools = Files.createDirectories(n.machine.root.resolve("tools"))
    Bash.SystemPath.split(':').reverse.foreach { d =>
      val dir = Path.of(d)
      if (Files.isDirectory(dir))
        Files.list(dir).iterator().asScala.foreach { p =>
          val link = tools.resolve(p.getFileName)
          if (
            p.getFileName.toString != name && !Files.exists(
              link,
              java.nio.file.LinkOption.NOFOLLOW_LINKS
            )
          )
            Files.createSymbolicLink(link, p)
        }
    }
    Files.deleteIfExists(n.machine.bin.resolve(name))
    s"${n.machine.bin}:$tools"
  }

  private val Docker = "for the local databases and the integration suites"

  /** The tools under a checkout's `tools/` that the gate needs, each fetched by its own script. */
  private val Fetched: Vector[String] = Vector("enola", "scalafmt", "shellcheck")

  val tests = Tests {
    test(
      "on a machine lacking nothing, one line names the machine the edge knows, and it exits 0"
    ) {
      needs { n =>
        val done = n.run()
        (done.exit, done.out) ==> ((
          0,
          Vector(
            s"coder-needs: nothing lacking; the edge names this machine ${MachineName.value(Machine)}"
          )
        ))
      }
    }

    test("a glibc older than 2.39, or none, is named as what Mill's launcher needs") {
      needs { n =>
        val _ = n.getconf("echo 'glibc 2.35'")
        val old = n.run()
        val _ = n.getconf("exit 1")
        val none = n.run()
        val lack = "for Mill's native launcher, which ./mill runs"
        (old.exit, old.out, none.exit, none.out) ==> ((
          1,
          Vector(s"coder-needs: lacks glibc 2.39 or later (this has glibc 2.35), $lack"),
          1,
          Vector(s"coder-needs: lacks glibc 2.39 or later (this has none), $lack")
        ))
      }
    }

    test("no scout binary where scripts/scout looks is named, with where to put one") {
      needs { n =>
        val done = n.run(Map("SCOUT" -> n.machine.root.resolve("nowhere/scout").toString))
        (done.exit, done.out) ==> ((
          1,
          Vector(
            "coder-needs: lacks the scout binary, for reading Scala: link it at ~/Projects/Spikes/scout-bin/scout or set SCOUT"
          )
        ))
      }
    }

    test("each fetched tool the gate needs, missing, is named with the script that fetches it") {
      needs { n =>
        Fetched.foreach(tool => Files.delete(n.f.proj.resolve(s"tools/$tool")))
        val _ = n.machine.write(n.f.proj.resolve("tools/scalafmt"), "not a program\n")
        val done = n.run()
        (done.exit, done.out) ==> ((
          1,
          Vector(
            "coder-needs: lacks tools/enola, for the gate: bash scripts/fetch-enola.sh",
            "coder-needs: lacks tools/scalafmt, for the gate: bash scripts/fetch-scalafmt.sh",
            "coder-needs: lacks tools/shellcheck, for the gate: bash scripts/fetch-shellcheck.sh"
          )
        ))
      }
    }

    test(
      "docker absent, its daemon not answering, and Postgres not publishing 5432 each name one lack"
    ) {
      needs { n =>
        val _ = n.docker(1, "0.0.0.0:5432")
        val down = n.run()
        val _ = n.docker(0, "0.0.0.0:5433")
        val unpublished = n.run()
        val absent = n.run(Map("PATH" -> without(n, "docker")))
        (
          absent.out,
          down.out,
          unpublished.out,
          Set(absent.exit, down.exit, unpublished.exit)
        ) ==> ((
          Vector(s"coder-needs: lacks docker on PATH, $Docker"),
          Vector(
            s"coder-needs: lacks a running Docker daemon (docker info did not answer), $Docker"
          ),
          Vector(
            "coder-needs: lacks the compose Postgres publishing port 5432, for the local databases: " +
              "docker compose up -d, or restart its container once nothing else holds 5432"
          ),
          Set(1)
        ))
      }
    }

    test(
      "no origin, and an origin that does not authenticate, are each named as a push credential's lack"
    ) {
      needs { n =>
        val _ = n.machine.sh(n.f.proj, "git remote remove origin")
        val none = n.run()
        val _ = n.machine.sh(n.f.proj, "git remote add origin ssh://git@example.invalid/r.git")
        val _ =
          n.machine.write(n.machine.bin.resolve("ssh"), "#!/bin/sh\nexit 255\n", executable = true)
        val ssh = n.run()
        // Fetched from the bare origin, which answers; pushed to https, which no helper gives
        // a credential for.
        val _ = n.machine.sh(n.f.proj, s"git remote set-url origin '${n.origin}'")
        val _ =
          n.machine.sh(n.f.proj, "git remote set-url --push origin https://example.invalid/r.git")
        val https = n.run()
        def refused(url: String) =
          s"coder-needs: lacks a git credential that can push to origin ($url), for pushing built branches"
        (none.exit, none.out, ssh.exit, ssh.out, https.exit, https.out) ==> ((
          1,
          Vector("coder-needs: lacks a remote origin, for pushing built branches"),
          1,
          Vector(refused("ssh://git@example.invalid/r.git")),
          1,
          Vector(refused("https://example.invalid/r.git"))
        ))
      }
    }

    test(
      "an ssh origin only the checkout's core.sshCommand reaches is reached by both probes, as git reaches it"
    ) {
      needs { n =>
        // The ssh on PATH reaches nothing; the configured one runs git's remote command against
        // the bare origin beside the checkout, as a forge's server would.
        val _ =
          n.machine.write(n.machine.bin.resolve("ssh"), "#!/bin/sh\nexit 255\n", executable = true)
        val configured = n.machine.write(
          n.machine.root.resolve("configured-ssh"),
          s"#!/bin/sh\nfor a; do last=$$a; done\ncd '${n.machine.root}' && exec sh -c \"$$last\"\n",
          executable = true
        )
        val _ = n.machine.sh(n.f.proj, "git remote set-url origin git@example.invalid:origin.git")
        val _ =
          n.machine.sh(n.f.proj, s"git config core.sshCommand '$configured -o IdentitiesOnly=yes'")
        val done = n.run()
        (done.exit, done.out) ==> ((
          0,
          Vector(
            s"coder-needs: nothing lacking; the edge names this machine ${MachineName.value(Machine)}"
          )
        ))
      }
    }

    test(
      "an origin that lists its refs but refuses a push is named as lacking write access, with git's first error line"
    ) {
      needs { n =>
        // The bare origin answers ls-remote; its receive-pack, which a dry-run push still opens,
        // refuses as a forge refuses a credential without write access.
        val refuse = n.machine.write(
          n.machine.root.resolve("refusing-receive-pack"),
          "#!/bin/sh\necho 'ERROR: Permission to r.git denied to someone.' >&2\nexit 1\n",
          executable = true
        )
        val _ = n.machine.sh(n.f.proj, s"git config remote.origin.receivepack '$refuse'")
        val done = n.run()
        val refs = n.machine.sh(n.machine.root, "git --git-dir=origin.git for-each-ref")
        (done.exit, done.out, refs) ==> ((
          1,
          Vector(
            "coder-needs: lacks write access to origin as git authenticates here " +
              "(ERROR: Permission to r.git denied to someone.), for pushing built branches"
          ),
          Vector()
        ))
      }
    }

    test(
      "a checkout never joined, and an edge that does not answer, are each named as the edge's lack"
    ) {
      needs { n =>
        val alone = n.machine.checkout("alone")
        val _ = n.machine.sh(alone, s"git remote add origin '${n.origin}'")
        n.ready(alone)
        val unjoined = n.run(in = alone)
        val gone = HookFixtures.closedPort
        val down = n.run(Map("GRIT_EDGE" -> s"http://127.0.0.1:$gone"))
        (unjoined.exit, unjoined.out, down.exit, down.out) ==> ((
          1,
          Vector(
            s"coder-needs: lacks this checkout's join to the edge (no hook at ${n.machine.hook(alone)}), " +
              "for claims and briefs: run the edge's join"
          ),
          1,
          Vector(
            s"coder-needs: lacks the edge's answer for this checkout's claim holder (holder: the edge at " +
              s"http://127.0.0.1:$gone did not answer), for claims and briefs"
          )
        ))
      }
    }

    test("glibc 2.38 lacks, and 2.39, 2.40 and 3.0 lack nothing") {
      needs { n =>
        def lacking(version: String): Vector[String] = {
          val _ = n.getconf(s"echo 'glibc $version'")
          n.run().out.filter(_.contains("glibc"))
        }
        Vector("2.38", "2.39", "2.40", "3.0").map(lacking) ==> Vector(
          Vector(
            "coder-needs: lacks glibc 2.39 or later (this has glibc 2.38), " +
              "for Mill's native launcher, which ./mill runs"
          ),
          Vector(),
          Vector(),
          Vector()
        )
      }
    }
    test(
      "an https push URL a credential helper answers for is proved by a push, not taken on the credential's word"
    ) {
      needs { n =>
        val _ =
          n.machine.sh(n.f.proj, "git remote set-url --push origin https://example.invalid/r.git")
        val _ = n.machine.sh(
          n.f.proj,
          "git config credential.helper '!f() { echo username=u; echo password=p; }; f'"
        )
        // A proxy at a closed loopback port fails the push with no lookup and no network; what
        // git says of it is curl's, so only coder-needs' own words around it are pinned.
        val _ = n.machine.sh(
          n.f.proj,
          s"git config http.proxy http://127.0.0.1:${HookFixtures.closedPort}"
        )
        val done = n.run()
        val refused = done.out.filter(line =>
          line.startsWith(
            "coder-needs: lacks write access to origin as git authenticates here ("
          ) && line.endsWith("), for pushing built branches")
        )
        (done.exit, done.out.size, refused.size) ==> ((1, 1, 1))
      }
    }
    test("outside a git checkout it says to run it in one, and exits 2") {
      needs { n =>
        val outside = Files.createDirectories(n.machine.root.resolve("outside"))
        val done = n.run(in = outside)
        (done.exit, done.out, done.err.trim) ==>
          ((2, Vector(), "coder-needs: not inside a git checkout; run it in one"))
      }
    }
    test("a hook that answers with no holder is named as the edge's lack") {
      needs { n =>
        val quiet = n.machine.checkout("quiet")
        val _ = n.machine.sh(quiet, s"git remote add origin '${n.origin}'")
        n.ready(quiet)
        val _ = n.machine.write(n.machine.hook(quiet), "#!/bin/sh\nexit 0\n", executable = true)
        val done = n.run(in = quiet)
        (done.exit, done.out) ==> ((
          1,
          Vector(
            "coder-needs: lacks the edge's answer for this checkout's claim holder (it printed none), " +
              "for claims and briefs"
          )
        ))
      }
    }
    test("every lack at once is one line each, in a fixed order") {
      needs { n =>
        val _ = n.getconf("exit 1")
        val _ = n.docker(1, "")
        val _ = n.machine.sh(n.f.proj, "git remote remove origin")
        Files.delete(n.f.proj.resolve("tools/enola"))
        val done = n.run(
          Map(
            "SCOUT" -> n.machine.root.resolve("nowhere/scout").toString,
            "GRIT_EDGE" -> s"http://127.0.0.1:${HookFixtures.closedPort}"
          )
        )
        (done.exit, done.out.map(_.split(' ').take(4).mkString(" "))) ==> ((
          1,
          Vector(
            "coder-needs: lacks glibc 2.39",
            "coder-needs: lacks the scout",
            "coder-needs: lacks tools/enola, for",
            "coder-needs: lacks a running",
            "coder-needs: lacks a remote",
            "coder-needs: lacks the edge's"
          )
        ))
      }
    }
  }
}
