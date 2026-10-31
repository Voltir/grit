package grit.host.checkout

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.host.*

import utest.*

/** [[LocalProcesses]] against a temporary root. */
object LocalProcessesTests extends TestSuite {

  private def argv(program: String, args: String*): Argv = Argv.of(program, args*) match {
    case Right(a) => a
    case Left(why) => throw new java.lang.AssertionError(why)
  }

  /** A fresh root, given to `body`, deleted after. */
  private def inRoot[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-processes")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  val tests = Tests {
    test("a started program answers each line of its input on its output") {
      inRoot { dir =>
        val answered = new LocalProcesses(dir, sys.env).started(argv("cat"), RelPath.Root) { s =>
          val in = s.input
          val out = new BufferedReader(new InputStreamReader(s.output, StandardCharsets.UTF_8))
          in.write("one\n".getBytes(StandardCharsets.UTF_8))
          in.flush()
          val first = out.readLine()
          in.write("two\n".getBytes(StandardCharsets.UTF_8))
          in.close()
          val second = out.readLine()
          (first, second, s.await(5.seconds).map(_.exit))
        }
        answered ==> Right(("one", "two", Right(0)))
      }
    }

    test("leaving its scope kills a program still running, and what it started") {
      inRoot { dir =>
        val pids = new LocalProcesses(dir, sys.env).started(
          argv("sh", "-c", "sleep 60 & echo $$ $!; wait"),
          RelPath.Root
        ) { s =>
          new BufferedReader(new InputStreamReader(s.output, StandardCharsets.UTF_8)).readLine()
        }
        val alive = pids.map(_.split(' ').toList.map { pid =>
          ProcessHandle.of(pid.toLong).map(_.isAlive).orElse(false)
        })
        alive ==> Right(List(false, false))
      }
    }

    test("a started program past its wait is killed and reported timed out with its errors") {
      inRoot { dir =>
        val waited = new LocalProcesses(dir, sys.env).started(
          argv("sh", "-c", "echo late >&2; exec sleep 30"),
          RelPath.Root
        )(s => s.await(300.millis))
        waited.map(_.left.map {
          case HostError.TimedOut(after, errors) => (after, errors.show)
          case other => (0.seconds, other.message)
        }) ==> Right(Left((300.millis, "late\n")))
      }
    }

    test("a program runs with its arguments as given, in its directory, to its end") {
      inRoot { dir =>
        Files.createDirectory(dir.resolve("sub"))
        val ran = new LocalProcesses(dir, sys.env).run(
          argv("sh", "-c", "pwd; echo \"$1\"; echo err >&2; exit 3", "sh", "a; b"),
          RelPath.of("sub").getOrElse(RelPath.Root),
          10.seconds
        )
        ran.map(r => (r.exit, r.output.show)) ==>
          Right((3, s"${dir.toRealPath().resolve("sub")}\na; b\nerr\n"))
      }
    }

    test("a program sees no variable named like a secret") {
      inRoot { dir =>
        val environment = sys.env ++ Map("ANTHROPIC_API_KEY" -> "planted", "LANG" -> "C.UTF-8")
        val names = new LocalProcesses(dir, environment)
          .run(argv("env"), RelPath.Root, 10.seconds)
          .map(_.output.show.linesIterator.map(_.takeWhile(_ != '=')).toSet)
        names.map(n => (n.contains("ANTHROPIC_API_KEY"), n.contains("LANG"))) ==>
          Right((false, true))
      }
    }

    test("a program that is not there is refused as failing to start, where one that is runs") {
      inRoot { dir =>
        val processes = new LocalProcesses(dir, sys.env)
        val missing = argv("grit-no-such-program")
        def failed[A](r: Either[HostError, A]): Boolean = r match {
          case Left(HostError.Failed(_)) => true
          case _ => false
        }
        (
          failed(processes.run(missing, RelPath.Root, 10.seconds)),
          failed(processes.started(missing, RelPath.Root)(_ => "ran")),
          processes.run(argv("true"), RelPath.Root, 10.seconds).map(_.exit),
          processes.started(argv("true"), RelPath.Root)(_ => "ran")
        ) ==> (true, true, Right(0), Right("ran"))
      }
    }

    test("a directory that is not there is refused before anything starts") {
      inRoot { dir =>
        val processes = new LocalProcesses(dir, sys.env)
        val gone = RelPath.of("gone").getOrElse(RelPath.Root)
        processes.run(argv("true"), gone, 10.seconds).left.map(_.message) ==>
          Left(HostError.NotFound(gone).message)
      }
    }
  }
}
