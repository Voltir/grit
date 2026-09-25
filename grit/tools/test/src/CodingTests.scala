package grit.tools

import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.host.*
import grit.core.id.ToolCallId
import grit.core.message.AssistantBlock
import grit.core.tool.{Bound, Outcome, ToolName, Toolbox}

import utest.*

/** [[Coding]]'s tools over a scripted host: what each call reads, what a person is asked, what
  * reaches the host, and what the model reads back.
  */
object CodingTests extends TestSuite {

  /** A host that notes each call and answers as scripted. A fake of stateful capabilities,
    * so it may hold its log (STYLE rule 7).
    */
  final class Scripted(
      edited: Either[EditError, Edited] = Right(Edited(Vector(3))),
      ran: Either[HostError, Ran] = Right(Ran(0, Clipped.tail("ok\n", _ => None)))
  ) extends Workspace,
        Edits,
        Shell {
    @caps.unsafe.untrackedCaptures
    var log = Vector.empty[String]

    private def note(s: String): Unit = log = log :+ s

    def read(path: RelPath, lines: Lines): Either[HostError, Clipped] = {
      note(s"read ${RelPath.value(path)} ${lines.offset} ${lines.limit}")
      if (RelPath.value(path) == "gone") Left(HostError.NotFound(path))
      else Right(Clipped.head("text", _ => Some("hint")))
    }

    def list(dir: RelPath, depth: Int): Either[HostError, Clipped] = {
      note(s"list ${RelPath.value(dir)} $depth")
      Right(Clipped.head("a/\nb", _ => None))
    }

    def search(pattern: String, under: RelPath): Either[HostError, Clipped] = {
      note(s"search $pattern ${RelPath.value(under)}")
      Right(Clipped.head("No matches.", _ => None))
    }

    def write(path: RelPath, text: String): Either[HostError, Unit] = {
      note(s"write ${RelPath.value(path)} ${text.length}")
      Right(())
    }

    def edit(path: RelPath, edits: Seq[Replace]): Either[EditError, Edited] = {
      note(s"edit ${RelPath.value(path)} ${edits.mkString(";")}")
      edited
    }

    def run(command: String, timeout: FiniteDuration): Either[HostError, Ran] = {
      note(s"run $command ${timeout.toSeconds}")
      ran
    }
  }

  private def readOnly(host: Scripted^): Toolbox[{host}] =
    Coding.readOnly(host).fold(d => throw new java.lang.AssertionError(d), identity)

  private def all(host: Scripted^): Toolbox[{host}] =
    Coding.all(host, host, host).fold(d => throw new java.lang.AssertionError(d), identity)

  private def call(name: String, args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), name, args)

  /** `name` called with `args` against every tool over `host`: what it asks, and what it
    * came to when run, approved when it asks.
    */
  private def settle(
      host: Scripted,
      name: String,
      args: ujson.Value
  ): Either[String, (Option[String], Outcome)] =
    all(host).bind(call(name, args)).left.map(_.message).map {
      case free: Bound.Free => (None, free())
      case gated: Bound.Gated => (Some(gated.ask), gated(Approval.Approved))
    }

  val tests = Tests {
    test("the tool sets") {
      val host = new Scripted()
      readOnly(host).names.map(ToolName.value) ==> Vector("read", "list", "search")
      all(host).names.map(ToolName.value) ==>
        Vector("read", "list", "search", "write", "edit", "run")
    }

    test("each call is shown in one line: the tool, then what it acts on") {
      val box = all(new Scripted())
      val calls = Vector(
        "read" -> ujson.Obj("path" -> "src/a.scala", "offset" -> 10),
        "list" -> ujson.Obj(),
        "search" -> ujson.Obj("pattern" -> "def \\w+", "path" -> "src"),
        "write" -> ujson.Obj("path" -> "a.txt", "content" -> "x\ny"),
        "edit" -> ujson.Obj(
          "path" -> "a.txt",
          "edits" -> ujson.Arr(ujson.Obj("oldText" -> "x", "newText" -> "y"))
        ),
        "run" -> ujson.Obj("command" -> "git status\n  && ls")
      )
      calls.map((name, args) => box.bind(call(name, args)).map(_.shown)) ==> Vector(
        Right("read src/a.scala:10"),
        Right("list ."),
        Right("search \"def \\w+\" src"),
        Right("write a.txt"),
        Right("edit a.txt"),
        Right("run git status && ls")
      )
    }

    test("under a strict schema every field is required, an optional one nullable") {
      val host = new Scripted()
      val read = readOnly(host).schemas(strict = true).find(_.name == "read")
      read.map(_.parameters("required")) ==> Some(ujson.Arr("path", "offset", "limit"))
      read.map(_.parameters("properties")("offset")("type")) ==>
        Some(ujson.Arr("integer", "null"))
    }

    test("descriptions state their limits") {
      val schemas = all(new Scripted()).schemas(strict = false)
      val described = schemas.map(s => s.name -> s.description).toMap
      assert(
        described.get("read").exists(_.contains("2000 lines or 50 KB")),
        described.get("run").exists(_.contains("60 seconds when not given")),
        described.get("edit").exists(_.contains("exactly once")),
        described.get("search").exists(_.contains("cut at 500 characters"))
      )
    }

    test("read runs free, passing offset and limit") {
      val host = new Scripted()
      settle(host, "read", ujson.Obj("path" -> "src/a.scala", "offset" -> 10, "limit" -> 5)) ==>
        Right((None, Outcome.Done("text\n\n[hint]")))
      settle(host, "read", ujson.Obj("path" -> "./b", "offset" -> ujson.Null)) ==>
        Right((None, Outcome.Done("text\n\n[hint]")))
      host.log ==> Vector("read src/a.scala 10 Some(5)", "read b 1 None")
    }

    test("a host failure is a failed outcome in its words") {
      settle(new Scripted(), "read", ujson.Obj("path" -> "gone")) ==>
        Right((None, Outcome.Failed("There is no file or directory `gone`.")))
    }

    test("a path outside the checkout is refused before anything runs") {
      val host = new Scripted()
      val refused = settle(host, "read", ujson.Obj("path" -> "../etc/passwd"))
      assert(
        refused.left.exists(_.contains("`../etc/passwd` leads outside the checkout")),
        refused.left.exists(_.contains("You sent:"))
      )
      settle(host, "write", ujson.Obj("path" -> ".env", "content" -> "x")).isLeft ==> true
      host.log ==> Vector()
    }

    test("list and search default to the root") {
      val host = new Scripted()
      settle(host, "list", ujson.Obj()) ==> Right((None, Outcome.Done("a/\nb")))
      settle(host, "list", ujson.Obj("path" -> "src", "depth" -> 3)).isRight ==> true
      settle(host, "list", ujson.Obj("depth" -> 9)).isLeft ==> true
      settle(host, "search", ujson.Obj("pattern" -> "def .*")) ==>
        Right((None, Outcome.Done("No matches.")))
      host.log ==> Vector("list . 1", "list src 3", "search def .* .")
    }

    test("write asks with a preview, then writes") {
      val host = new Scripted()
      val text = (1 to 20).map(i => s"l$i").mkString("\n")
      settle(host, "write", ujson.Obj("path" -> "a.txt", "content" -> text)) ==> Right(
        (
          Some(
            "Write a.txt (20 lines):\n" + (1 to 12).map(i => s"+ l$i").mkString("\n") +
              "\n  … 8 more lines"
          ),
          Outcome.Done("Wrote a.txt: 20 lines.")
        )
      )
      host.log ==> Vector("write a.txt 70")
    }

    test("edit asks with each replacement, then edits") {
      val host = new Scripted()
      val args = ujson.Obj(
        "path" -> "a.txt",
        "edits" -> ujson.Arr(
          ujson.Obj("oldText" -> "one\ntwo", "newText" -> "2"),
          ujson.Obj("oldText" -> "x", "newText" -> "y")
        )
      )
      settle(host, "edit", args) ==> Right(
        (
          Some(
            "Edit a.txt (2 replacements):\n@@ edits[0]\n- one\n- two\n+ 2\n@@ edits[1]\n- x\n+ y"
          ),
          Outcome.Done("Edited a.txt: 1 replacement, starting on line 3.")
        )
      )
      host.log ==> Vector("edit a.txt Replace(one\ntwo,2);Replace(x,y)")
    }

    test("an edit's failure names the edit") {
      val host = new Scripted(edited = Left(EditError.NotFound(RelPath.Root, 1)))
      val args = ujson.Obj(
        "path" -> "a",
        "edits" -> ujson.Arr(ujson.Obj("oldText" -> "a", "newText" -> "b"))
      )
      settle(host, "edit", args).map(_._2) ==> Right(
        Outcome.Failed(EditError.NotFound(RelPath.Root, 1).message)
      )
    }

    test("an edit with a bad item is refused at its path") {
      val args = ujson.Obj("path" -> "a", "edits" -> ujson.Arr(ujson.Obj("oldText" -> "a")))
      assert(
        settle(new Scripted(), "edit", args).left
          .exists(_.contains("`edits[0].newText` is missing"))
      )
    }

    test("run asks with the command, then reports the exit code") {
      val host = new Scripted(ran = Right(Ran(3, Clipped.tail("boom\n", _ => None))))
      settle(host, "run", ujson.Obj("command" -> "make", "timeout" -> 5)) ==> Right(
        (
          Some("Run in the checkout (timeout 5 s):\nmake"),
          Outcome.Done("Exit code 3. Output:\nboom\n")
        )
      )
      settle(host, "run", ujson.Obj("command" -> "true")).map(_._1) ==>
        Right(Some("Run in the checkout (timeout 60 s):\ntrue"))
      settle(host, "run", ujson.Obj("command" -> "x", "timeout" -> 601)).isLeft ==> true
      host.log ==> Vector("run make 5", "run true 60")
    }

    test("a command with no output, and one that timed out") {
      val quiet = new Scripted(ran = Right(Ran(0, Clipped.tail("", _ => None))))
      settle(quiet, "run", ujson.Obj("command" -> "true")).map(_._2) ==>
        Right(Outcome.Done("Exit code 0; no output."))
      val slow =
        new Scripted(ran = Left(HostError.TimedOut(5.seconds, Clipped.tail("so far", _ => None))))
      settle(slow, "run", ujson.Obj("command" -> "sleep 9")).map(_._2) ==>
        Right(
          Outcome.Failed("The command was killed after 5 seconds. Its output until then:\nso far")
        )
    }
  }
}
