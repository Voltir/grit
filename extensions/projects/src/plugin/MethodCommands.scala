package grit.projects.plugin

import java.time.Instant

import scala.util.Try

import grit.core.claim.Claims
import grit.core.document.{DocRef, DocText, Document, DocumentKeeper, DocumentShelf}
import grit.core.id.DocKey
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{CommandBy, CommandOutcome, KeepingCommand, ReadingCommand}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.{Args, ArgsError, Field, ToolName, ToolSpec}
import grit.core.visibility.Label
import grit.projects.method.{
  Declared,
  Example,
  Method,
  MethodBench,
  MethodJson,
  Mover,
  TransitionCheck
}
import grit.projects.name.{CheckName, MethodName, StateName}

import ProjectArgs.parsed
import ProjectCommands.{PersonOnlyBy, moverOf, refusal, written}
import Stop.{answered, refuse, stored}
import MethodCommands.{Asking, Declaring}

/** `project_declare` and `project_methods`, and the method versions every other command reads:
  * each version one document of the plugin's own, under [[MethodCommands.key]] at `methodsAt`
  * alone (one digest to a version, whoever reads it), holding its method and worked examples as
  * [[MethodJson]] writes them, its checks found among `checks`.
  */
private[plugin] final class MethodCommands(checks: Vector[TransitionCheck], methodsAt: Label)
    extends caps.Pure {

  /** `project_declare`. */
  object Declare extends KeepingCommand[Declaring] {

    val spec: ToolSpec[Declaring] = ToolSpec(
      ToolName("project_declare"),
      "Declares a version of a method, from its JSON (as project_methods gives a version), once " +
        "its worked examples come out as they say. A version never changes: declaring one " +
        "declared already under the same digest changes nothing, and under another is refused. " +
        "Refused, too, naming each cause: the JSON does not read as a method, names a check " +
        "not compiled here, or an example comes out otherwise; given through a turn, or by a " +
        "caller believed below Authority.",
      Args
        .of((method = Field.text("The method version and its worked examples, as JSON.")))
        .refine(a =>
          Try(ujson.read(a.method)).toOption
            .toRight(ArgsError.Refused("method", "it is not JSON"))
            .map(Declaring(_))
        )
    )

    def run(
        args: Declaring,
        by: CommandBy,
        own: DocumentKeeper,
        claims: Claims,
        at: Instant
    )(using Tx^): Either[StoreError, CommandOutcome] = answered {
      for {
        _ <- direct(by)
        declared <- MethodJson
          .read(args.json, checks)
          .left
          .map(why => Stop.Refused(s"The method does not read: ${why.mkString("; ")}."))
        method = declared.method
        named = s"method ${MethodName.value(method.name)} version ${method.version}"
        _ <- MethodBench.failures(method, declared.examples) match {
          case Vector() => Right(())
          case failed =>
            Left(
              Stop.Refused(
                s"The $named is refused: its worked examples come out otherwise than they say: " +
                  failed.map(benched).mkString("; ") + "."
              )
            )
        }
        _ <- floored(named)
        held <- current(own, method.name, method.version)
        answer <- held match {
          case Some(document) =>
            readBack(document).flatMap(was =>
              Either.cond(
                was.method.digest == method.digest,
                s"The $named is ${Projects.DeclaredAlready}.",
                Stop.Refused(
                  s"The $named is declared already, under digest ${was.method.digest.hex}, and a " +
                    s"version never changes: declare this one as version ${method.version + 1}."
                )
              )
            )
          case None => kept(own, declared, named, at)
        }
      } yield answer
    }
  }

  /** `project_methods`. */
  object Methods extends ReadingCommand[Asking] {
    override def shown(args: Asking): String =
      args.name.fold("")(n => MethodName.value(n) + args.version.fold("")(v => s"@$v"))

    val spec: ToolSpec[Asking] = ToolSpec(
      ToolName("project_methods"),
      "Lists the methods declared here, each with its versions and their digests; given a " +
        "name, that method's versions; given a name and a version, that version as JSON, with " +
        "its worked examples, as project_declare takes it.",
      Args
        .of(
          (
            name = Field.text("The method's name.").optional,
            version = Field.count("One of its versions.", 1, Int.MaxValue).optional
          )
        )
        .refine(a =>
          for {
            name <- a.name.fold(Right(None))(n => parsed("name", n, MethodName.of).map(Some(_)))
            _ <- Either.cond(
              a.version.isEmpty || name.nonEmpty,
              (),
              ArgsError.Refused("version", "a version is asked of a method named")
            )
          } yield Asking(name, a.version)
        )
    )

    def run(args: Asking, by: CommandBy, own: PluginReads, at: Instant)(using
        Tx^
    ): Either[StoreError, CommandOutcome] = answered {
      (args.name, args.version) match {
        case (Some(name), Some(v)) =>
          for {
            document <- current(own.documents, name, v).flatMap(
              _.toRight(Stop.Refused(undeclared(name, v)))
            )
            was <- readBack(document)
          } yield ujson.write(MethodJson.written(was.method, was.examples), indent = 2)
        case (named, _) =>
          for {
            found <- versionsOf(own.documents, named)
            lines <- lined(found)
          } yield
            if (lines.isEmpty)
              named.fold("No method is declared here.")(n =>
                s"No method ${MethodName.value(n)} is declared here."
              )
            else lines.mkString("\n")
      }
    }

    /** One line per version `found` holds, `<name>@<version> <digest>`, or why one does not read. */
    private def lined(found: Vector[Document]): Either[Stop, Vector[String]] =
      found.foldLeft[Either[Stop, Vector[String]]](Right(Vector.empty)) { (so, d) =>
        so.flatMap(lines =>
          readBack(d).map(was => lines :+ s"${DocKey.value(d.key)} ${was.method.digest.hex}")
        )
      }
  }

  /** The latest version of the method `name` declared, or why none is. */
  def latest(own: DocumentShelf, name: MethodName)(using Tx^): Either[Stop, Method] =
    for {
      found <- versionsOf(own, Some(name))
      newest <- found.lastOption.toRight(
        Stop.Refused(
          s"No method ${MethodName.value(name)} is declared here: declare it with project_declare."
        )
      )
      was <- readBack(newest)
    } yield was.method

  /** The method `name` at `version`, as declared, or why not: it is not, or it does not read. */
  def version(own: DocumentShelf, name: MethodName, version: Int)(using
      Tx^
  ): Either[Stop, Method] =
    ifDeclared(own, name, version).flatMap {
      case Some(method) => Right(method)
      case None =>
        versionsOf(own, Some(name)).flatMap { found =>
          val versions = found.flatMap(d => keyed(d.key)).map(_.version).mkString(", ")
          Left(
            Stop.Refused(
              undeclared(name, version) +
                (if (versions.isEmpty) "" else s"; its versions here: $versions")
            )
          )
        }
    }

  /** The method `name` at `version`, as declared; `None` when it is not declared; or why it does
    * not read.
    */
  def ifDeclared(own: DocumentShelf, name: MethodName, version: Int)(using
      Tx^
  ): Either[Stop, Option[Method]] =
    current(own, name, version).flatMap {
      case Some(document) => readBack(document).map(d => Some(d.method))
      case None => Right(None)
    }

  /** Nothing when `by` moves as [[Mover.Person]] ([[ProjectCommands.moverOf]]); else a refusal. */
  private def direct(by: CommandBy): Either[Stop, Unit] = moverOf(by) match {
    case Mover.Person => Right(())
    case Mover.Other =>
      Left(
        Stop.Refused(
          s"A method is declared only by $PersonOnlyBy. Ask a person to declare it from the " +
            "method's file."
        )
      )
  }

  /** Keeps `declared` as its version's document, answering `named` declared; refused, keeping
    * nothing, when its JSON is over [[Document.DataMost]].
    */
  private def kept(own: DocumentKeeper, declared: Declared, named: String, at: Instant)(using
      Tx^
  ): Either[Stop, String] = {
    val method = declared.method
    val data = MethodJson.written(method, declared.examples)
    for {
      _ <- Document
        .oversized(data)
        .fold(Right(()))(why => Left(Stop.Refused(s"The $named is $why.")))
      key <- refuse(MethodCommands.key(method.name, method.version))
      text <- refuse(
        DocText.of(s"Method ${MethodName.value(method.name)} version ${method.version}.")
      )
      refs <- refuse(MethodCommands.refs(method.name))
      _ <- written(own.write(key, methodsAt, Tx.top, Place.Everywhere, text, data, None, at, refs))
    } yield s"Declared the $named, digest ${method.digest.hex}."
  }

  /** Nothing when this transaction writes at `methodsAt`; else why the `named` is refused. */
  private def floored(named: String)(using tx: Tx^): Either[Stop, Unit] = {
    val floor = Tx.floor(tx)
    Either.cond(
      floor == methodsAt,
      (),
      Stop.Refused(
        s"The $named is refused: methods are kept at ${Label.shown(methodsAt)}, which every " +
          "caller of the project commands reads, and this room keeps what it writes at " +
          s"${Label.shown(floor)}; declare it from a room labelled ${Label.shown(methodsAt)}."
      )
    )
  }

  /** The document of `name` at `version`, kept at `methodsAt`. */
  private def current(own: DocumentShelf, name: MethodName, version: Int)(using
      Tx^
  ): Either[Stop, Option[Document]] =
    for {
      key <- refuse(MethodCommands.key(name, version))
      held <- stored(own.current(key, methodsAt))
    } yield held

  /** Each version declared of `name`, or of every method when `None`, kept at `methodsAt`,
    * once, oldest version first within a method, the methods by name.
    */
  private def versionsOf(own: DocumentShelf, name: Option[MethodName])(using
      Tx^
  ): Either[Stop, Vector[Document]] =
    for {
      ref <- refuse(name.fold(MethodCommands.All)(MethodCommands.naming))
      found <- stored(own.referring(ref, Projects.Terms.bound))
    } yield found
      .filter(_.label == methodsAt)
      .distinctBy(_.key)
      .flatMap(d => keyed(d.key).map(k => (k.name, k.version, d)))
      .sortBy((n, v, _) => (n, v))
      .map(_._3)

  /** `document` read back as declared, or why not, naming it. */
  private def readBack(document: Document): Either[Stop, Declared] =
    MethodJson
      .read(document.data, checks)
      .left
      .map(why =>
        Stop.Refused(
          s"${DocKey.value(document.key)} is declared but does not read: ${why.mkString("; ")}"
        )
      )

  /** The method name and version a key [[MethodCommands.key]] wrote names; none for another key. */
  private def keyed(key: DocKey): Option[(name: String, version: Int)] =
    DocKey.value(key).split('@') match {
      case Array(n, v) => v.toIntOption.map(version => (name = n, version = version))
      case _ => None
    }

  private def undeclared(name: MethodName, version: Int): String =
    s"the method ${MethodName.value(name)} version $version is not declared here"

  /** `failed` in words. */
  private def benched(failed: MethodBench.Failed): String = {
    val expected = failed.move.expect match {
      case Example.Expect.Allowed => "allowed"
      case Example.Expect.RefusedBy(check) => s"refused by ${CheckName.value(check)}"
      case Example.Expect.RefusedPersonOnly =>
        s"refused as a move made only by $PersonOnlyBy"
    }
    s"example \"${failed.example}\", move ${failed.at} (${failed.move.key.value} to " +
      s"${StateName.value(failed.move.to)}) expected $expected; " +
      failed.met.fold("but allowed")(r => s"but ${refusal(r)}")
  }
}

private[plugin] object MethodCommands {

  /** `project_declare`'s argument: the method's JSON, read in the command. */
  final case class Declaring(json: ujson.Value) extends caps.Pure

  /** `project_methods`'s arguments: a method, and one of its versions. */
  final case class Asking(name: Option[MethodName], version: Option[Int]) extends caps.Pure

  /** The key `name` at `version` is kept under: `<name>@<version>`, which no artifact's or
    * project's key is (theirs hold no `@`).
    */
  def key(name: MethodName, version: Int): Either[String, DocKey] =
    DocKey.of(s"${MethodName.value(name)}@$version")

  /** The ref every method version holds. */
  val All: Either[String, DocRef] = DocRef.of("methods", "declared")

  /** The ref every version of `name` holds. */
  def naming(name: MethodName): Either[String, DocRef] = DocRef.of("method", MethodName.value(name))

  /** The refs a version of `name` holds. */
  def refs(name: MethodName): Either[String, Set[DocRef]] =
    for {
      all <- All
      named <- naming(name)
    } yield Set(all, named)
}
