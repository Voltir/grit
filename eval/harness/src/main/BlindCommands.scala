package grit.eval.harness.main

import java.nio.file.{Files, Path}

import scala.util.Try
import scala.util.hashing.MurmurHash3

import grit.dbos.internal.TrialRows
import grit.eval.harness.evidence.Blind

import Cli.*
import ProposeCommands.blindTask

/** `scripts/eval blind`: a task's diffs packed for a reviewer who sees no arm ([[Blind.packed]]). */
private[main] object BlindCommands {

  /** `args`' `--diff <label>=<file>` pairs, in order, and the arguments left. Refused naming the
    * argument when a `--diff` has no value, or its value no `=` with a label before it.
    */
  private[main] def diffFlags(
      args: List[String]
  ): Either[String, (Vector[(String, String)], List[String])] = args match {
    case Nil => Right((Vector.empty, Nil))
    case "--diff" :: pair :: more =>
      pair.indexOf('=') match {
        case at if at > 0 =>
          diffFlags(more).map((named: Vector[(String, String)], rest: List[String]) =>
            ((pair.take(at) -> pair.drop(at + 1)) +: named, rest)
          )
        case _ => Left(s"--diff $pair: give <label>=<file>")
      }
    case "--diff" :: Nil => Left("--diff takes <label>=<file>")
    case other :: more =>
      diffFlags(more).map((named: Vector[(String, String)], rest: List[String]) =>
        (named, other :: rest)
      )
  }

  /** `blind --task <name> --diff <label>=<file> … --tasks <dir> --rubric <file> --out <dir>`: the
    * frozen task `<name>`'s brief, the rubric and each diff, packed under `<out>`, seeded by `args`
    * (the same arguments packing alike), and the letters' key in `<out>/hidden/key.json`. Refused,
    * writing nothing, when `<out>` exists and is not empty, or a file is unread. Prints each
    * written file's path.
    */
  private[main] def blind(args: List[String]): Either[String, Unit] =
    diffFlags(args).flatMap((named: Vector[(String, String)], rest: List[String]) =>
      packed(args, named, rest)
    )

  private def packed(
      args: List[String],
      named: Vector[(String, String)],
      rest: List[String]
  ): Either[String, Unit] =
    for {
      f <- flags(rest)
      name <- need(f, "task").flatMap(TrialRows.TaskName.of(_).left.map(_.toString))
      task <- blindTask(f, name)
      rubric <- need(f, "rubric").flatMap(r => read(Path.of(r)))
      out <- need(f, "out").map(Path.of(_))
      _ <- emptyOrAbsent(out)
      diffs <- named.foldLeft[Either[String, Vector[(String, String)]]](Right(Vector.empty)) {
        case (acc, (label, file)) =>
          acc.flatMap(ds => read(Path.of(file)).map(d => ds :+ (label -> d)))
      }
      packed <- Blind.packed(task, rubric, diffs, MurmurHash3.seqHash(args).toLong)
      _ <- mkdirs(out.resolve("hidden"))
      _ <- packed.files.foldLeft[Either[String, Unit]](Right(())) { case (acc, (file, text)) =>
        acc.flatMap(_ => write(out.resolve(file), text))
      }
      key = out.resolve("hidden").resolve("key.json")
      _ <- write(
        key,
        ujson.Obj
          .from(packed.key.toVector.sortBy(_._1).map((k, v) => k -> ujson.Str(v)))
          .render(2) + "\n"
      )
    } yield {
      packed.files.foreach((file, _) => println(out.resolve(file)))
      println(key)
    }

  private def emptyOrAbsent(out: Path): Either[String, Unit] =
    Try(!Files.exists(out) || {
      val s = Files.list(out)
      try s.findAny().isEmpty
      finally s.close()
    }).toEither.left
      .map(e => s"$out unread: ${e.getClass.getName}")
      .flatMap(empty =>
        if (empty) Right(()) else Left(s"$out is not empty: pack into a new directory")
      )
}
