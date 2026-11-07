package grit.eval.harness.bench

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.util.Try

import grit.core.id.Sha256

/** The standing text a grit coder's brief begins with: how a grit coder works, wherever it runs. */
final case class Standing private (text: String) extends caps.Pure {

  /** The digest of its text's UTF-8 bytes, by which a trial records it. */
  def sha256: Sha256 = Sha256.text(text)
}

object Standing {

  /** `arm`'s standing text; `None` for Claude Code's, whose agent prompt is its own. */
  def ofArm(arm: Arm): Option[Standing] = arm match {
    case g: Arm.Grit => Some(g.standing)
    case _: Arm.ClaudeCode => None
  }

  /** `text` as a standing text, or why not: it is blank. */
  def of(text: String): Either[String, Standing] =
    Either.cond(text.trim.nonEmpty, Standing(text), "the standing text is blank")

  /** The standing text the file at `file` holds, or why not, naming the file: it cannot be read
    * (it is missing, say), or it is blank.
    */
  def read(file: Path): Either[String, Standing] =
    Try(Files.readString(file, StandardCharsets.UTF_8)).toOption
      .toRight(s"the standing text's file $file cannot be read")
      .flatMap(text => of(text).left.map(_ => s"the standing text's file $file is blank"))
}
