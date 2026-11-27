package grit.host.checkout

import java.nio.file.{Files, Path}

import grit.core.host.{AbsPath, Reach, Reached}

/** Where the paths a command is given lead from the checkout `root`, on this machine's file
  * system.
  */
final class LocalReach(root: Path) extends Reach {
  import Checkout.attempt

  def reached(path: String): Reached = {
    val top = attempt(root.toRealPath()).getOrElse(root.toAbsolutePath)
    // `toRealPath` and `toAbsolutePath` give only absolute paths; were the root's not one,
    // nothing would be inside it.
    AbsPath
      .of(top.toString)
      .fold(
        _ => Reached.Outside(AbsPath.Root),
        real => {
          val at = walk(if (path.startsWith("/")) AbsPath.Root else real, parts(path), 0)
          if (AbsPath.within(at, real)) Reached.Inside else Reached.Outside(at)
        }
      )
  }

  /** `rest` taken from `at`, a path whose links are all followed, each link met followed in
    * turn: as the kernel reads a path, up to [[LocalReach.Hops]] links, past which, or past
    * a part that cannot be read, the rest is read as written.
    */
  private def walk(at: AbsPath, rest: List[String], hops: Int): AbsPath = rest match {
    case Nil => at
    case ("" | ".") :: more => walk(at, more, hops)
    case ".." :: more => walk(AbsPath.resolve(at, ".."), more, hops)
    case name :: more =>
      val next = AbsPath.resolve(at, name)
      val link =
        if (hops >= LocalReach.Hops) None
        else
          attempt {
            val p = Path.of(AbsPath.value(next))
            if (Files.isSymbolicLink(p)) Some(Files.readSymbolicLink(p).toString) else None
          }.toOption.flatten
      link match {
        case Some(target) =>
          walk(if (target.startsWith("/")) AbsPath.Root else at, parts(target) ++ more, hops + 1)
        case None => walk(next, more, hops)
      }
  }

  private def parts(path: String): List[String] = path.split('/').toList
}

object LocalReach {

  /** 40: the most links followed in one path, as Linux's `ELOOP` limit. */
  val Hops = 40
}
