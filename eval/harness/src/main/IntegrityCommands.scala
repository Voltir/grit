package grit.eval.harness.main

import java.time.Instant

import scala.util.Try

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.message.Cost
import grit.dbos.sql.DbConfig
import grit.eval.harness.loss.{LossWindow, Measure}
import grit.kit.environment.DotEnv
import grit.models.JevClassifier
import grit.models.JevConfig

import Cli.*

/** `integrity-loss`: the lost-tool measure over a database's recent turns. */
private[main] object IntegrityCommands {

  /** `integrity-loss --url <jdbc> --since <instant>`: the lost-tool measure ([[Measure]]) over
    * the turns the database at `<jdbc>` created from `<instant>` to `clock`'s now, printed
    * ([[Measure.lines]]), with how many roots Jev was asked and what it reported they cost. It
    * reads one window of step outputs ([[LossWindow.gates]]) and the entries of the
    * conversations whose roots are asked, and writes nothing. Each turn that had a write-out
    * tool withheld has its root asked of Jev once ([[grit.eval.harness.loss.WriteOut]]),
    * uncached; its key
    * comes from the environment ([[DotEnv.loadDefault]]), read only when a
    * root is to be asked, and the database's login from `GRIT_DATABASE_USER` and `_PASSWORD`.
    */
  private[main] def integrityLoss(f: Map[String, String], clock: Clock^): Either[String, Unit] =
    for {
      url <- need(f, "url")
      since <- need(f, "since").flatMap(s =>
        Try(Instant.parse(s)).toOption.toRight(s"--since $s: not an instant (2026-10-14T00:00:00Z)")
      )
      env <- DotEnv.loadDefault(sys.env)
      config <- DbConfig.fromEnv(env.updated(DbConfig.UrlVar, url)).left.map(_.message)
      lines <- opened(config) { reader =>
        for {
          gated <- LossWindow.gates(reader, since, clock.now())
          asking = Measure.toAsk(gated)
          roots <- LossWindow.roots(reader, asking)
          said = asking.flatMap(t => roots.get(t).flatMap(_.toOption).map(t -> _))
          // Jev's settings are read only when a root is to be asked.
          jevConfig <-
            if (said.isEmpty) Right(None)
            else JevConfig.fromEnv(env).left.map(_.message).map(Some(_))
        } yield {
          val jev: Classifier^ = jevConfig match {
            case Some(c) => JevClassifier(c)
            case None => Classifier.none("no root was to be asked")
          }
          val (answered, spent) = LossWindow.ask(jev, said)
          val asked =
            roots.map((t, r) => t -> r.flatMap(_ => answered.getOrElse(t, Left("not asked"))))
          Measure.lines(Measure.of(gated, asked)) :+
            s"Jev: asked ${said.size}, answered ${answered.count(_._2.isRight)}, spent " +
            Cost.cents(spent)
        }
      }
    } yield lines.foreach(println)
}
