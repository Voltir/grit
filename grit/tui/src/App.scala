package grit.tui

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal
import scala.util.Using

import org.jline.terminal.{Terminal, TerminalBuilder}
import java.util.concurrent.LinkedBlockingQueue
import grit.tui.key.Key
import grit.tui.model.Msg
import grit.tui.term.KeyReader
import java.util.concurrent.BlockingQueue

object App {

  private type Event = Msg | Key

  private val tickRate: FiniteDuration = 120.millis

  def main(args: Array[String]): Unit = Using.Manager {
    use =>
      val events = new LinkedBlockingQueue[Event]

      println("todo")
      val terminal = use(openTerminal())

    use(startReader(new KeyReader(terminal), events))
    use(ticking("tui-tick", tickRate)(() => {
      // events.put(Event.Message(Msg.Tick));
      events.put(Msg.Tick)
      true
    }))
    var running = true
    while (running) {
      val msg = events.take() match {
        case m: Msg => m
        case _: Key => Msg.Quit
      }

      running = msg != Msg.Quit

      if (running) {}

    }
  }.get

  private def ticking(name: String, periodMillis: FiniteDuration)(
      step: () => Boolean
  ): Background^{step} = Background.start(name) { () =>
    var more = true
    while (more) {
      Thread.sleep(periodMillis.toMillis)
      more = step()
    }
  }

  private def startReader(reader: KeyReader, queue: BlockingQueue[Event]): Background =
    Background.start("tui-input") { () =>
      try {
        while (!Thread.currentThread().isInterrupted) {
          queue.put(reader.read())
        }
      } catch {
        // Throwing InterruptedException clears the interrupt flag, so it has to be put
        // back before the flag is tested below.
        case _: InterruptedException => Thread.currentThread().interrupt()
        case NonFatal(_) => ()
      }
      // Input has ended — a closed pty, or the terminal going away. Without this the
      // loop would block on an empty queue forever and the process would hang with the
      // terminal still in raw mode. A close, by contrast, is not news for the loop.
      if (!Thread.currentThread().isInterrupted) {
        queue.put(Msg.Quit)
      }
    }

  private def openTerminal(): Terminal =
    try { TerminalBuilder.builder().system(true).ffm(true).build() }
    catch {
      case NonFatal(e) =>
        System.err.println(
          s"FFM terminal provider unavailable (${e.getMessage}); auto-detecting"
        )
        TerminalBuilder.builder().system(true).build()
    }
}
