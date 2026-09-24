package grit.tui.runtime

import java.util.concurrent.{
  ConcurrentHashMap,
  ExecutorService,
  Executors,
  ScheduledExecutorService,
  ScheduledFuture,
  TimeUnit
}

/** Cancellable timers on virtual threads.
  *
  * A capability, and it says so in its type: anything that can arm a timer has to be
  * handed one. `update` is not, which is why it returns [[Effect]] values instead.
  *
  * One platform thread does the waiting and dispatches the work onto virtual threads, so
  * a pending timer costs a queue entry rather than a parked carrier. Next door,
  * `Cmd.afterMs` was `Future { Thread.sleep(d); … }` on the global fork-join pool: every
  * pending timer held a worker for its whole delay, and three modest chains -- a 40ms
  * autoscroll, a 120ms streamer, a 200ms heartbeat -- permanently held three of them.
  *
  * Timers are keyed by [[TimerId]] and **at most one is pending per id**: arming an id
  * that is already pending cancels the old one first. That is the structural fix for the
  * forking chain that generation counters were invented to paper over.
  */
final class Scheduler private (
    private val clock: ScheduledExecutorService,
    private val workers: ExecutorService
) extends caps.SharedCapability {

  private val pending = new ConcurrentHashMap[String, ScheduledFuture[?]]()
  @volatile private var closed = false

  /** Run `task` after `delayMs`, replacing any timer already pending under `id`.
    *
    * A no-op once [[close]] has been called, so a teardown race cannot resurrect a timer
    * after the terminal has been given back.
    */
  def after(id: TimerId, delayMs: Long)(task: () => Unit): Unit = {
    if (!closed) {
      cancel(id)
      val key = id.name
      // Ascribed rather than inferred: ScheduledExecutorService.schedule is overloaded
      // (Runnable and Callable), and capture checking crashes the compiler on overloaded
      // Java calls -- the dotty bug the fullscreen spike hit and worked around the same way.
      val run: Runnable^{task, this} = () => {
        pending.remove(key)
        val work: Runnable^{task} = () => task()
        if (!closed) { val _ = workers.submit(work) }
      }
      val handle = clock.schedule(run, math.max(0L, delayMs), TimeUnit.MILLISECONDS)
      // `put` and `remove` answer null when nothing was pending under the key.
      Option(pending.put(key, handle)).foreach(prior => { val _ = prior.cancel(false) })
      // Lost a race with close(): make sure nothing outlives the shutdown.
      if (closed) { cancel(id) }
    }
  }

  /** Drop the timer pending under `id`. A no-op when none is.
    *
    * This is a real cancel -- the task never runs -- not a stale-generation stamp that
    * lets it fire and be discarded by `update`. Both defences are cheap; only this one
    * also stops the work.
    */
  def cancel(id: TimerId): Unit = {
    Option(pending.remove(id.name)).foreach(prior => { val _ = prior.cancel(false) })
  }

  /** True while a timer is pending under `id` -- for tests and for the status line. */
  def isPending(id: TimerId): Boolean = pending.containsKey(id.name)

  /** How many timers are pending. */
  def pendingCount: Int = pending.size

  /** Cancel everything and stop both pools. Idempotent: the flag is set before the work,
    * so a second call from a `finally` or a shutdown hook does nothing.
    *
    * Ordering matters at teardown -- schedulers stop *before* the terminal is restored,
    * or a late timer paints escape sequences into a shell already back in cooked mode.
    */
  def close(): Unit = {
    if (!closed) {
      closed = true
      pending.values.forEach(f => { val _ = f.cancel(false) })
      pending.clear()
      clock.shutdownNow()
      workers.shutdownNow()
    }
  }
}

object Scheduler {

  /** A scheduler with its own clock thread and a virtual thread per fired task.
    *
    * The caller owns it and must [[Scheduler.close]] it -- before restoring the terminal.
    */
  def create(): Scheduler = {
    val clock = Executors.newSingleThreadScheduledExecutor { (r: Runnable) =>
      val t = new Thread(r, "tui-clock")
      t.setDaemon(true)
      t
    }
    new Scheduler(clock, Executors.newVirtualThreadPerTaskExecutor())
  }

  /** Releases the scheduler, so `Using` gives it back in reverse acquisition order. */
  given releasable[C^]: scala.util.Using.Releasable[Scheduler^{C}] = (s: Scheduler^{C}) => s.close()
}
