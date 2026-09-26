package grit.core.period

/** The lifecycle's settings in force: the `windows` periods close and are purged by, and
  * how many of a conversation's newest closing entries open each turn's window
  * (`closings`, at least 0).
  */
final case class LifecycleSettings private (windows: Windows, closings: Int)

object LifecycleSettings {

  /** The settings, or why not: `closings` must not be negative. */
  def of(windows: Windows, closings: Int): Either[String, LifecycleSettings] =
    Either.cond(
      closings >= 0,
      new LifecycleSettings(windows, closings),
      "closings must not be negative"
    )

  /** [[Windows.Default]], and 3 closing entries. */
  val Default: LifecycleSettings = new LifecycleSettings(Windows.Default, 3)
}
