package alife.util

/**
 * A utility class that allows to space out execution of given events.
 * The typical use is to limit the processing rate of certain updates, such as image refresh.
 * One may either skip the event because it is not yet the time (using `runOrSkip`)
 * or to wait until it is the time (using `runOrWait`).
 */
class DelayGate:
  private val myOffset = System.nanoTime()
  private var minimumDelayNanos: Long = 0
  private var nextAllowedTimeNanos: Long = 0
  private var lastExecutionTimeNanos: Long = 0
  private var lastLeadInTimeNanos: Long = 0

  private def relativeNanoTime(): Long = System.nanoTime() - myOffset
  
  /**
   * Resets the clock, so that the next invocation of either `runOrSkip` or `runOrWait` is immediately successful.
   */
  def reset(): Unit =
    nextAllowedTimeNanos = relativeNanoTime()
  
  /**
   * Returns the current configured delay, in seconds.
   * @return the current configured delay.
   */
  def delay: Double = minimumDelayNanos / 1e9
  
  /**
   * Configures the new delay, given in seconds. Negative values are silently rounded to a zero.
   * The specified delay is not a real-time guarantee, the measurements happen on a best-effort basis.
   * @param newDelay the new delay.
   */
  def setDelay(newDelay: Double): Unit = minimumDelayNanos = math.max(0, newDelay * 1e9).toLong
  
  /**
   * Returns the last execution time of a task guarded by this gate, in seconds. If no tasks have ever been executed, returns zero.
   * The execution time is measured on a best-effort basis as a timestamp difference between the moments when the
   * task is about to be called and when the task has just returned.
   * @return the last execution time.
   */
  def lastExecutionTime: Double = lastExecutionTimeNanos / 1e9
  
  /**
   * Returns the lead-in time for the last task guarded by this gate, in seconds.
   * This is the time between the corresponding method (`runOrSkip` or `runOrWait`) has entered and the task finished executing.
   * If no tasks have ever been executed, returns zero.
   *
   * When using `runOrSkip`, this time is either not updated when the task is skipped, or is equal to `lastExecutionTime`
   * when it is executed. When using `runOrWait`, this time includes the wait time.
   * @return
   */
  def lastLeadInTime: Double = lastLeadInTimeNanos / 1e9
  
  /**
   * Runs the task given as `body` if the current time is past the gate trigger time, or skips it and returns immediately.
   * On a successful execution, the gate trigger time is advanced by the configured delay time.
   *
   * This is an inline method with inlined body for performance.
   *
   * @param body the body of the task to execute.
   * @tparam T the type of the value returned by the task.
   * @return `None` if the task was skipped, or `Some` containing the result of task execution.
   */
  inline def runOrSkip[T](inline body: => T): Option[T] =
    val entryTime = relativeNanoTime()
    if entryTime >= nextAllowedTimeNanos then
      nextAllowedTimeNanos += minimumDelayNanos
      val result = body
      lastExecutionTimeNanos = relativeNanoTime() - entryTime
      lastLeadInTimeNanos = lastExecutionTimeNanos
      Some(result)
    else None
  
  /**
   * Runs the task given as `body`, doing this immediately if the current time is past the gate trigger time,
   * and waiting for the trigger time to start if not. The gate trigger time is advanced by the configured delay time
   * after the task is executed.
   *
   * This is an inline method with inlined body for performance.
   *
   * @param body the body of the task to execute.
   * @tparam T the type of the value returned by the task
   * @return whatever the task returns.
   */
  inline def runOrWait[T](inline body: => T): T =
    val entryTime = relativeNanoTime()
    var lastQueryTime = entryTime
    while lastQueryTime < nextAllowedTimeNanos do
      val diff = nextAllowedTimeNanos - lastQueryTime
      Thread.sleep(diff / 1000000, (diff % 1000000).toInt)
      lastQueryTime = relativeNanoTime()
    nextAllowedTimeNanos += minimumDelayNanos
    val result = body
    val finishTime = relativeNanoTime()
    lastExecutionTimeNanos = finishTime - lastQueryTime
    lastLeadInTimeNanos = finishTime - entryTime
    result
