package alife.util

class DelayGate:
  private var minimumDelayNanos: Long = 0
  private var nextAllowedTimeNanos: Long = System.nanoTime()
  private var lastExecutionTimeNanos: Long = 0
  private var lastLeadInTimeNanos: Long = 0

  def reset(): Unit =
    nextAllowedTimeNanos = System.nanoTime()
    
  def delay: Double = minimumDelayNanos / 1e9
  def setDelay(newDelay: Double): Unit = minimumDelayNanos = math.max(0, newDelay * 1e9).toLong
  def lastExecutionTime: Double = lastExecutionTimeNanos / 1e9
  def lastLeadInTime: Double = lastLeadInTimeNanos / 1e9
    
  inline def runOrSkip[T](inline body: => T): Option[T] =
    val entryTime = System.nanoTime()
    if entryTime >= nextAllowedTimeNanos then
      nextAllowedTimeNanos += minimumDelayNanos
      val result = body
      lastExecutionTimeNanos = System.nanoTime() - entryTime
      lastLeadInTimeNanos = lastExecutionTimeNanos
      Some(result)
    else None  
    
  inline def runOrWait[T](inline body: => T): T =
    val entryTime = System.nanoTime()
    var lastQueryTime = entryTime
    while lastQueryTime < nextAllowedTimeNanos do
      val diff = nextAllowedTimeNanos - lastQueryTime
      Thread.sleep(diff / 1000000, (diff % 1000000).toInt)
      lastQueryTime = System.nanoTime()
    nextAllowedTimeNanos += minimumDelayNanos
    val result = body
    val finishTime = System.nanoTime()
    lastExecutionTimeNanos = finishTime - lastQueryTime
    lastLeadInTimeNanos = finishTime - entryTime
    result
