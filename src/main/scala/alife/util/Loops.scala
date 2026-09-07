package alife.util

/**
 * Various efficient loop-like functions that are absent or inefficient in standard library.
 */
object Loops:
  /**
   * A forever loop that returns `Nothing`.
   * @param body the body to execute forever.
   * @return nothing
   */
  inline def forever(inline body: => Unit): Nothing =
    while true do body
    throw new AssertionError("Should never reach there")
  
  /**
   * This loops from an int `from`, inclusively,
   * until an int `until`, exclusively, and executes the specified body for each of the values.
   * 
   * This is an inline function which inlines the body, so accessing local `var`s does not incur a runtime penalty.
   * 
   * @param from the initial value (inclusive)
   * @param until the final value (exclusive)
   * @param body the loop body to execute.
   */
  inline def foreach(from: Int, until: Int)(inline body: Int => Unit): Unit =
    var i = from
    while i < until do
      body(i)
      i += 1
  
  /**
   * This loops from an int `from` to an int `to`, both inclusively,
   * and executes the specified body for each of the values.
   *
   * This is an inline function which inlines the body, so accessing local `var`s does not incur a runtime penalty.
   * No bound checking is performed; `to` equal to `Int.MaxValue` would loop foreever.
   *
   * @param from the initial value (inclusive)
   * @param to the final value (inclusive)
   * @param body the loop body to execute.
   */
  inline def foreachInclusive(from: Int, to: Int)(inline body: Int => Unit): Unit =
    var i = from
    while i <= to do
      body(i)
      i += 1
