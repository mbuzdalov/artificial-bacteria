package alife.util

/**
 * An auxiliary class that looks like a push-or-erase stack of `Double` values,
 * except that it also allows to access elements by their distance to the stack top.
 */
final class PseudoStack:
  private var myContents: Array[Double] = Array.ofDim[Double](16)
  private var mySize: Int = 0
  
  /**
   * Returns the number of elements currently on the stack.
   * @return the number of elements currently on the stack.
   */
  def size: Int = mySize
  
  /**
   * Pushes a new value onto the stack.
   * @param value the new value.
   */
  def push(value: Double): Unit =
    if mySize == myContents.length then
      myContents = java.util.Arrays.copyOf(myContents, 2 * myContents.length)
    myContents(mySize) = value
    mySize += 1
  
  /**
   * Returns the `index`-th element from the top of the stack. Here, `index` is one-based, so the last pushed element
   * will be returned for `apply(1)`. If `index` is invalid, returns `0.0`.
   * @param index the index of the element to return.
   * @return the element (or a zero if no such element).
   */
  def apply(index: Int): Double =
    val realIndex = mySize - index
    if index <= 0 || realIndex < 0 then 0.0 else myContents(realIndex)
  
  /**
   * Removes all elements from the stack.
   */
  def clear(): Unit =
    mySize = 0
