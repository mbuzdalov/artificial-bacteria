package alife.util

final class PseudoStack:
  private var myContents: Array[Double] = Array.ofDim[Double](16)
  private var mySize: Int = 0
  
  def size: Int = mySize
  def push(value: Double): Unit =
    if mySize == myContents.length then
      myContents = java.util.Arrays.copyOf(myContents, 2 * myContents.length)
    myContents(mySize) = value
    mySize += 1
  
  def apply(index: Int): Double =
    val realIndex = mySize - index
    if index <= 0 || realIndex < 0 then 0.0 else myContents(realIndex)
  
  def clear(): Unit =
    mySize = 0
