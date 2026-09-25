package alife

import alife.util.Loops.*

import scala.compiletime.uninitialized

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int):
  // squares stores the contents row first to align the access patterns with the screen buffers
  private val squares = Array.tabulate(height, width)((y, x) => Field.Square(x, y, this))
  private val sumDistancesL, sumDistancesR = Array.ofDim[Int](height)

  def deepCopy(): Field =
    val result = Field(width, height)
    System.arraycopy(sumDistancesL, 0, result.sumDistancesL, 0, sumDistancesL.length)
    System.arraycopy(sumDistancesR, 0, result.sumDistancesR, 0, sumDistancesR.length)
    loopFromUntil(0, height): y =>
      loopFromUntil(0, width): x =>
        result.squares(y)(x).copyFrom(squares(y)(x))
    result    
  
  def getSquare(x: Int, y: Int): Field.Square = squares(y)(x)

  def getSquareChecked(x: Int, y: Int): Field.Square =
    val x0 = (x % width + width) % width
    val y0 = (y % height + height) % height
    getSquare(x0, y0)
  
  def getRelativeSquare(x: Int, y: Int, relativeLocation: Int): Field.Square =
    val dirFW = squares(y)(x).individual.direction
    val dirLF = (dirFW + 1) & 3
    val scaleFW = Field.relativeLocationsFW(relativeLocation)
    val scaleLF = Field.relativeLocationsLF(relativeLocation)
    val realX = x + Field.directionX(dirFW) * scaleFW + Field.directionX(dirLF) * scaleLF
    val realY = y + Field.directionY(dirFW) * scaleFW + Field.directionY(dirLF) * scaleLF
    getSquareChecked(realX, realY)
  
  def getSumOfDistancesFromLeft(y: Int): Int = sumDistancesL(y)
  def getSumOfDistancesFromRight(y: Int): Int = sumDistancesR(y)

  def findAndMarkLongestGenome(label: Int): Unit = labelMaxIndividual(_.genome.size, 0, label)
  def findAndMarkMostProductive(label: Int): Unit = labelMaxIndividual(_.numberOfChildren, 0, label)
  def findAndMarkFastest(label: Int): Unit = labelMaxIndividual(_.averageSpeed, 0.0, label)

  private def labelMaxIndividual[T: Ordering as o](fun: Individual => T, defVal: T, label: Int): Unit =
    var t = defVal
    forEachIndividual((_, ind) => t = o.max(t, fun(ind)))
    forEachIndividual: (c, ind) =>
      if o.equiv(t, fun(ind)) then
        ind.setLabel(label)

  private inline def forEachIndividual(inline fun: (Field.Square, Individual) => Unit): Unit =
    loopFromUntil(0, height): y =>
      loopFromUntil(0, width): x =>
        val sq = getSquare(x, y)
        val ind = sq.individual
        if ind != null then fun(sq, ind)
end Field

object Field:
  private val directionX = Array(1, 0, -1, 0)
  private val directionY = Array(0, 1, 0, -1)
  private val relativeLocationsFW = Array(0, 1,  0, 0,  1, 1, 2,  0, 0)
  private val relativeLocationsLF = Array(0, 0, -1, 1, -1, 1, 0, -2, 2)

  val relativeLocationForward = 1
  val numberOfRelativeLocations: Int = relativeLocationsFW.length

  class Square(x: Int, y: Int, f: Field):
    private var _food: Double = 0.0
    private var _debris: Double = 0.0
    private var _individual: Individual = uninitialized
    
    def food: Double = _food
    def debris: Double = _debris
    def individual: Individual = _individual
    
    def setFood(value: Double): Unit = _food = value
    def setDebris(value: Double): Unit = _debris = value
    
    def eraseEverything(): Unit =
      if _individual != null then removeIndividual()
      _food = 0.0
      _debris = 0.0
    
    def removeIndividual(): Individual =
      require(_individual != null)
      val result = _individual
      _individual = null
      result.unpin()
      f.sumDistancesL(y) -= x
      f.sumDistancesR(y) -= f.width - 1 - x
      result

    def setIndividual(individual: Individual): Unit =
      require(_individual == null)
      _individual = individual
      _individual.pin()
      f.sumDistancesL(y) += x
      f.sumDistancesR(y) += f.width - 1 - x

    def copyFrom(that: Square): Unit =
      _food = that._food
      _debris = that._debris
      _individual = if that._individual != null then that._individual.deepCopy() else null
    end copyFrom
  end Square
end Field
