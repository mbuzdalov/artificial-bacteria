package alife

import alife.util.Loops.*

import scala.compiletime.uninitialized

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int):
  // cells stores the contents row first to align the access patterns with the screen buffers
  private val cells = Array.tabulate(height, width)((y, x) => Field.Cell(x, y, this))
  private val sumDistancesL, sumDistancesR = Array.ofDim[Int](height)

  def getCell(x: Int, y: Int): Field.Cell = cells(y)(x)

  def getCellChecked(x: Int, y: Int): Field.Cell =
    val x0 = (x % width + width) % width
    val y0 = (y % height + height) % height
    getCell(x0, y0)
  
  def getRelativeCell(x: Int, y: Int, relativeLocation: Int): Field.Cell =
    val dirFW = cells(y)(x).direction
    val dirLF = (dirFW + 1) & 3
    val scaleFW = Field.relativeLocationsFW(relativeLocation)
    val scaleLF = Field.relativeLocationsLF(relativeLocation)
    val realX = x + Field.directionX(dirFW) * scaleFW + Field.directionX(dirLF) * scaleLF
    val realY = y + Field.directionY(dirFW) * scaleFW + Field.directionY(dirLF) * scaleLF
    getCellChecked(realX, realY)
  
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
        c.setIndividual(ind.copy(label = label), c.direction, c.health)

  private inline def forEachIndividual(inline fun: (Field.Cell, Individual) => Unit): Unit =
    loopFromUntil(0, height): y =>
      loopFromUntil(0, width): x =>
        val cell = getCell(x, y)
        val ind = cell.individual
        if ind != null then fun(cell, ind)
end Field

object Field:
  private val directionX = Array(1, 0, -1, 0)
  private val directionY = Array(0, 1, 0, -1)
  private val relativeLocationsFW = Array(0, 1,  0, 0,  1, 1,  0, 0, 2)
  private val relativeLocationsLF = Array(0, 0, -1, 1, -1, 1, -2, 2, 0)

  val relativeLocationForward = 1
  val numberOfRelativeLocations: Int = relativeLocationsFW.length

  class Cell(x: Int, y: Int, f: Field):
    private var _food: Double = 0.0
    private var _debris: Double = 0.0
    private var _health: Double = 0.0
    private var _direction: Int = 0
    private var _individual: Individual = uninitialized
    
    def food: Double = _food
    def debris: Double = _debris
    def health: Double = _health
    def weight: Int = if _individual == null then 0 else _individual.genome.length
    def direction: Int = _direction
    def individual: Individual = _individual
    
    def setFood(value: Double): Unit = _food = value
    def setDebris(value: Double): Unit = _debris = value
    def setIndividual(g: Individual, d: Int, h: Double): Unit =
      if _individual != null then
        f.sumDistancesL(y) -= x
        f.sumDistancesR(y) -= f.width - 1 - x
      if h < 0 || g == null then
        _individual = null
        _health = 0
        _direction = 0
      else
        _individual = g
        _health = h
        _direction = d
        f.sumDistancesL(y) += x
        f.sumDistancesR(y) += f.width - 1 - x
