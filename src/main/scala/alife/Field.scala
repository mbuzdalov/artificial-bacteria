package alife

import alife.util.{Loops, PseudoStack}

import java.util.concurrent.ThreadLocalRandom
import scala.compiletime.uninitialized

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int):
  // cells stores the contents row first to align the access patterns with the screen buffers
  private val cells = Array.tabulate(height, width)((y, x) => Field.Cell(x, y, this))
  private val sumDistancesL, sumDistancesR = Array.ofDim[Int](height)
  private val callStack = PseudoStack()

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

  def increaseEnergy(x: Int, y: Int, radius: Int, amount: Double): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val cell = getCellChecked(x + xi, y + yi)
          val oldEnergy = cell.energy
          cell.setEnergy(oldEnergy + (amount * 1.05 - oldEnergy) * 0.5)

  def eraseEverything(x: Int, y: Int, radius: Int): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val cell = getCellChecked(x + xi, y + yi)
          cell.setEnergy(0)
          cell.setDebris(0)
          cell.setIndividual(null, 0, 0)
  
  private def performActionsOnIndividuals(constants: Field.Constants): Array[Int] =
    val actionCount = Array.ofDim[Int](Action.all.size)
    
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = getCell(x, y)
        val ind = cell.individual
        if ind != null then
          val g = ind.genome
          callStack.clear()
          Loops.foreach(0, g.size): i =>
            callStack.push(g(i).apply(this, x, y, callStack))
            
          // Outputs are organized as follows:
          // action:            0             1                2      ...
          // output:  callStack(1)  callStack(2)     callStack(3)     ...
          // order:   most recent   2nd most recent  3rd most recent  ...
          
          var chosenAction = -1
          Loops.foreach(0, math.min(g.size, Action.all.size)): i =>
            if Action.all(i).canApply(this, x, y, constants) then
              if chosenAction == -1 || callStack(i + 1) > callStack(chosenAction + 1) then
                chosenAction = i
          
          if chosenAction != -1 then
            val theAction = Action.all(chosenAction)
            ind.recordAction(theAction)
            theAction.apply(this, x, y, constants)
            actionCount(chosenAction) += 1

    actionCount
  
  private def drainIdleEnergy(constants: Field.Constants): Unit =
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = getCell(x, y)
        if cell.individual != null then
          val spentForLiving = math.min(constants.idleCost, cell.health)
          cell.setDebris(cell.debris + spentForLiving * constants.debrisFromActions)
          cell.setIndividual(cell.individual, cell.direction, cell.health - constants.idleCost)

  private def depositFoodAndConvertDebris(constants: Field.Constants, stepNumber: Int): Unit =
    val synthDecay = math.exp(-stepNumber * constants.synthesisDecay) // initially 1, then decreases to 0
    val sineDecay = math.exp(-stepNumber * constants.spotDecay) // initially 1, then decreases to 0
    val synthesisBase = 2 * (synthDecay * constants.synthesisInit + (1 - synthDecay) * constants.synthesisFinal)
    
    val spotXOffset = 2 * math.Pi * stepNumber * constants.spotSpeedX
    val spotYOffset = 2 * math.Pi * stepNumber * constants.spotSpeedY
    val spotXScale = math.Pi * constants.spotPeriodX / width
    val spotYScale = math.Pi * constants.spotPeriodY / height
    
    val debrisTotalDecay = math.max(0, 1 - constants.debrisDegradation - constants.debrisToEnergy)
    Loops.foreach(0, height): y =>
      val sinY = math.sin(y * spotYScale + spotYOffset)
      Loops.foreach(0, width): x =>
        val cosX = math.cos(x * spotXScale + spotXOffset)
        val cell = getCell(x, y)
        val newFoodScale = synthesisBase * ((1 - sineDecay) * cosX * cosX * sinY * sinY + sineDecay)
        val newFood = newFoodScale * ThreadLocalRandom.current().nextDouble()
        val d2e = cell.debris * constants.debrisToEnergy
        cell.setDebris(cell.debris * debrisTotalDecay)
        cell.setEnergy(cell.energy + d2e + newFood)
  
  private def computeStatistics(actionCount: Array[Int]): Field.StepStatistics =
    var maxHealth = 0.0
    var sumHealths = 0.0
    var sumEnergies = 0.0
    var maxEnergy = 0.0
    var maxGenomeSize = 0
    var sumGenomeSizes = 0L
    var nMonsters = 0
    var nBacteria = 0
    
    var maxLifeSpan = 0
    var maxChildren = 0
    var maxDistance = 0
    var maxSpeed = 0.0
    
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = getCell(x, y)
        sumEnergies += cell.energy
        maxEnergy = math.max(maxEnergy, cell.energy)
        val ind = cell.individual
        if ind != null then
          nBacteria += 1
          sumHealths += cell.health
          maxHealth = math.max(maxHealth, cell.health)
          if ind.label < 0 then nMonsters += 1
          maxGenomeSize = math.max(maxGenomeSize, ind.genome.size)
          sumGenomeSizes += ind.genome.size
          maxLifeSpan = math.max(maxLifeSpan, ind.lifeSpan)
          maxChildren = math.max(maxChildren, ind.numberOfChildren)
          maxDistance = math.max(maxDistance, ind.travelDistance)
          maxSpeed = math.max(maxSpeed, ind.averageSpeed)
    
    Field.StepStatistics(
      maxGenomeSize = maxGenomeSize,
      averageGenomeSize = sumGenomeSizes.toDouble / math.max(1, nBacteria),
      numberOfBacteria = nBacteria,
      averageHealth = sumHealths / math.max(1, nBacteria),
      maximalHealth = maxHealth,
      totalEnergy = sumEnergies,
      maxEnergy = maxEnergy,
      nEats = actionCount(Action.Eat.index),
      nForks = actionCount(Action.Fork.index),
      nMoves = actionCount(Action.Move.index),
      nClockwise = actionCount(Action.RotateMinus.index),
      nCounterClockwise = actionCount(Action.RotatePlus.index),
      maxLife = maxLifeSpan,
      maxChildren = maxChildren,
      maxTravelDistance = maxDistance,
      maxSpeed = maxSpeed,
      nMonsters = nMonsters,
    )
  
  def simulationStep(constants: Field.Constants, stepNumber: Int): Field.StepStatistics =
    val actionCount = performActionsOnIndividuals(constants)
    drainIdleEnergy(constants)
    depositFoodAndConvertDebris(constants, stepNumber)
    computeStatistics(actionCount)

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
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
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
    private var _energy: Double = 0.0
    private var _debris: Double = 0.0
    private var _health: Double = 0.0
    private var _direction: Int = 0
    private var _individual: Individual = uninitialized
    
    def energy: Double = _energy
    def debris: Double = _debris
    def health: Double = _health
    def weight: Int = if _individual == null then 0 else _individual.genome.length
    def direction: Int = _direction
    def individual: Individual = _individual
    
    def setEnergy(value: Double): Unit = _energy = value
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
  
  case class StepStatistics(maxGenomeSize: Int, averageGenomeSize: Double, numberOfBacteria: Int,
                            averageHealth: Double, maximalHealth: Double, totalEnergy: Double, maxEnergy: Double,
                            nEats: Int, nForks: Int, nMoves: Int, nClockwise: Int, nCounterClockwise: Int,
                            maxLife: Int, maxChildren: Int, maxTravelDistance: Int, maxSpeed: Double, nMonsters: Int)

  case class Constants(rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                       debrisDegradation: Double, debrisToEnergy: Double, debrisFromActions: Double,
                       synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                       idleCost: Double, healthMultiple: Double, healthIncrementMultiple: Double,
                       spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double,
                       spotDecay: Double, mutationOperator: Individual => Individual)
