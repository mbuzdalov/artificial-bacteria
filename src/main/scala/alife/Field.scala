package alife

import alife.util.{Loops, PseudoStack}

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

  def increaseFood(x: Int, y: Int, radius: Int, targetAmount: Double): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val cell = getCellChecked(x + xi, y + yi)
          val oldFood = cell.food
          cell.setFood(oldFood + (targetAmount - oldFood) * 0.5)

  def eraseEverything(x: Int, y: Int, radius: Int): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val cell = getCellChecked(x + xi, y + yi)
          cell.setFood(0)
          cell.setDebris(0)
          cell.setIndividual(null, 0, 0)
  
  private def performActionsOnIndividuals(config: Config): Array[Int] =
    val actions = config.actions
    val actionCount = Array.ofDim[Int](actions.size)
    
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
          Loops.foreach(0, math.min(g.size, actions.size)): i =>
            if actions(i).canApply(this, x, y, config) then
              if chosenAction == -1 || callStack(i + 1) > callStack(chosenAction + 1) then
                chosenAction = i
          
          if chosenAction != -1 then
            val theAction = actions(chosenAction)
            ind.recordAction(theAction)
            theAction.apply(this, x, y, config)
            actionCount(chosenAction) += 1

    actionCount
  
  private def drainIdleEnergy(config: Config): Unit =
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = getCell(x, y)
        if cell.individual != null then
          val spentForLiving = math.min(config.idleCost, cell.health)
          cell.setDebris(cell.debris + spentForLiving * config.debrisFromActions)
          cell.setIndividual(cell.individual, cell.direction, cell.health - config.idleCost)

  private def depositFoodAndConvertDebris(config: Config, stepNumber: Int): Unit =
    val synthDecay = math.exp(-stepNumber * config.synthesisDecay) // initially 1, then decreases to 0
    val sineDecay = math.exp(-stepNumber * config.spotDecay) // initially 1, then decreases to 0
    
    // This is the average expected energy to deposit onto a cell.
    // "Average" means it can go up and down, currently in a periodic way.
    // "Expected" means that the actual deposited amount is sampled u.a.r. from [0; the value determined for the cell].
    val expectedFoodPerCell = synthDecay * config.synthesisInit + (1 - synthDecay) * config.synthesisFinal
    
    val pi2 = 2 * math.Pi
    val spotXOffset = pi2 * stepNumber * config.spotSpeedX
    val spotYOffset = pi2 * stepNumber * config.spotSpeedY
    val spotXScale = pi2 * config.spotPeriodX / width
    val spotYScale = pi2 * config.spotPeriodY / height
    
    val debrisTotalDecay = math.max(0, 1 - config.debrisDegradation - config.debrisToFood)
    Loops.foreach(0, height): y =>
      // this is in [0;1]
      val changeY = (math.sin(y * spotYScale + spotYOffset) + 1) / 2
      Loops.foreach(0, width): x =>
        val changeX = (math.sin(x * spotXScale + spotXOffset) + 1) / 2
        val cell = getCell(x, y)
        // The product of changes is additionally multiplied by 4,
        // because the integral of changeX * changeY over the entire field is 1/4.
        // This way, `newFoodScale` is exactly `expectedFoodPerCell` on average, which is what we want.
        val newFoodScale = expectedFoodPerCell * (sineDecay + (1 - sineDecay) * changeX * changeY * 4)
        val newFood = newFoodScale * config.random.nextDouble(0, 2)
        val d2e = cell.debris * config.debrisToFood
        cell.setDebris(cell.debris * debrisTotalDecay)
        cell.setFood(cell.food + d2e + newFood)
  
  private def computeStatistics(config: Config, actionCount: Array[Int]): Field.StepStatistics =
    var maxHealth = 0.0
    var sumHealths = 0.0
    var totalFood = 0.0
    var maxFood = 0.0
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
        totalFood += cell.food
        maxFood = math.max(maxFood, cell.food)
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
      totalFood = totalFood,
      maxFood = maxFood,
      // the following selectors are not efficient, but this action is by far not a bottleneck
      nEats = actionCount(config.actions.indexOf(Action.Eat)),
      nForks = actionCount(config.actions.indexOf(Action.Fork)),
      nMoves = actionCount(config.actions.indexOf(Action.Move)),
      nClockwise = actionCount(config.actions.indexOf(Action.RotatePlus)),
      nCounterClockwise = actionCount(config.actions.indexOf(Action.RotateMinus)),
      maxLife = maxLifeSpan,
      maxChildren = maxChildren,
      maxTravelDistance = maxDistance,
      maxSpeed = maxSpeed,
      nMonsters = nMonsters,
    )
  
  def initialize(config: Config): Unit =
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = getCell(x, y)
        cell.setDebris(0)
        cell.setFood(1e-9)
        if config.random.nextDouble() < config.initialBacteriaProbability
        then cell.setIndividual(Individual(IArray.tabulate(config.initialGenomeLength)(i => Instruction.random(config.random, i)), 0),
          config.random.nextInt(4),
          config.initialHealth)
        else cell.setIndividual(null, 0, 0)
  
  
  def simulationStep(config: Config, stepNumber: Int): Field.StepStatistics =
    val actionCount = performActionsOnIndividuals(config)
    drainIdleEnergy(config)
    depositFoodAndConvertDebris(config, stepNumber)
    computeStatistics(config, actionCount)

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
  
  case class StepStatistics(maxGenomeSize: Int, averageGenomeSize: Double, numberOfBacteria: Int,
                            averageHealth: Double, maximalHealth: Double, totalFood: Double, maxFood: Double,
                            nEats: Int, nForks: Int, nMoves: Int, nClockwise: Int, nCounterClockwise: Int,
                            maxLife: Int, maxChildren: Int, maxTravelDistance: Int, maxSpeed: Double, nMonsters: Int)
