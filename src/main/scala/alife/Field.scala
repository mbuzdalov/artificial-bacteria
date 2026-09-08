package alife

import alife.util.{Loops, PseudoStack}

import java.util.concurrent.ThreadLocalRandom

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int):
  import alife.Field.Constants

  private val energy = Field.Matrix[Double](width, height)
  private val debris = Field.Matrix[Double](width, height)
  private val health = Field.Matrix[Double](width, height)
  private val individual = Field.Matrix[Individual](width, height)
  private val direction = Field.Matrix[Int](width, height)
  private val sumDistancesL, sumDistancesR = Array.ofDim[Int](height)

  private var numberOfBacteria = 0
  private val callStack = PseudoStack()

  def getEnergy(x: Int, y: Int): Double = energy.checked(x, y)
  def getDebris(x: Int, y: Int): Double = debris.checked(x, y)
  def getHealth(x: Int, y: Int): Double = health.checked(x, y)
  def getDirection(x: Int, y: Int): Int = direction.checked(x, y)
  def getIndividual(x: Int, y: Int): Individual = individual.checked(x, y)
  def getSumOfDistancesFromLeft(y: Int): Int = sumDistancesL(y)
  def getSumOfDistancesFromRight(y: Int): Int = sumDistancesR(y)
  def getWeight(x: Int, y: Int): Double =
    val g = getIndividual(x, y)
    if (g == null) 0 else g.genome.size

  def setEnergy(x: Int, y: Int, value: Double): Unit = energy.setChecked(x, y, value)
  def setDebris(x: Int, y: Int, value: Double): Unit = debris.setChecked(x, y, value)
  def setIndividual(x: Int, y: Int, g: Individual, d: Int, h: Double): Unit =
    val xm = (width + x % width) % width
    val ym = (height + y % height) % height
    if individual(xm, ym) != null then
      numberOfBacteria -= 1
      sumDistancesL(ym) -= xm
      sumDistancesR(ym) -= width - 1 - xm
    if h < 0 || g == null then
      individual(xm, ym) = null
      direction(xm, ym) = 0
      health(xm, ym) = 0
    else
      individual(xm, ym) = g
      direction(xm, ym) = d
      numberOfBacteria += 1
      health(xm, ym) = h
      sumDistancesL(ym) += xm
      sumDistancesR(ym) += width - 1 - xm
    end if

  def increaseEnergy(x: Int, y: Int, radius: Int, amount: Double): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val oldEnergy = getEnergy(x + xi, y + yi)
          setEnergy(x + xi, y + yi, oldEnergy + (amount * 1.05 - oldEnergy) * 0.5)

  def eraseEverything(x: Int, y: Int, radius: Int): Unit =
    Loops.foreachInclusive(-radius, radius): xi =>
      Loops.foreachInclusive(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          setEnergy(x + xi, y + yi, 0)
          setDebris(x + xi, y + yi, 0)
          setIndividual(x + xi, y + yi, null, 0, 0)
  
  private inline def atRelative(x: Int, y: Int, relativeLocation: Int, inline fun: (Int, Int) => Double): Double =
    val dirFW = direction.checked(x, y)
    val dirLF = (dirFW + 1) & 3
    val scaleFW = Field.relativeLocationsFW(relativeLocation)
    val scaleLF = Field.relativeLocationsLF(relativeLocation)
    val realX = x + Field.directionX(dirFW) * scaleFW + Field.directionX(dirLF) * scaleLF
    val realY = y + Field.directionY(dirFW) * scaleFW + Field.directionY(dirLF) * scaleLF
    fun(realX, realY)

  final def getEnergyRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getEnergy)
  final def getDebrisRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getDebris)
  final def getHealthRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getHealth)
  final def setIndividualRelative(x: Int, y: Int, relativeLocation: Int, g: Individual, d: Int, h: Double): Unit =
    atRelative(x, y, relativeLocation, (i, j) => { setIndividual(i, j, g, d, h); 0.0 })

  private def performActionsOnIndividuals(constants: Constants): Array[Int] =
    val actionCount = Array.ofDim[Int](Action.all.size)
    
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val ind = individual(x, y)
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
  
  private def drainIdleEnergy(constants: Constants): Unit =
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        if individual(x, y) != null then
          val spentForLiving = math.min(constants.idleCost, health(x, y))
          debris(x, y) += spentForLiving * constants.debrisFromActions
          setIndividual(x, y, individual(x, y), direction(x, y), health(x, y) - constants.idleCost)

  private def depositFoodAndConvertDebris(constants: Constants, stepNumber: Int): Unit =
    val synthDecay = math.exp(-stepNumber * constants.synthesisDecay) // initially 1, then decreases to 0
    val sineDecay = math.exp(-stepNumber * constants.spotDecay) // initially 1, then decreases to 0
    val synthesisBase = 2 * (synthDecay * constants.synthesisInit + (1 - synthDecay) * constants.synthesisFinal)
    
    val spotXOffset = 2 * math.Pi * stepNumber * constants.spotSpeedX
    val spotYOffset = 2 * math.Pi * stepNumber * constants.spotSpeedY
    val spotXScale = math.Pi * constants.spotPeriodX / width
    val spotYScale = math.Pi * constants.spotPeriodY / height
    
    Loops.foreach(0, height): y =>
      val sinY = math.sin(y * spotYScale + spotYOffset)
      Loops.foreach(0, width): x =>
        val cosX = math.cos(x * spotXScale + spotXOffset)
        val newFoodScale = synthesisBase * ((1 - sineDecay) * cosX * cosX * sinY * sinY + sineDecay)
        val newFood = newFoodScale * ThreadLocalRandom.current().nextDouble()
        val d2e = debris(x, y) * constants.debrisToEnergy
        debris(x, y) *= 1 - constants.debrisDegradation - constants.debrisToEnergy
        energy(x, y) += d2e + newFood
  
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
        sumEnergies += energy(x, y)
        maxEnergy = math.max(maxEnergy, energy(x, y))
        val ind = individual(x, y)
        if ind != null then
          nBacteria += 1
          sumHealths += health(x, y)
          maxHealth = math.max(maxHealth, health(x, y))
          if ind.label < 0 then nMonsters += 1
          maxGenomeSize = math.max(maxGenomeSize, ind.genome.size)
          sumGenomeSizes += ind.genome.size
          maxLifeSpan = math.max(maxLifeSpan, ind.lifeSpan)
          maxChildren = math.max(maxChildren, ind.numberOfChildren)
          maxDistance = math.max(maxDistance, ind.travelDistance)
          maxSpeed = math.max(maxSpeed, ind.averageSpeed)
    
    assert(nBacteria == numberOfBacteria)
    
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
  
  def simulationStep(constants: Constants, stepNumber: Int): Field.StepStatistics =
    val actionCount = performActionsOnIndividuals(constants)
    drainIdleEnergy(constants)
    depositFoodAndConvertDebris(constants, stepNumber)
    computeStatistics(actionCount)

  def findAndMarkLongestGenome(label: Int): Unit = labelMaxIndividual(_.genome.size, 0, label)
  def findAndMarkMostProductive(label: Int): Unit = labelMaxIndividual(_.numberOfChildren, 0, label)
  def findAndMarkFastest(label: Int): Unit = labelMaxIndividual(_.averageSpeed, 0.0, label)

  private def labelMaxIndividual[T: Ordering as o](fun: Individual => T, defVal: T, label: Int): Unit =
    var t = defVal
    forEachIndividual((_, _, ind) => t = o.max(t, fun(ind)))
    forEachIndividual((x, y, ind) => if o.equiv(t, fun(ind)) then individual(x, y) = ind.copy(label = label))

  private inline def forEachIndividual(inline fun: (Int, Int, Individual) => Unit): Unit =
    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val ind = individual(x, y)
        if ind != null then fun(x, y, ind)
end Field

object Field:
  private val directionX = Array(1, 0, -1, 0)
  private val directionY = Array(0, 1, 0, -1)
  private val relativeLocationsFW = Array(0, 1,  0, 0,  1, 1,  0, 0, 2)
  private val relativeLocationsLF = Array(0, 0, -1, 1, -1, 1, -2, 2, 0)

  val relativeLocationForward = 1
  val numberOfRelativeLocations: Int = relativeLocationsFW.length

  //noinspection ScalaUnusedSymbol this is for ClassTag, actually used in Array.ofDim.
  private class Matrix[@specialized(Double, Int) T : scala.reflect.ClassTag](width: Int, height: Int):
    private val data: Array[Array[T]] = Array.ofDim(height, width)
    def apply(x: Int, y: Int): T = data(y)(x)
    def checked(x: Int, y: Int): T = data(mod(y, height))(mod(x, width))
    def update(x: Int, y: Int, value: T): Unit = data(y)(x) = value
    def setChecked(x: Int, y: Int, value: T): Unit = data(mod(y, height))(mod(x, width)) = value
    private inline def mod(i: Int, n: Int) = if (i >= 0 && i < n) i else (i % n + n) % n

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
