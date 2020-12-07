package alife

import java.util.{Arrays => JArrays}
import java.util.concurrent.ThreadLocalRandom

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int) {
  import alife.Field.{Constants, DataAccess}

  private val energy = new Field.Matrix[Double](width, height)
  private val debris = new Field.Matrix[Double](width, height)
  private val health = new Field.Matrix[Double](width, height)
  private val individual = new Field.Matrix[Individual](width, height)
  private val direction = new Field.Matrix[Int](width, height)
  private val action = new Field.Matrix[Int](width, height)

  private var maxGenomeSize = 0
  private var numberOfBacteria = 0

  def getMaxGenomeSize: Int = maxGenomeSize
  def getNumberOfBacteria: Int = numberOfBacteria

  def getEnergy(x: Int, y: Int): Double = energy(x, y)
  def getDebris(x: Int, y: Int): Double = debris(x, y)
  def getHealth(x: Int, y: Int): Double = health(x, y)
  def getDirection(x: Int, y: Int): Int = direction(x, y)
  def getIndividual(x: Int, y: Int): Individual = individual(x, y)
  def getWeight(x: Int, y: Int): Double = {
    val g = getIndividual(x, y)
    if (g == null) 0 else g.genome.size
  }

  def setEnergy(x: Int, y: Int, value: Double): Unit = energy(x, y) = value
  def setDebris(x: Int, y: Int, value: Double): Unit = debris(x, y) = value
  def setIndividual(x: Int, y: Int, g: Individual, d: Int, h: Double): Unit = {
    if (individual(x, y) != null) {
      numberOfBacteria -= 1
    }
    if (h < 0 || g == null) {
      individual(x, y) = null
      direction(x, y) = 0
      health(x, y) = 0
    } else {
      individual(x, y) = g
      direction(x, y) = d
      numberOfBacteria += 1
      maxGenomeSize = math.max(maxGenomeSize, g.genome.size)
      health(x, y) = h
    }
  }

  def increaseEnergy(x: Int, y: Int, radius: Int, amount: Double): Unit = {
    for (xi <- -radius to radius; yi <- -radius to radius) {
      if (xi * xi + yi * yi <= radius * radius) {
        val oldEnergy = getEnergy(x + xi, y + yi)
        setEnergy(x + xi, y + yi, oldEnergy + (amount * 1.05 - oldEnergy) * 0.5)
      }
    }
  }

  def eraseEverything(x: Int, y: Int, radius: Int): Unit = {
    for (xi <- -radius to radius; yi <- -radius to radius) {
      if (xi * xi + yi * yi <= radius * radius) {
        setEnergy(x + xi, y + yi, 0)
        setDebris(x + xi, y + yi, 0)
        setIndividual(x + xi, y + yi, null, 0, 0)
      }
    }
  }

  def rotate(x: Int, y: Int, value: Int): Unit = direction(x, y) = (direction(x, y) + value) & 3

  private[this] trait IntIntDoubleFun {
    def apply(a: Int, b: Int): Double
  }

  @inline
  private[this] def atRelative(x: Int, y: Int, relativeLocation: Int, fun: IntIntDoubleFun): Double = {
    val dirFW = direction(x, y)
    val dirLF = (dirFW + 1) & 3
    val scaleFW = Field.relativeLocationsFW(relativeLocation)
    val scaleLF = Field.relativeLocationsLF(relativeLocation)
    val realX = x + Field.directionX(dirFW) * scaleFW + Field.directionX(dirLF) * scaleLF
    val realY = y + Field.directionY(dirFW) * scaleFW + Field.directionY(dirLF) * scaleLF
    fun(realX, realY)
  }

  final def getEnergyRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getEnergy)
  final def getDebrisRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getDebris)
  final def getHealthRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getHealth)
  final def setIndividualRelative(x: Int, y: Int, relativeLocation: Int, g: Individual, d: Int, h: Double): Unit = {
    atRelative(x, y, relativeLocation, (i, j) => { setIndividual(i, j, g, d, h); 0.0 })
  }

  def simulationStep(constants: Constants, stepNumber: Int): Field.StepStatistics = {
    val actionCount = Array.ofDim[Int](Action.all.size)
    var da = new DataAccess(Array.ofDim((maxGenomeSize + 5) * 2))

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val ind = individual(x, y)
        if (ind != null) {
          val g = ind.genome
          if (da.array.length < maxGenomeSize) {
            da = new DataAccess(Array.ofDim(maxGenomeSize * 2))
          }
          da.clear()
          var i = 0
          while (i < g.size) {
            da.array(i) = g(i).apply(this, x, y, da)
            da.offset += 1
            i += 1
          }
          action(x, y) = -1
          i = 0
          val iMax = math.min(g.size, Action.all.size)
          while (i < iMax) {
            if (Action.all(i).canApply(this, x, y, constants) && (action(x, y) == -1 || da(i + 1) > da(action(x, y)))) {
              action(x, y) = i
            }
            i += 1
          }
          if (action(x, y) != -1) {
            val theAction = Action.all(action(x, y))
            ind.recordAction(theAction)
            theAction.apply(this, x, y, constants)
            actionCount(action(x, y)) += 1
          }
        }
        x += 1
      }
      y += 1
    }

    var maxHealth = 0.0
    var sumHealths = 0.0
    var sumEnergies = 0.0
    var maxEnergy = 0.0
    maxGenomeSize = 0
    var nMonsters = 0

    var maxLifeSpan = 0
    var maxChildren = 0
    var maxDistance = 0
    var maxSpeed = 0.0

    val synthDecay = math.exp(-stepNumber * constants.synthesisDecay)
    val synthesisBase = 2 * (synthDecay * constants.synthesisInit + (1 - synthDecay) * constants.synthesisFinal)

    y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val cosX = math.cos(x * math.Pi * constants.spotPeriodX / width + 2 * math.Pi * stepNumber * constants.spotSpeedX)
        val sinY = math.sin(y * math.Pi * constants.spotPeriodY / height + 2 * math.Pi * stepNumber * constants.spotSpeedY)
        val decay = math.exp(-stepNumber * constants.spotDecay) // initially 1, then decreases to 0
        val synthesis = synthesisBase * ((1 - decay) * cosX * cosX * sinY * sinY + decay)

        energy(x, y) += debris(x, y) * constants.debrisToEnergy
        debris(x, y) *= (1 - constants.debrisDegradation)
        energy(x, y) += ThreadLocalRandom.current().nextDouble() * synthesis
        sumEnergies += energy(x, y)
        maxEnergy = math.max(maxEnergy, energy(x, y))
        if (individual(x, y) != null) {
          setIndividual(x, y, individual(x, y), direction(x, y), health(x, y) - constants.idleCost)
          sumHealths += health(x, y)
          maxHealth = math.max(maxHealth, health(x, y))
          val ind = individual(x, y)
          if (ind != null) {
            if (ind.label < 0) nMonsters += 1
            maxGenomeSize = math.max(maxGenomeSize, ind.genome.size)
            maxLifeSpan = math.max(maxLifeSpan, ind.lifeSpan)
            maxChildren = math.max(maxChildren, ind.numberOfChildren)
            maxDistance = math.max(maxDistance, ind.travelDistance)
            maxSpeed = math.max(maxSpeed, ind.averageSpeed)
          }
        }
        x += 1
      }
      y += 1
    }

    Field.StepStatistics(
      averageHealth = sumHealths / math.max(1, getNumberOfBacteria),
      maximalHealth = maxHealth,
      totalEnergy = sumEnergies,
      maxEnergy = maxEnergy,
      nEats = actionCount(Action.all.indexOf(Action.Eat)),
      nForks = actionCount(Action.all.indexOf(Action.Fork)),
      nMoves = actionCount(Action.all.indexOf(Action.Move)),
      nClockwise = actionCount(Action.all.indexOf(Action.RotateMinus)),
      nCounterClockwise = actionCount(Action.all.indexOf(Action.RotatePlus)),
      maxLife = maxLifeSpan,
      maxChildren = maxChildren,
      maxTravelDistance = maxDistance,
      maxSpeed = maxSpeed,
      nMonsters = nMonsters
    )
  }

  def findAndMarkLongestGenome(label: Int): Unit =
    forEachIndividual((x, y, ind) => if (ind.genome.size == maxGenomeSize) individual(x, y) = ind.copy(label = label))

  def findAndMarkMostProductive(label: Int): Unit = labelMaxIndividual(_.numberOfChildren, 0, label)
  def findAndMarkFastest(label: Int): Unit = labelMaxIndividual(_.averageSpeed, 0.0, label)

  private[this] def labelMaxIndividual[T: Ordering](fun: Individual => T, defVal: T, label: Int): Unit = {
    var t = defVal
    forEachIndividual((_, _, ind) => t = implicitly[Ordering[T]].max(t, fun(ind)))
    forEachIndividual((x, y, ind) => if (implicitly[Ordering[T]].equiv(t, fun(ind))) individual(x, y) = ind.copy(label = label))
  }

  private[this] def forEachIndividual(fun: (Int, Int, Individual) => Unit): Unit = {
    for (y <- 0 until height; x <- 0 until width) {
      val ind = individual(x, y)
      if (ind != null) fun(x, y, ind)
    }
  }
}

object Field {
  private val directionX = Array(1, 0, -1, 0)
  private val directionY = Array(0, 1, 0, -1)
  private val relativeLocationsFW = Array(0, 1,  0, 0,  1, 1,  0, 0, 2)
  private val relativeLocationsLF = Array(0, 0, -1, 1, -1, 1, -2, 2, 0)

  val relativeLocationForward = 1
  val numberOfRelativeLocations: Int = relativeLocationsFW.length

  class DataAccess(val array: Array[Double]) extends (Int => Double) {
    var offset = 0
    def apply(index: Int): Double = {
      val idx = offset - index
      if (idx < 0 || index <= 0) 0.0 else array(idx)
    }
    def clear(): Unit = {
      offset = 0
      JArrays.fill(array, 0.0)
    }
  }

  class Matrix[@specialized(Double, Int) T : scala.reflect.ClassTag](width: Int, height: Int) {
    val data: Array[Array[T]] = Array.ofDim(height, width)
    def apply(x: Int, y: Int): T = data(mod(y, height))(mod(x, width))
    def update(x: Int, y: Int, value: T): Unit = data(mod(y, height))(mod(x, width)) = value

    @inline private def mod(i: Int, n: Int) = if (i >= 0 && i < n) i else (i % n + n) % n
  }

  case class StepStatistics(averageHealth: Double, maximalHealth: Double, totalEnergy: Double, maxEnergy: Double,
                            nEats: Int, nForks: Int, nMoves: Int, nClockwise: Int, nCounterClockwise: Int,
                            maxLife: Int, maxChildren: Int, maxTravelDistance: Int, maxSpeed: Double, nMonsters: Int)

  case class Constants(rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                       debrisDegradation: Double, debrisToEnergy: Double,
                       synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                       idleCost: Double, healthMultiple: Double,
                       spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double,
                       spotDecay: Double)
}
