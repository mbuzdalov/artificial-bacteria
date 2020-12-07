package alife

import java.util.{Arrays => JArrays}
import java.util.concurrent.ThreadLocalRandom

/**
 * A class for a field where bacteria live.
 */
class Field(val width: Int, val height: Int) {
  type Genome = Seq[Instruction]
  import alife.Field.{Constants, DataAccess}

  private val energy = new Field.Matrix[Double](width, height)
  private val debris = new Field.Matrix[Double](width, height)
  private val health = new Field.Matrix[Double](width, height)
  private val genome = new Field.Matrix[Genome](width, height)
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
  def getGenome(x: Int, y: Int): Genome = genome(x, y)
  def getWeight(x: Int, y: Int): Double = {
    val g = getGenome(x, y)
    if (g == null) 0 else g.size
  }

  def setEnergy(x: Int, y: Int, value: Double): Unit = energy(x, y) = value
  def setDebris(x: Int, y: Int, value: Double): Unit = debris(x, y) = value
  def setGenome(x: Int, y: Int, g: Genome, d: Int, h: Double): Unit = {
    if (genome(x, y) != null) {
      numberOfBacteria -= 1
    }
    if (h < 0 || g == null) {
      genome(x, y) = null
      direction(x, y) = 0
      health(x, y) = 0
    } else {
      genome(x, y) = g
      direction(x, y) = d
      numberOfBacteria += 1
      maxGenomeSize = math.max(maxGenomeSize, g.size)
      health(x, y) = h
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
  final def setGenomeRelative(x: Int, y: Int, relativeLocation: Int, g: Genome, d: Int, h: Double): Unit = {
    atRelative(x, y, relativeLocation, (i, j) => { setGenome(i, j, g, d, h); 0.0 })
  }

  def simulationStep(constants: Constants, stepNumber: Int): Field.StepStatistics = {
    val actionCount = Array.ofDim[Int](Action.all.size)
    var da = new DataAccess(Array.ofDim((maxGenomeSize + 5) * 2))

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val g = genome(x, y)
        if (g != null) {
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
    maxGenomeSize = 0

    val synthDecay = math.exp(-stepNumber * constants.synthesisDecay)
    val synthesisBase = 2 * (synthDecay * constants.synthesisInit + (1 - synthDecay) * constants.synthesisFinal)

    y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val cosx = math.cos(x * math.Pi * constants.spotPeriodX / width + 2 * math.Pi * stepNumber * constants.spotSpeedX)
        val siny = math.sin(y * math.Pi * constants.spotPeriodY / height + 2 * math.Pi * stepNumber * constants.spotSpeedY)
        val decay = math.exp(-stepNumber * constants.spotDecay) // initially 1, then decreases to 0
        val synthesis = synthesisBase * ((1 - decay) * cosx * cosx * siny * siny + decay)

        energy(x, y) += debris(x, y) * constants.debrisToEnergy
        debris(x, y) *= (1 - constants.debrisDegradation)
        energy(x, y) += ThreadLocalRandom.current().nextDouble() * synthesis
        sumEnergies += energy(x, y)
        if (genome(x, y) != null) {
          setGenome(x, y, genome(x, y), direction(x, y), health(x, y) - constants.idleCost)
          sumHealths += health(x, y)
          maxHealth = math.max(maxHealth, health(x, y))
          if (genome(x, y) != null) {
            maxGenomeSize = math.max(maxGenomeSize, genome(x, y).size)
          }
        }
        x += 1
      }
      y += 1
    }
    val resultMap = actionCount.zipWithIndex.map(p => Action.all(p._2).toString -> p._1.toDouble).toMap
    resultMap + ("AvgHealth" -> sumHealths / math.max(1, getNumberOfBacteria)) + ("TotalEnergy" -> sumEnergies)

    Field.StepStatistics(
      averageHealth = sumHealths / math.max(1, getNumberOfBacteria),
      maximalHealth = maxHealth,
      totalEnergy = sumEnergies,
      nEats = actionCount(Action.all.indexOf(Action.Eat)),
      nForks = actionCount(Action.all.indexOf(Action.Fork)),
      nMoves = actionCount(Action.all.indexOf(Action.Move)),
      nClockwise = actionCount(Action.all.indexOf(Action.RotateMinus)),
      nCounterClockwise = actionCount(Action.all.indexOf(Action.RotatePlus))
    )
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

  case class StepStatistics(averageHealth: Double, maximalHealth: Double, totalEnergy: Double,
                            nEats: Int, nForks: Int, nMoves: Int, nClockwise: Int, nCounterClockwise: Int)

  case class Constants(rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                       debrisDegradation: Double, debrisToEnergy: Double,
                       synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                       idleCost: Double, healthMultiple: Double,
                       spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double,
                       spotDecay: Double)
}
