package alife

import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable

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

  @inline
  private def atRelative(x: Int, y: Int, relativeLocation: Int, fun: (Int, Int) => Double) = {
    val dirFW = direction(x, y)
    val dirLF = (dirFW + 1) & 3
    val scaleFW = Field.relativeLocationsFW(relativeLocation)
    val scaleLF = Field.relativeLocationsLF(relativeLocation)
    val realX = x + Field.directionX(dirFW) * scaleFW + Field.directionX(dirLF) * scaleLF
    val realY = y + Field.directionY(dirFW) * scaleFW + Field.directionY(dirLF) * scaleLF
    fun(realX, realY)
  }

  @inline
  final def getEnergyRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getEnergy)
  @inline
  final def getDebrisRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getDebris)
  @inline
  final def getHealthRelative(x: Int, y: Int, relativeLocation: Int): Double = atRelative(x, y, relativeLocation, getHealth)
  @inline
  final def setGenomeRelative(x: Int, y: Int, relativeLocation: Int, g: Genome, d: Int, h: Double): Unit = {
    atRelative(x, y, relativeLocation, (i, j) => { setGenome(i, j, g, d, h); 0.0 })
  }

  def simulationStep(constants: Constants): Map[String, Double] = {
    val actionCount = new mutable.HashMap[String, Double]
    for (a <- Action.all) actionCount += a.toString -> 0.0
    var x = 0
    while (x < width) {
      var y = 0
      while (y < height) {
        val g = genome(x, y)
        if (g != null) {
          val da = new DataAccess(Array.ofDim(maxGenomeSize))
          da.offset = 0
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
            val key = theAction.toString
            actionCount.update(key, actionCount(key) + 1)
          }
        }
        y += 1
      }
      x += 1
    }

    var sumHealths = 0.0
    var sumEnergies = 0.0
    maxGenomeSize = 0

    x = 0
    while (x < width) {
      var y = 0
      while (y < height) {
        energy(x, y) += debris(x, y) * constants.debrisToEnergy
        debris(x, y) *= (1 - constants.debrisDegradation)
        energy(x, y) += ThreadLocalRandom.current().nextDouble() * 2 * constants.synthesis
        sumEnergies += energy(x, y)
        if (genome(x, y) != null) {
          setGenome(x, y, genome(x, y), direction(x, y), health(x, y) - constants.idleCost)
          sumHealths += health(x, y)
          if (genome(x, y) != null) {
            maxGenomeSize = math.max(maxGenomeSize, genome(x, y).size)
          }
        }
        y += 1
      }
      x += 1
    }
    actionCount.toMap + ("AvgHealth" -> sumHealths / math.max(1, getNumberOfBacteria)) + ("TotalEnergy" -> sumEnergies)
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
  }

  class Matrix[@specialized(Double, Int) T : scala.reflect.ClassTag](width: Int, height: Int) {
    val data: Array[Array[T]] = Array.ofDim(width, height)
    def apply(x: Int, y: Int): T = data(mod(x, width))(mod(y, height))
    def update(x: Int, y: Int, value: T): Unit = data(mod(x, width))(mod(y, height)) = value

    @inline private def mod(i: Int, n: Int) = if (i >= 0 && i < n) i else (i % n + n) % n
  }

  case class Constants(rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                       debrisDegradation: Double, debrisToEnergy: Double, synthesis: Double,
                       idleCost: Double, healthMultiple: Double)
}
