package alife

import alife.Field.Constants

/**
 * A trait for all actions.
 */
sealed trait Action:
  def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean
  def apply(field: Field, x: Int, y: Int, constants: Constants): Unit
  def index: Int

/**
 * All the available actions.
 */
object Action:
  final val all: IArray[Action] = IArray(Fork, RotateMinus, RotatePlus, Move, Eat)
  all.indices.foreach(i => assert(all(i).index == i))

  abstract class Rotate(rotation: Int, val index: Int) extends Action:
    private def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double =
      constants.rotationCost * (field.getHealth(x, y) + field.getWeight(x, y))
    
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = true
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      field.setIndividual(x, y, field.getIndividual(x, y), (field.getDirection(x, y) + rotation) & 3, field.getHealth(x, y) - e)
      field.setDebris(x, y, field.getDebris(x, y) + e / 2)
  
  case object RotateMinus extends Rotate(-1, 1)
  case object RotatePlus  extends Rotate(+1, 2)

  case object Move extends Action:
    override def index: Int = 3
    private def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double =
      constants.moveCost * (field.getHealth(x, y) + field.getWeight(x, y) + field.getDebris(x, y))
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean =
      field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y)
      val d = field.getDirection(x, y)
      field.setIndividualRelative(x, y, Field.relativeLocationForward, g, d, h - e)
      field.setIndividual(x, y, null, 0, 0)
      field.setDebris(x, y, field.getDebris(x, y) + e / 2)

  case object Eat extends Action:
    override def index: Int = 4
    private def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double = 0.0
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = true
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val w = field.getWeight(x, y)
      // how much can we eat before hitting our global limit
      val intakeLimitGlobal = w * constants.healthMultiple - field.getHealth(x, y)
      // how much can we eat technically: min of current food and of the max increment
      val intakeLimitLocal = math.min(w * constants.healthIncrementMultiple, field.getEnergy(x, y))
      val eatAmount = math.max(0, math.min(intakeLimitGlobal, intakeLimitLocal))
      field.setEnergy(x, y, field.getEnergy(x, y) - eatAmount)
      field.setIndividual(x, y, field.getIndividual(x, y), field.getDirection(x, y), field.getHealth(x, y) + eatAmount * (1 - constants.eatCost))

  case object Fork extends Action:
    override def index: Int = 0
    private def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double =
      constants.forkCost * field.getWeight(x, y)
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean =
      field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      assert(Move.canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y) - e
      val d = field.getDirection(x, y)
      if h / 2 > 0 then
        field.setIndividual(x, y, Operators.mutate(g), d, h / 2)
        Move.apply(field, x, y, constants)
      field.setIndividual(x, y, g, d, h / 2)
