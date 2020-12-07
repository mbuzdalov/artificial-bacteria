package alife

import alife.Field.Constants

/**
 * A trait for all actions.
 */
sealed trait Action {
  protected def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double
  def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = true
  def apply(field: Field, x: Int, y: Int, constants: Constants): Unit
}

/**
 * All the available actions.
 */
object Action {
  final val all: Seq[Action] = Seq(Fork, RotateMinus, RotatePlus, Move, Eat)

  abstract class Rotate(rotation: Int) extends Action {
    override protected def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double = {
      constants.rotationCost * (field.getHealth(x, y) + field.getWeight(x, y))
    }
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit = {
      assert(canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      field.setIndividual(x, y, field.getIndividual(x, y), (field.getDirection(x, y) + rotation) & 3, field.getHealth(x, y) - e)
      field.setDebris(x, y, field.getDebris(x, y) + e / 2)
    }
  }

  case object RotatePlus extends Rotate(1)
  case object RotateMinus extends Rotate(-1)

  case object Move extends Action {
    override protected def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double = {
      constants.moveCost * (field.getHealth(x, y) + field.getWeight(x, y) + field.getDebris(x, y))
    }
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = {
      super.canApply(field, x, y, constants) && field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    }
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit = {
      assert(canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y)
      val d = field.getDirection(x, y)
      field.setIndividualRelative(x, y, Field.relativeLocationForward, g, d, h - e)
      field.setIndividual(x, y, null, 0, 0)
      field.setDebris(x, y, field.getDebris(x, y) + e / 2)
    }
  }

  case object Eat extends Action {
    override protected def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double = 0.0
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit = {
      assert(canApply(field, x, y, constants))
      val w = field.getWeight(x, y)
      val eatAmount = math.max(0, math.min(math.min(field.getEnergy(x, y), w), w * constants.healthMultiple - field.getEnergy(x, y)))
      field.setEnergy(x, y, field.getEnergy(x, y) - eatAmount)
      field.setIndividual(x, y, field.getIndividual(x, y), field.getDirection(x, y), field.getHealth(x, y) + eatAmount * (1 - constants.eatCost))
    }
  }

  case object Fork extends Action {
    override protected def requiredEnergy(field: Field, x: Int, y: Int, constants: Constants): Double = {
      constants.forkCost * field.getWeight(x, y)
    }
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = {
      super.canApply(field, x, y, constants) && field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    }
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit = {
      assert(canApply(field, x, y, constants))
      assert(Move.canApply(field, x, y, constants))
      val e = requiredEnergy(field, x, y, constants)
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y) - e
      val d = field.getDirection(x, y)
      if (h / 2 > 0) {
        field.setIndividual(x, y, Operators.mutate(g), d, h / 2)
        Move.apply(field, x, y, constants)
      }
      field.setIndividual(x, y, g, d, h / 2)
    }
  }
}
