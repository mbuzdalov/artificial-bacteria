package alife

import java.util.concurrent.ThreadLocalRandom

/**
 * A trait for all instructions.
 */
sealed trait Instruction {
  def apply(field: Field, x: Int, y: Int, data: Instruction.DA): Double
}

/**
 * All the available instructions.
 */
object Instruction {
  type DA = Int => Double

  case class Const(value: Double) extends Instruction {
    override def apply(field: Field, x: Int, y: Int, data: DA): Double = value
  }
  object MyWeight extends Instruction {
    override def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getWeight(x, y)
  }
  object MyHealth extends Instruction {
    override def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getHealth(x, y)
  }

  abstract class RelativeQuery(relativeLocation: Int, query: Field => (Int, Int, Int) => Double) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = query(field)(x, y, relativeLocation)
  }

  case class EnergyAt(relativeLocation: Int) extends RelativeQuery(relativeLocation, _.getEnergyRelative)
  case class DebrisAt(relativeLocation: Int) extends RelativeQuery(relativeLocation, _.getDebrisRelative)
  case class HealthAt(relativeLocation: Int) extends RelativeQuery(relativeLocation, _.getHealthRelative)

  abstract class UnaryOp(arg: Int, fun: Double => Double) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = fun(data(arg))
  }

  case class Sin(arg: Int) extends UnaryOp(arg, math.sin)
  case class Cos(arg: Int) extends UnaryOp(arg, math.cos)
  case class Exp(arg: Int) extends UnaryOp(arg, math.exp)
  case class Log(arg: Int) extends UnaryOp(arg, math.log)

  abstract class BinaryOp(arg1: Int, arg2: Int, fun: (Double, Double) => Double) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = fun(data(arg1), data(arg2))
  }

  case class Plus(arg1: Int, arg2: Int)    extends BinaryOp(arg1, arg2, _ + _)
  case class Minus(arg1: Int, arg2: Int)   extends BinaryOp(arg1, arg2, _ - _)
  case class Times(arg1: Int, arg2: Int)   extends BinaryOp(arg1, arg2, _ * _)
  case class Divide(arg1: Int, arg2: Int)  extends BinaryOp(arg1, arg2, _ / _)
  case class Sigmoid(arg1: Int, arg2: Int) extends BinaryOp(arg1, arg2, (l, r) => 2 / (1 + math.exp(l - r)) - 1)

  def random(position: Int): Instruction = {
    val rng = ThreadLocalRandom.current()
    def nextPos = rng.nextInt(position + 1) - 1
    rng.nextInt(15) match {
      case 0 => Const(rng.nextDouble())
      case 1 => MyWeight
      case 2 => MyHealth
      case 3 => EnergyAt(rng.nextInt(Field.numberOfRelativeLocations))
      case 4 => DebrisAt(rng.nextInt(Field.numberOfRelativeLocations))
      case 5 => HealthAt(rng.nextInt(Field.numberOfRelativeLocations))
      case 6 => Sin(nextPos)
      case 7 => Cos(nextPos)
      case 8 => Exp(nextPos)
      case 9 => Log(nextPos)
      case 10 => Plus(nextPos, nextPos)
      case 11 => Minus(nextPos, nextPos)
      case 12 => Times(nextPos, nextPos)
      case 13 => Divide(nextPos, nextPos)
      case 14 => Sigmoid(nextPos, nextPos)
      case _ => throw new AssertionError()
    }
  }
}
