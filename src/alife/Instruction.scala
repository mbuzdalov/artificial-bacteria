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

  case object MyWeight extends Instruction {
    override def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getWeight(x, y)
  }

  case object MyHealth extends Instruction {
    override def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getHealth(x, y)
  }

  case class EnergyAt(relativeLocation: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getEnergyRelative(x, y, relativeLocation)
  }

  case class DebrisAt(relativeLocation: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getDebrisRelative(x, y, relativeLocation)
  }

  case class HealthAt(relativeLocation: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = field.getHealthRelative(x, y, relativeLocation)
  }

  case class Sin(arg: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = math.sin(data(arg))
  }

  case class Cos(arg: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = math.cos(data(arg))
  }

  case class Exp(arg: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = math.exp(data(arg))
  }

  case class Log(arg: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = math.log(data(arg))
  }

  case class Plus(arg1: Int, arg2: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = data(arg1) + data(arg2)
  }

  case class Minus(arg1: Int, arg2: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = data(arg1) - data(arg2)
  }

  case class Times(arg1: Int, arg2: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = data(arg1) * data(arg2)
  }

  case class Divide(arg1: Int, arg2: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = data(arg1) / data(arg2)
  }

  case class Sigmoid(arg1: Int, arg2: Int) extends Instruction {
    override final def apply(field: Field, x: Int, y: Int, data: DA): Double = 2 / (1 + math.exp(data(arg1) - data(arg2))) - 1
  }

  def random(position: Int): Instruction = {
    val rng = ThreadLocalRandom.current()

    def nextPos() = rng.nextInt(position + 1) - 1
    def nextLoc() = rng.nextInt(Field.numberOfRelativeLocations)

    rng.nextInt(15) match {
      case 0 => Const(rng.nextDouble())
      case 1 => MyWeight
      case 2 => MyHealth
      case 3 => EnergyAt(nextLoc())
      case 4 => DebrisAt(nextLoc())
      case 5 => HealthAt(nextLoc())
      case 6 => Sin(nextPos())
      case 7 => Cos(nextPos())
      case 8 => Exp(nextPos())
      case 9 => Log(nextPos())
      case 10 => Plus(nextPos(), nextPos())
      case 11 => Minus(nextPos(), nextPos())
      case 12 => Times(nextPos(), nextPos())
      case 13 => Divide(nextPos(), nextPos())
      case 14 => Sigmoid(nextPos(), nextPos())
      case _ => throw new AssertionError()
    }
  }
}
