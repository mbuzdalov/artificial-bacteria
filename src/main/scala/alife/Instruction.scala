package alife

import alife.util.PseudoStack

import java.util.concurrent.ThreadLocalRandom

/**
 * A trait for all Cartesian Genetic Programming instructions used in this system.
 * Every instruction returns a `Double` value
 * by (optionally) querying the field and the results of preceding instructions.
 */
sealed trait Instruction:
  /**
   * Computes the value of the instruction using the information provided.
   * The `data` argument deserves an explanation. `data(i)` refers to the result of computation of the `i`-th instruction
   * before the current one. That is, `data(1)` returns the result of the previous instruction executed,
   * `data(2)` the one preceding that, and so on. Illegal values result in 0 being returned,
   * which also includes `data(0)` being currently computed.
   *
   * @param field the field to query.
   * @param x the X coordinate of the current individual.
   * @param y the Y coordinate of the current individual.
   * @param data the accessor to the previous computation results.
   * @return the computed value.
   */
  def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double

/**
 * All the available instructions.
 */
object Instruction:
  /**
   * Always returns a constant value.
   * @param value the constant value.
   */
  case class Const(value: Double) extends Instruction:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = value
  
  /**
   * Returns the weight of the current individual.
   */
  case object MyWeight extends Instruction:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getCell(x, y).weight
  
  /**
   * Returns the health of the current individual.
   */
  case object MyHealth extends Instruction:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getCell(x, y).health
  
  /**
   * Returns the food amount at a given relative location to the current individual.
   * @param relativeLocation the relative location.
   */
  case class FoodAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getRelativeCell(x, y, relativeLocation).food
  
  /**
   * Returns the amount of debris at a given relative location to the current individual.
   * @param relativeLocation the relative location.
   */
  case class DebrisAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getRelativeCell(x, y, relativeLocation).debris
  
  /**
   * Returns the health of an individual at a given relative location to the current individual, 0 if none.
   * @param relativeLocation the relative location.
   */
  case class HealthAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getRelativeCell(x, y, relativeLocation).health
  
  /**
   * Returns the sine of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  case class Sin(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      math.sin(data(arg))
  
  /**
   * Returns the cosine of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  case class Cos(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      math.cos(data(arg))
  
  /**
   * Returns the exponent of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  case class Exp(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      math.exp(data(arg))
  
  /**
   * Returns the logarithm of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  case class Log(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      math.log(data(arg))
  
  /**
   * Returns the sum of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  case class Plus(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      data(arg1) + data(arg2)
  
  /**
   * Returns the difference of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  case class Minus(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      data(arg1) - data(arg2)
  
  /**
   * Returns the product of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  case class Times(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      data(arg1) * data(arg2)
  
  /**
   * Returns the ratio of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  case class Divide(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      data(arg1) / data(arg2)
  
  /**
   * Returns the soft comparison of values of two other instructions:
   * 1) if the arguments are equal, 0 is returned;
   * 2) if the first argument is smaller than the second one, a positive value is returned: the bigger the difference, the closer the value to +1;
   * 3) if the first argument is larger than the first one, a negative value is returned: the bigger the difference, the closer the value to -1.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  case class Sigmoid(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      2 / (1 + math.exp(data(arg1) - data(arg2))) - 1
  
  /**
   * Generates a new random instruction for the given position in the individual.
   * @param position the 0-based position of the instruction being generated.
   * @return the random instruction.
   */
  def random(position: Int): Instruction =
    val rng = ThreadLocalRandom.current()

    def nextPos() = rng.nextInt(position + 1)
    def nextLoc() = rng.nextInt(Field.numberOfRelativeLocations)

    rng.nextInt(15) match
      case 0 => Const(rng.nextDouble())
      case 1 => MyWeight
      case 2 => MyHealth
      case 3 => FoodAt(nextLoc())
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
  
  /**
   * Maps all argument indices in the supplied instruction using the provided mapping function.
   * @param mapper the mapping function to change argument indices.
   * @param instruction the instruction to modify.
   * @return the modified instruction.
   */
  def mapArguments(mapper: Int => Int)(instruction: Instruction): Instruction = instruction match
    case i: Const => i
    case MyWeight => MyWeight
    case MyHealth => MyHealth
    case i: FoodAt => i
    case i: DebrisAt => i
    case i: HealthAt => i
    case Sin(a) => Sin(mapper(a))
    case Cos(a) => Cos(mapper(a))
    case Exp(a) => Exp(mapper(a))
    case Log(a) => Log(mapper(a))
    case Plus(a1, a2) => Plus(mapper(a1), mapper(a2))
    case Minus(a1, a2) => Minus(mapper(a1), mapper(a2))
    case Times(a1, a2) => Times(mapper(a1), mapper(a2))
    case Divide(a1, a2) => Divide(mapper(a1), mapper(a2))
    case Sigmoid(a1, a2) => Sigmoid(mapper(a1), mapper(a2))
    